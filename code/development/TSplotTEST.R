## SET WORKING DIR & PACKAGES
library(ggpattern)
library(here)
library(MARSS)
library(panelr)
library(readxl)
library(tidyverse)

here::i_am("code/development/TSplotTEST.R")
# what do I mean by TS?
options(max.print=2000)

# pull in data - model objects
ssm_chin <- readRDS(file=here::here("data", "clean", "ssm_chinM9.rds"))

# pull in data - observed time series & methods
load(file=here::here("data", "clean", "nosa_chinPOP.Rda"))
load(file=here::here("data", "clean", "nosa_chin_methods.Rda"))

# pull in data - state key
key_chin <- read_excel(here("data", "clean", "xtT_statekey.xlsx"), sheet = "chin")

# pull in data - pop list
load(file=here::here("data", "clean", "populations_list.Rda"))
pop_list <- pop_list |> 
  filter(CommonPopName !="Lostine River Spring Chinook")
pop_list$COMMONPOPNAME[pop_list$ESAPOPNAME == "Salmon, Chinook (Upper Willamette River ESU) Clackamas River - spring"] <- "Clackamas River - Spring"
pop_list$COMMONPOPNAME[pop_list$ESAPOPNAME == "Salmon, Chinook (Lower Columbia River ESU) Clackamas River - fall"] <- "Clackamas River - Fall"
pop_list$COMMONPOPNAME[pop_list$ESAPOPNAME == "Salmon, Chinook (Lower Columbia River ESU) Sandy River - late fall"] <- "Sandy River - Fall"
pop_list$COMMONPOPNAME[pop_list$ESAPOPNAME == "Salmon, Chinook (Lower Columbia River ESU) Sandy River - spring"] <- "Sandy River - Spring"
pop_names <- pop_list[-c(1, 3, 4, 6:10)]

# pull fitted states from MARSS objects
states_chin <- tsSmooth(ssm_chin, type = "xtT")
param_estimates <- tidy(ssm_chin)

# must match state key to state names in states_X
names(states_chin)[names(states_chin) == ".rownames"] <- "state"
states_chin$state <- as.numeric(sub("^X", "", states_chin$state))
states_chin <- states_chin %>%
  left_join(key_chin, by = c("state" = "State")) %>% 
  select(-state)
states_chin <- states_chin %>%
  left_join(pop_names, by = "PopID")

# must match state key to state names in states_X
states_chin$Year <- states_chin$t + 1979
states_chin$lnnosa <- states_chin$.estimate
states_chin$SE <- states_chin$.se
states_chin <- states_chin %>%
  left_join(nosa_chin_methods, by = c("PopID", "Year"))
states_chin <- states_chin[-c(1:4)]
states_chin$Dataset <- "fitted"

# do that thing where you drop fitted estimates before/after surveys
# first beat nosa_chinPOP into shape
names(nosa_chinPOP)[2:46] <- paste0("t", 1980:2024)
longnosa_chin <- nosa_chinPOP %>%
  pivot_longer(
    cols = starts_with("t"), 
    names_to = "Year", 
    values_to = "Nosa_Value"
  ) %>%
  mutate(Year = as.numeric(str_remove(Year, "t"))) # Convert "t1980" to 1980
longnosa_chin$PopID <- as.numeric(as.character(longnosa_chin$PopID))
longnosa_chin <- longnosa_chin %>%
  left_join(pop_names, by = "PopID")
longnosa_chin  <- longnosa_chin  %>%
  left_join(nosa_chin_methods, by = c("PopID", "Year"))
longnosa_chin$Dataset <- "observed"
longnosa_chin$SE <- NA
longnosa_chin$lnnosa <- longnosa_chin$Nosa_Value
longnosa_chin <- longnosa_chin[-c(1,3)]

nosa_chin <- rbind(states_chin, longnosa_chin)

# fuck with estimates
Aestimates <- subset(param_estimates, grepl("^A", term))
Aestimates$term_numeric <- as.numeric(gsub("\\D", "", Aestimates$term))
Aestimates$MethodNameID <- Aestimates$term_numeric
Aestimates$A <- Aestimates$estimate
Aest <- Aestimates[-c(1:6)]

Restimates <- subset(param_estimates, grepl("^R", term))
Restimates$term_numeric <- as.numeric(gsub("\\D", "", Restimates$term))
Restimates$MethodNameID <- Restimates$term_numeric
Restimates$R <- Restimates$estimate
Rest <- Restimates[-c(1:6)]

# drop estiamtes from before any observations were made
nosa_chin <- nosa_chin %>%
  group_by(COMMONPOPNAME) %>%
  filter(
    trimws(Dataset) == "observed" | 
      (trimws(Dataset) == "fitted" & Year >= min(Year[trimws(Dataset) == "observed" & !is.na(lnnosa)], na.rm = TRUE) &
         Year <= max(Year[trimws(Dataset) == "observed" & !is.na(lnnosa)], na.rm = TRUE))) %>%
  ungroup()
nosa_chin <- nosa_chin[nosa_chin$Dataset != "observed", ]

# plot em up! - chinook
nosa_chin_plotted <- nosa_chin %>%
  mutate(
    lower = if_else(Dataset == "fitted", lnnosa - (1.96 * SE), NA_real_),
    upper = if_else(Dataset == "fitted", lnnosa + (1.96 * SE), NA_real_)
  )

# merge in A (and R even though I'm not using it)
nosa_chin_plotted  <- nosa_chin_plotted %>%
  left_join(Aest, by = "MethodNameID")
nosa_chin_plotted$A[nosa_chin_plotted$MethodNameID == 9] <- 0
  # accounts for reference method
nosa_chin_plotted  <- nosa_chin_plotted %>%
  left_join(Rest, by = "MethodNameID")

ggplot(data = nosa_chin_plotted, aes(x = Year, y = lnnosa, color = Dataset)) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper, fill = Dataset), 
    alpha = 0.2, 
    color = NA
  ) +
  geom_line(linewidth = 1) +
  facet_wrap(~ COMMONPOPNAME, scales = "free_y") +
  theme_minimal() +
  scale_color_manual(values = c("fitted" = "#1f77b4")) +
  scale_fill_manual(values = c("fitted" = "#1f77b4")) +
  labs(
    title = "Chinook - Fitted Values by ESU",
    x = "Year",
    y = "ln(NOSA)"
  ) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "none"
  )
