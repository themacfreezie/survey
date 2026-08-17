## SET WORKING DIR & PACKAGES
library(ggpattern)
library(here)
library(MARSS)
library(panelr)
library(readxl)
library(tidyverse)

here::i_am("code/primary/09-TScomparison.R")
options(max.print=2000)

# pull in data - model objects
ssm_chin <- readRDS(file=here::here("data", "clean", "ssm_chinM9.rds"))

# pull in data - observed time series
load(file=here::here("data", "clean", "nosa_chinPOP.Rda"))

# pull in data - state key
key_chin <- read_excel(here("data", "clean", "xtT_statekey.xlsx"), sheet = "chin")

# pull in data - pop list
load(file=here::here("data", "clean", "populations_list.Rda"))

pop_list <- pop_list |> filter(CommonPopName !="Lostine River Spring Chinook")
pop_list$COMMONPOPNAME[pop_list$ESAPOPNAME == "Salmon, Chinook (Upper Willamette River ESU) Clackamas River - spring"] <- "Clackamas River - Spring"
pop_list$COMMONPOPNAME[pop_list$ESAPOPNAME == "Salmon, Chinook (Lower Columbia River ESU) Clackamas River - fall"] <- "Clackamas River - Fall"
pop_list$COMMONPOPNAME[pop_list$ESAPOPNAME == "Salmon, Chinook (Lower Columbia River ESU) Sandy River - late fall"] <- "Sandy River - Fall"
pop_list$COMMONPOPNAME[pop_list$ESAPOPNAME == "Salmon, Chinook (Lower Columbia River ESU) Sandy River - spring"] <- "Sandy River - Spring"

pop_names <- pop_list[-c(1, 3, 4, 6:10)]

# 1. EXTRACT OBSERVATION BIAS (a) TERMS
# Extract 'a' parameter matrix from the MARSS object
a_matrix <- coef(ssm_chin, type = "matrix")$A
# If 'a' is a matrix with row names matching the observation processes, convert to a dataframe
bias_df <- data.frame(
  State = as.numeric(sub("^X", "", rownames(a_matrix))),
  bias_a = as.vector(a_matrix)
)

# pull fitted states from MARSS objects
states_chin <- tsSmooth(ssm_chin, type = "xtT")

# must match state key to state names in states_X
names(states_chin)[names(states_chin) == ".rownames"] <- "state"
states_chin$state <- as.numeric(sub("^X", "", states_chin$state))

# Join state key and extract the observation bias for each state
states_chin <- states_chin %>% 
  left_join(key_chin, by = c("state" = "State")) %>% 
  left_join(bias_df, by = c("state" = "State")) %>%
  select(-state)

states_chin <- states_chin %>% left_join(pop_names, by = "PopID")

states_chin$Year <- states_chin$t + 1979
states_chin$lnnosa <- states_chin$.estimate
states_chin$SE <- states_chin$.se
states_chin <- states_chin[-c(1:4)]

# 2. CREATE RAW AND BIAS-ADJUSTED STATES
# Create the standard raw fitted state dataset
states_raw <- states_chin %>% 
  mutate(Dataset = "fitted_raw") %>%
  select(-bias_a)

# Create the bias-adjusted state dataset (Fitted State + Observation Bias)
states_adjusted <- states_chin %>% 
  mutate(
    lnnosa = lnnosa + bias_a, # Adjusting state estimate by adding observation bias intercept
    Dataset = "fitted_bias_adjusted"
  ) %>%
  select(-bias_a)

# Combine the two types of fitted lines
states_combined <- rbind(states_raw, states_adjusted)

# first beat nosa_chinPOP into shape
names(nosa_chinPOP)[2:46] <- paste0("t", 1980:2024)

longnosa_chin <- nosa_chinPOP %>% 
  pivot_longer(
    cols = starts_with("t"),
    names_to = "Year",
    values_to = "Nosa_Value"
  ) %>% 
  mutate(Year = as.numeric(str_remove(Year, "t")))

longnosa_chin$PopID <- as.numeric(as.character(longnosa_chin$PopID))
longnosa_chin <- longnosa_chin %>% left_join(pop_names, by = "PopID")
longnosa_chin$Dataset = "observed"
longnosa_chin$SE <- NA
longnosa_chin$lnnosa <- longnosa_chin$Nosa_Value
longnosa_chin <- longnosa_chin[-c(1,3)]

# Combine observed data with both fitted state datasets
nosa_chin <- rbind(states_combined, longnosa_chin)

# drop estimates from before any observations were made
nosa_chin <- nosa_chin %>% 
  group_by(COMMONPOPNAME) %>% 
  filter(
    trimws(Dataset) == "observed" | 
      (startsWith(trimws(Dataset), "fitted") & 
         Year >= min(Year[trimws(Dataset) == "observed" & !is.na(lnnosa)], na.rm = TRUE) & 
         Year <= max(Year[trimws(Dataset) == "observed" & !is.na(lnnosa)], na.rm = TRUE))
  ) %>% 
  ungroup()

# Filter out the observed rows since we only want to plot the fitted paths
nosa_chin <- nosa_chin[nosa_chin$Dataset != "observed", ]

# plot em up! - chinook
nosa_chin_plotted <- nosa_chin %>% 
  mutate(
    lower = if_else(startsWith(Dataset, "fitted"), lnnosa - (1.96 * SE), NA_real_),
    upper = if_else(startsWith(Dataset, "fitted"), lnnosa + (1.96 * SE), NA_real_)
  )

# Plotting both fitted lines side by side with separate colors
ggplot(data = nosa_chin_plotted, aes(x = Year, y = lnnosa, color = Dataset)) + 
  geom_ribbon(
    aes(ymin = lower, ymax = upper, fill = Dataset), 
    alpha = 0.15, 
    color = NA
  ) + 
  geom_line(linewidth = 1) + 
  facet_wrap(~ COMMONPOPNAME, scales = "free_y") + 
  theme_minimal() + 
  scale_color_manual(values = c("fitted_raw" = "#1f77b4", "fitted_bias_adjusted" = "#e377c2")) + 
  scale_fill_manual(values = c("fitted_raw" = "#1f77b4", "fitted_bias_adjusted" = "#e377c2")) + 
  labs(
    title = "Chinook - Raw vs. Bias-Adjusted Fitted Values",
    x = "Year",
    y = "ln(NOSA)"
  ) + 
  theme(
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "bottom"
  )
