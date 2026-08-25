## SET WORKING DIR & PACKAGES
library(ggpattern)
library(here)
library(MARSS)
library(panelr)
library(readxl)
library(tidyverse)

here::i_am("code/primary/09-TScomparison.R")
# what do I mean by TS?
options(max.print=2000)

# pull in data - model objects
ssm_chin <- readRDS(file=here::here("data", "clean", "ssm_chinM9.rds"))
ssm_coho <- readRDS(file=here::here("data", "clean", "ssm_cohoM9.rds"))
ssm_stel <- readRDS(file=here::here("data", "clean", "ssm_stelM9.rds"))

# pull in data - observed time series
load(file=here::here("data", "clean", "nosa_chinPOP.Rda"))
load(file=here::here("data", "clean", "nosa_cohoPOP.Rda"))
load(file=here::here("data", "clean", "nosa_stelPOP.Rda"))

# pull in data - state key
key_chin <- read_excel(here("data", "clean", "xtT_statekey.xlsx"), sheet = "chin")
key_coho <- read_excel(here("data", "clean", "xtT_statekey.xlsx"), sheet = "coho")
key_stel <- read_excel(here("data", "clean", "xtT_statekey.xlsx"), sheet = "stel")

# pull in data - pop list
load(file=here::here("data", "clean", "populations_list.Rda"))
pop_list <- pop_list |> 
  filter(CommonPopName !="Lostine River Spring Chinook")
pop_list$COMMONPOPNAME[pop_list$ESAPOPNAME == "Salmon, Chinook (Upper Willamette River ESU) Clackamas River - spring"] <- "Clackamas River - Spring"
pop_list$COMMONPOPNAME[pop_list$ESAPOPNAME == "Salmon, Chinook (Lower Columbia River ESU) Clackamas River - fall"] <- "Clackamas River - Fall"
pop_list$COMMONPOPNAME[pop_list$ESAPOPNAME == "Salmon, Chinook (Lower Columbia River ESU) Sandy River - late fall"] <- "Sandy River - Fall"
pop_list$COMMONPOPNAME[pop_list$ESAPOPNAME == "Salmon, Chinook (Lower Columbia River ESU) Sandy River - spring"] <- "Sandy River - Spring"
pop_names <- pop_list[-c(1, 3, 4, 6:10)]

# check stuff out
summary(ssm_chin)
summary(ssm_coho)
summary(ssm_stel)

# autoplot(ssm_chin, plot.type = "fitted.xtT")
# autoplot(ssm_coho, plot.type = "fitted.xtT")
# autoplot(ssm_stel, plot.type = "fitted.xtT")

# pull fitted states from MARSS objects
states_chin <- tsSmooth(ssm_chin, type = "xtT")
states_coho <- tsSmooth(ssm_coho, type = "xtT")
states_stel <- tsSmooth(ssm_stel, type = "xtT")

# must match state key to state names in states_X
names(states_chin)[names(states_chin) == ".rownames"] <- "state"
states_chin$state <- as.numeric(sub("^X", "", states_chin$state))
states_chin <- states_chin %>%
  left_join(key_chin, by = c("state" = "State")) %>% 
  select(-state)
states_chin$Year <- states_chin$t + 1979
states_chin <- rename(states_chin, Value = .estimate, SE = .se)
states_chin <- states_chin[-c(1)]
states_chin$Dataset <- "fitted"

names(states_coho)[names(states_coho) == ".rownames"] <- "state"
states_coho$state <- as.numeric(sub("^X", "", states_coho$state))
states_coho <- states_coho %>%
  left_join(key_coho, by = c("state" = "State")) %>%
  select(-state)
states_coho$Year <- states_coho$t + 1979
states_coho <- rename(states_coho, Value = .estimate, SE = .se)
states_coho <- states_coho[-c(1)]
states_coho$Dataset <- "fitted"

names(states_stel)[names(states_stel) == ".rownames"] <- "state"
states_stel$state <- as.numeric(sub("^X", "", states_stel$state))
states_stel <- states_stel %>%
  left_join(key_stel, by = c("state" = "State")) %>%
  select(-state)
states_stel$Year <- states_stel$t + 1979
states_stel <- rename(states_stel, Value = .estimate, SE = .se)
states_stel <- states_stel[-c(1)]
states_stel$Dataset <- "fitted"

# create long format nosa data to plot
names(nosa_chinPOP)[2:46] <- paste0("t", 1980:2024)
longnosa_chin <- nosa_chinPOP %>%
  pivot_longer(
    cols = starts_with("t"), 
    names_to = "Year", 
    values_to = "Value"
  ) %>%
  mutate(Year = as.numeric(str_remove(Year, "t"))) # Convert "t1980" to 1980
longnosa_chin$SE <- NA
longnosa_chin$Dataset <- "observed"

names(nosa_cohoPOP)[2:46] <- paste0("t", 1980:2024)
longnosa_coho <- nosa_cohoPOP %>%
  pivot_longer(
    cols = starts_with("t"), 
    names_to = "Year", 
    values_to = "Value"
  ) %>%
  mutate(Year = as.numeric(str_remove(Year, "t"))) # Convert "t1980" to 1980
longnosa_coho$SE <- NA
longnosa_coho$Dataset <- "observed"

names(nosa_stelPOP)[2:46] <- paste0("t", 1980:2024)
longnosa_stel <- nosa_stelPOP %>%
  pivot_longer(
    cols = starts_with("t"), 
    names_to = "Year", 
    values_to = "Value"
  ) %>%
  mutate(Year = as.numeric(str_remove(Year, "t"))) # Convert "t1980" to 1980
longnosa_stel$SE <- NA
longnosa_stel$Dataset <- "observed"

# join
## chinook
combineddata_chin <- rbind(states_chin, longnosa_chin)

plotdata_chin <- combineddata_chin
plotdata_chin$upper95 <- plotdata_chin$Value + (plotdata_chin$SE * 1.96)
plotdata_chin$lower95 <- plotdata_chin$Value - (plotdata_chin$SE * 1.96)

# drop estimates from before any observations were made
plotdata_chin <- plotdata_chin %>%
  group_by(PopID) %>%
  filter(
    trimws(Dataset) == "observed" | 
      (trimws(Dataset) == "fitted" & Year >= min(Year[trimws(Dataset) == "observed" & !is.na(Value)], na.rm = TRUE) &
         Year <= max(Year[trimws(Dataset) == "observed" & !is.na(Value)], na.rm = TRUE))) %>%
  ungroup()

# pull in pop names
plotdata_chin <- plotdata_chin %>%
  mutate(PopID = as.character(PopID)) %>%
  left_join(
    pop_names %>% mutate(PopID = as.character(PopID)),
    by = "PopID"
  ) 
  ## SOMETHING WEIRD WITH PopID = 239 - look into it

# de-duping
duplicated(plotdata_chin)
plotdata_chin <- unique(plotdata_chin)
duplicated(plotdata_chin)
  # this seems to address the issue but I'm still not sure where the dupes came from

## coho
combineddata_coho <- rbind(states_coho, longnosa_coho)

plotdata_coho <- combineddata_coho
plotdata_coho$upper95 <- plotdata_coho$Value + (plotdata_coho$SE * 1.96)
plotdata_coho$lower95 <- plotdata_coho$Value - (plotdata_coho$SE * 1.96)

# drop estimates from before any observations were made
plotdata_coho <- plotdata_coho %>%
  group_by(PopID) %>%
  filter(
    trimws(Dataset) == "observed" | 
      (trimws(Dataset) == "fitted" & Year >= min(Year[trimws(Dataset) == "observed" & !is.na(Value)], na.rm = TRUE) &
         Year <= max(Year[trimws(Dataset) == "observed" & !is.na(Value)], na.rm = TRUE))) %>%
  ungroup()

# pull in pop names
plotdata_coho <- plotdata_coho %>%
  mutate(PopID = as.character(PopID)) %>%
  left_join(
    pop_names %>% mutate(PopID = as.character(PopID)),
    by = "PopID"
  ) 

# de-duping - just in case
duplicated(plotdata_coho)
plotdata_coho <- unique(plotdata_coho)
duplicated(plotdata_coho)

# steelies
combineddata_stel <- rbind(states_stel, longnosa_stel)

plotdata_stel <- combineddata_stel
plotdata_stel$upper95 <- plotdata_stel$Value + (plotdata_stel$SE * 1.96)
plotdata_stel$lower95 <- plotdata_stel$Value - (plotdata_stel$SE * 1.96)

# drop estimates from before any observations were made
plotdata_stel <- plotdata_stel %>%
  group_by(PopID) %>%
  filter(
    trimws(Dataset) == "observed" | 
      (trimws(Dataset) == "fitted" & Year >= min(Year[trimws(Dataset) == "observed" & !is.na(Value)], na.rm = TRUE) &
         Year <= max(Year[trimws(Dataset) == "observed" & !is.na(Value)], na.rm = TRUE))) %>%
  ungroup()

# pull in pop names
plotdata_stel <- plotdata_stel %>%
  mutate(PopID = as.character(PopID)) %>%
  left_join(
    pop_names %>% mutate(PopID = as.character(PopID)),
    by = "PopID"
  )

# de-duping - just in case
duplicated(plotdata_stel)
  # pop 503
plotdata_stel <- unique(plotdata_stel)
duplicated(plotdata_stel)
  # some duplicates here too? how strange?

# plot em up
statecompare_chin <- ggplot(plotdata_chin, aes(x = Year, y = Value, color = Dataset, fill = Dataset)) +
  geom_ribbon(aes(ymin = lower95, ymax = upper95), alpha = 0.2, color = NA, show.legend = FALSE) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ COMMONPOPNAME, scales = "free_y") + # 'free_y' adjusts vertical scales for each population
  theme_minimal() +
  scale_color_manual(
    values = c("fitted" = "#D55E00", "observed" = "#0072B2"), # Replace with your preferred colors
    labels = c("fitted" = "State Estimate", "observed" = "Observation")
  ) +
  labs(
    title = "Chinook population time series comparison by ESU (1980-2024)",
    x = "",
    y = "ln(NOSA)",
    color = ""
  ) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    panel.grid = element_blank()
  )
statecompare_chin

statecompare_coho <- ggplot(plotdata_coho, aes(x = Year, y = Value, color = Dataset, fill = Dataset)) +
  geom_ribbon(aes(ymin = lower95, ymax = upper95), alpha = 0.2, color = NA, show.legend = FALSE) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ COMMONPOPNAME, scales = "free_y") + # 'free_y' adjusts vertical scales for each population
  theme_minimal() +
  scale_color_manual(
    values = c("fitted" = "#D55E00", "observed" = "#0072B2"), # Replace with your preferred colors
    labels = c("fitted" = "State Estimate", "observed" = "Observation")
  ) +
  labs(
    title = "Coho population time series comparison by ESU (1980-2024)",
    x = "",
    y = "ln(NOSA)",
    color = ""
  ) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    panel.grid = element_blank()
  )
statecompare_coho

# statecompare_stel <- ggplot(plotdata_stel, aes(x = Year, y = Value, color = Dataset)) +
#   geom_line(linewidth = 0.8) +
#   facet_wrap(~ COMMONPOPNAME, scales = "free_y") + # 'free_y' adjusts vertical scales for each population
#   theme_minimal() +
#   labs(
#     title = "Steelhead Population Time Series Comparison (1980-2024)",
#     x = "",
#     y = "ln(NOSA)",
#     color = ""
#   ) +
#   theme(
#     legend.position = "bottom",
#     strip.text = element_text(face = "bold") # Makes PopID headers bold
#   ) +  
#   theme(
#     panel.grid = element_blank()
#   )
# statecompare_stel
# youngs bay is kind of ridiculous here (only 2 obs) - will drop from plot

plotdata_stel2 <- plotdata_stel[plotdata_stel$COMMONPOPNAME != "Youngs Bay", ]
statecompare_stel <- ggplot(plotdata_stel2, aes(x = Year, y = Value, color = Dataset, fill = Dataset)) +
  geom_ribbon(aes(ymin = lower95, ymax = upper95), alpha = 0.2, color = NA, show.legend = FALSE) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ COMMONPOPNAME, scales = "free_y") + # 'free_y' adjusts vertical scales for each population
  theme_minimal() +
  scale_color_manual(
    values = c("fitted" = "#D55E00", "observed" = "#0072B2"), # Replace with your preferred colors
    labels = c("fitted" = "State Estimate", "observed" = "Observation")
  ) +
  labs(
    title = "Steelhead population time series comparison by ESU (1980-2024)",
    x = "",
    y = "ln(NOSA)",
    color = ""
  ) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    panel.grid = element_blank()
  )
statecompare_stel

# total difference between observed and estimated by population throughout study period
# chinook
diffdata_chin <- plotdata_chin[-c(2, 6, 7)]
diffdata_chin$expVal <- exp(diffdata_chin$Value)
popdiff_chin <- diffdata_chin %>%
  pivot_wider(
    id_cols = c(PopID, Year, COMMONPOPNAME),
    names_from = Dataset,
    values_from = expVal
  ) |> 
  rename(Nosa = `observed`, States = `fitted`)  %>%
  mutate(
    yearly_diff = Nosa - States,          # estimates are over/under-counting
    abs_yearly_diff = abs(Nosa - States)  # magnitude of error, ignoring direction
  ) %>%
  # group by population to calculate the total totals
  group_by(PopID) %>%
  summarize(
    total_net_difference = sum(yearly_diff, na.rm = TRUE),
    total_absolute_difference = sum(abs_yearly_diff, na.rm = TRUE),
    years_compared = sum(!is.na(yearly_diff)) # how many years actually had data for both
  )
popdiff_chin$avg_netdiff <- popdiff_chin$total_net_difference/popdiff_chin$years_compared
print(popdiff_chin)

# coho
diffdata_coho <- plotdata_coho[-c(2, 6, 7)]
diffdata_coho$expVal <- exp(diffdata_coho$Value)
popdiff_coho <- diffdata_coho %>%
  pivot_wider(
    id_cols = c(PopID, Year, COMMONPOPNAME),
    names_from = Dataset,
    values_from = expVal
  ) |> 
  rename(Nosa = `observed`, States = `fitted`)  %>%
  mutate(
    yearly_diff = Nosa - States,          # estimates are over/under-counting
    abs_yearly_diff = abs(Nosa - States)  # magnitude of error, ignoring direction
  ) %>%
  # group by population to calculate the total totals
  group_by(PopID) %>%
  summarize(
    total_net_difference = sum(yearly_diff, na.rm = TRUE),
    total_absolute_difference = sum(abs_yearly_diff, na.rm = TRUE),
    years_compared = sum(!is.na(yearly_diff)) # how many years actually had data for both
  )
popdiff_coho$avg_netdiff <- popdiff_coho$total_net_difference/popdiff_coho$years_compared
print(popdiff_coho)

# steelhead
diffdata_stel <- plotdata_stel[-c(2, 6, 7)]
diffdata_stel$expVal <- exp(diffdata_stel$Value)
popdiff_stel <- diffdata_stel %>%
  pivot_wider(
    id_cols = c(PopID, Year, COMMONPOPNAME),
    names_from = Dataset,
    values_from = expVal
  ) |> 
  rename(Nosa = `observed`, States = `fitted`)  %>%
  mutate(
    yearly_diff = Nosa - States,          # estimates are over/under-counting
    abs_yearly_diff = abs(Nosa - States)  # magnitude of error, ignoring direction
  ) %>%
  # group by population to calculate the total totals
  group_by(PopID) %>%
  summarize(
    total_net_difference = sum(yearly_diff, na.rm = TRUE),
    total_absolute_difference = sum(abs_yearly_diff, na.rm = TRUE),
    years_compared = sum(!is.na(yearly_diff)) # how many years actually had data for both
  )
popdiff_stel$avg_netdiff <- popdiff_stel$total_net_difference/popdiff_stel$years_compared
print(popdiff_stel)

# pull in data - ESU names
esu_list <- pop_list[-c(1, 3, 5:10)]
esu_list$ESAPOPNAME <- sub("\\).*$", ")", esu_list$ESAPOPNAME)

popdiff_chin <- popdiff_chin %>%
  left_join(
    esu_list %>% mutate(PopID = as.character(PopID)),
    by = "PopID"
  ) 

popdiff_coho <- popdiff_coho %>%
  left_join(
    esu_list %>% mutate(PopID = as.character(PopID)),
    by = "PopID"
  ) 

popdiff_stel <- popdiff_stel %>%
  left_join(
    esu_list %>% mutate(PopID = as.character(PopID)),
    by = "PopID"
  ) 

# save difference data
save(popdiff_chin, file=here("data", "clean", "popdiff_chin.Rda"))
save(popdiff_coho, file=here("data", "clean", "popdiff_coho.Rda"))
save(popdiff_stel, file=here("data", "clean", "popdiff_stel.Rda"))
