## SET WORKING DIR & PACKAGES
library(here)
library(MARSS)
library(panelr)
library(readxl)
library(tidyverse)

here::i_am("code/primary/09-TScomparison.R")
options(max.print=2000)

# pull in data - model objects
# ssm_chin <- readRDS(file=here::here("data", "clean", "ssm_chinM16.rds"))
# ssm_coho <- readRDS(file=here::here("data", "clean", "ssm_cohoM10.rds"))
# ssm_stel <- readRDS(file=here::here("data", "clean", "ssm_stelM22.rds"))
ssm_chin <- readRDS(file=here::here("data", "clean", "ssm_chinM9.rds"))
ssm_coho <- readRDS(file=here::here("data", "clean", "ssm_cohoM9.rds"))
ssm_stel <- readRDS(file=here::here("data", "clean", "ssm_stelM9.rds"))

# pull in data - observed time series
nosa_chinPOP <- readRDS(file=here::here("data", "clean", "nosa_chinPOP.rds"))
nosa_cohoPOP <- readRDS(file=here::here("data", "clean", "nosa_cohoPOP.rds"))
nosa_stelPOP <- readRDS(file=here::here("data", "clean", "nosa_stelPOP.rds"))

# pull in data - state key
key_chin <- read_excel(here("data", "clean", "xtT_statekey.xlsx"), sheet = "chin")
# key_coho <- read_excel(here("data", "clean", "xtT_statekey.xlsx"), sheet = "coho")
# key_stel <- read_excel(here("data", "clean", "xtT_statekey.xlsx"), sheet = "stel")

# check stuff out
summary(ssm_chin)
summary(ssm_coho)
summary(ssm_stel)

autoplot(ssm_chin, plot.type = "fitted.xtT")
autoplot(ssm_coho, plot.type = "fitted.xtT")
autoplot(ssm_stel, plot.type = "fitted.xtT")

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

# names(states_coho)[names(states_coho) == ".rownames"] <- "state"
# states_coho$state <- as.numeric(sub("^X", "", states_coho$state))
# states_coho <- states_coho %>%
#   left_join(key_coho, by = c("state" = "State")) %>% 
#   select(-state)
# 
# names(states_stel)[names(states_stel) == ".rownames"] <- "state"
# states_stel$state <- as.numeric(sub("^X", "", states_stel$state))
# states_stel <- states_stel %>%
#   left_join(key_stel, by = c("state" = "State")) %>% 
#   select(-state)

# set data wide (rows = popid/method, columns = year)
states_chinW <- states_chin[-c(3)]
states_chinW <- panel_data(states_chinW, id = PopID, wave = t)
states_chinW <- widen_panel(states_chinW, separator = "_")
names(states_chinW)[2:46] <- paste0("t", 1980:2024)
names(nosa_chinPOP)[2:46] <- paste0("t", 1980:2024)

# states_cohoW <- states_coho[-c(3)]
# states_cohoW <- panel_data(states_cohoW, id = PopID, wave = t)
# states_cohoW <- widen_panel(states_cohoW, separator = "_")
# names(states_cohoW)[2:46] <- paste0("t", 1980:2024)
# names(nosa_cohoPOP)[2:46] <- paste0("t", 1980:2024)
# 
# states_stelW <- states_stel[-c(3)]
# states_stelW <- panel_data(states_stelW, id = PopID, wave = t)
# states_stelW <- widen_panel(states_stelW, separator = "_")
# names(states_stelW)[2:46] <- paste0("t", 1980:2024)
# names(nosa_stelPOP)[2:46] <- paste0("t", 1980:2024)

# going off AI - let's see...
# 1. Pivot states_chinW to long format
long_states <- states_chinW %>%
  pivot_longer(
    cols = starts_with("t"), 
    names_to = "Year", 
    values_to = "States_Value"
  ) %>%
  mutate(Year = as.numeric(str_remove(Year, "t"))) # Convert "t1980" to 1980

# 2. Pivot nosa_chinPOP to long format
long_nosa <- nosa_chinPOP %>%
  pivot_longer(
    cols = starts_with("t"), 
    names_to = "Year", 
    values_to = "Nosa_Value"
  ) %>%
  mutate(Year = as.numeric(str_remove(Year, "t"))) # Convert "t1980" to 1980

# 3. Join the dataframes together by PopID and Year
combined_data <- left_join(long_states, long_nosa, by = c("PopID", "Year"))

# 4. Final pivot to create a clean "Dataset" label column for the plot legend
plot_data <- combined_data %>%
  pivot_longer(
    cols = c(States_Value, Nosa_Value),
    names_to = "Dataset",
    values_to = "Value"
  ) %>%
  mutate(Dataset = recode(Dataset, 
                          "States_Value" = "States Chin", 
                          "Nosa_Value" = "Nosa Chin"))

# 5. plot em up
ggplot(plot_data, aes(x = Year, y = Value, color = Dataset)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ PopID, scales = "free_y") + # 'free_y' adjusts vertical scales for each population
  theme_minimal() +
  labs(
    title = "Population Time Series Comparison (1980-2024)",
    x = "Year",
    y = "Recorded Value",
    color = "Source Dataset"
  ) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold") # Makes PopID headers bold
  )
    ## SEEMS TO WORK!! I'M GOING TO GO THROUGH THS CODE IN DETAIL TOMORROW