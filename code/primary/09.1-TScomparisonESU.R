## SET WORKING DIR & PACKAGES
library(ggpattern)
library(here)
library(MARSS)
library(panelr)
library(readxl)
library(tidyverse)

here::i_am("code/primary/09.1-TScomparisonESU.R")
# what do I mean by TS? time seires!
options(max.print=2000)

# pull in data - model objects
ssm_chin <- readRDS(file=here::here("data", "clean", "ssm_ESUchinM9.rds"))
ssm_coho <- readRDS(file=here::here("data", "clean", "ssm_ESUcohoM9.rds"))
ssm_stel <- readRDS(file=here::here("data", "clean", "ssm_ESUstelM9.rds"))

# ESU level abundance data
load(file=here::here("data", "clean", "nosa_chinESU.Rda"))
load(file=here::here("data", "clean", "nosa_cohoESU.Rda"))
load(file=here::here("data", "clean", "nosa_stelESU.Rda"))

# pull in data - state key
key_chin <- read_excel(here("data", "clean", "xtT_statekey.xlsx"), sheet = "chinESU")
key_coho <- read_excel(here("data", "clean", "xtT_statekey.xlsx"), sheet = "cohoESU")
key_stel <- read_excel(here("data", "clean", "xtT_statekey.xlsx"), sheet = "stelESU")

# pull fitted states from MARSS objects
states_chin <- tsSmooth(ssm_chin, type = "xtT")
states_coho <- tsSmooth(ssm_coho, type = "xtT")
states_stel <- tsSmooth(ssm_stel, type = "xtT")

# must match state key to state names in states_X
names(states_chin)[names(states_chin) == ".rownames"] <- "ESANAME"
states_chin$ESANAME <- as.numeric(sub("^X", "", states_chin$ESANAME))
states_chin <- states_chin %>%
  left_join(key_chin, by = c("ESANAME" = "State")) %>% 
  select(-ESANAME)
states_chin$ESANAME <- states_chin$ESANAME.y
states_chin$Year <- states_chin$t + 1979
states_chin$lnnosa <- states_chin$.estimate
states_chin$SE <- states_chin$.se
states_chin <- states_chin[-c(1:4)]
states_chin$Dataset <- "State Estimate"

names(states_coho)[names(states_coho) == ".rownames"] <- "ESANAME"
states_coho$ESANAME <- as.numeric(sub("^X", "", states_coho$ESANAME))
states_coho <- states_coho %>%
  left_join(key_coho, by = c("ESANAME" = "State")) %>% 
  select(-ESANAME)
states_coho$ESANAME <- states_coho$ESANAME.y
states_coho$Year <- states_coho$t + 1979
states_coho$lnnosa <- states_coho$.estimate
states_coho$SE <- states_coho$.se
states_coho <- states_coho[-c(1:4)]
states_coho$Dataset <- "State Estimate"

names(states_stel)[names(states_stel) == ".rownames"] <- "ESANAME"
states_stel$ESANAME <- as.numeric(sub("^X", "", states_stel$ESANAME))
states_stel <- states_stel %>%
  left_join(key_stel, by = c("ESANAME" = "State")) %>% 
  select(-ESANAME)
states_stel$ESANAME <- states_stel$ESANAME.y
states_stel$Year <- states_stel$t + 1979
states_stel$lnnosa <- states_stel$.estimate
states_stel$SE <- states_stel$.se
states_stel <- states_stel[-c(1:4)]
states_stel$Dataset <- "State Estimate"

# structure observed data to drop fitted obs w/ no survey observations
nosa_chinESU$Dataset <- "Observation"
nosa_cohoESU$Dataset <- "Observation"
nosa_stelESU$Dataset <- "Observation"

nosa_chinESU$SE <- NA
nosa_cohoESU$SE <- NA
nosa_stelESU$SE <- NA

# append dateframes
nosa_chin <- rbind(states_chin, nosa_chinESU)
nosa_coho <- rbind(states_coho, nosa_cohoESU)
nosa_stel <- rbind(states_stel, nosa_stelESU)

# drop estiamtes from before any observations were made
nosa_chin <- nosa_chin %>%
  group_by(ESANAME) %>%
  filter(
    trimws(Dataset) == "Observation" | 
      (trimws(Dataset) == "State Estimate" & Year >= min(Year[trimws(Dataset) == "Observation" & !is.na(lnnosa)], na.rm = TRUE) &
         Year <= max(Year[trimws(Dataset) == "Observation" & !is.na(lnnosa)], na.rm = TRUE))) %>%
  ungroup()
nosa_chin <- nosa_chin[nosa_chin$Dataset != "Observation", ]

nosa_coho <- nosa_coho %>%
  group_by(ESANAME) %>%
  filter(
    trimws(Dataset) == "Observation" | 
      (trimws(Dataset) == "State Estimate" & Year >= min(Year[trimws(Dataset) == "Observation" & !is.na(lnnosa)], na.rm = TRUE) &
         Year <= max(Year[trimws(Dataset) == "Observation" & !is.na(lnnosa)], na.rm = TRUE))) %>%
  ungroup()
nosa_coho <- nosa_coho[nosa_coho$Dataset != "Observation", ]

nosa_stel <- nosa_stel %>%
  group_by(ESANAME) %>%
  filter(
    trimws(Dataset) == "Observation" | 
      (trimws(Dataset) == "State Estimate" & Year >= min(Year[trimws(Dataset) == "Observation" & !is.na(lnnosa)], na.rm = TRUE) &
         Year <= max(Year[trimws(Dataset) == "Observation" & !is.na(lnnosa)], na.rm = TRUE))) %>%
  ungroup()
nosa_stel <- nosa_stel[nosa_stel$Dataset != "Observation", ]

# plot em up! - chinook
nosa_chin_plotted <- nosa_chin %>%
  mutate(
    lower = if_else(Dataset == "State Estimate", lnnosa - (1.96 * SE), NA_real_),
    upper = if_else(Dataset == "State Estimate", lnnosa + (1.96 * SE), NA_real_)
  )

ggplot(data = nosa_chin_plotted, aes(x = Year, y = lnnosa, color = Dataset)) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper, fill = Dataset), 
    alpha = 0.2, 
    color = NA
  ) +
  geom_line(linewidth = 1) +
  # geom_point(size = 1.5) +
  facet_wrap(~ ESANAME, scales = "free_y") +
  theme_minimal() +
  scale_color_manual(values = c("State Estimate" = "#1f77b4")) +
  scale_fill_manual(values = c("State Estimate" = "#1f77b4")) +
  labs(
    title = "Chinook - State Estimates by ESU",
    x = "",
    y = "log(NOSA)"
  ) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "none"
  ) + 
  theme(
    panel.grid = element_blank()
  )
  # these aren't comparable because the fitted ESU is identifying an underlying state but not aggregating 
  # all sampled populations
    ## tweaked

# plot em up! - coho
nosa_coho_plotted <- nosa_coho %>%
  mutate(
    lower = if_else(Dataset == "State Estimate", lnnosa - (1.96 * SE), NA_real_),
    upper = if_else(Dataset == "State Estimate", lnnosa + (1.96 * SE), NA_real_)
  )

ggplot(data = nosa_coho_plotted, aes(x = Year, y = lnnosa, color = Dataset)) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper, fill = Dataset), 
    alpha = 0.2, 
    color = NA
  ) +
  geom_line(linewidth = 1) +
  facet_wrap(~ ESANAME, scales = "free_y") +
  theme_minimal() +
  scale_color_manual(values = c("State Estimate" = "#1f77b4")) +
  scale_fill_manual(values = c("State Estimate" = "#1f77b4")) +
  labs(
    title = "Coho - State Estimates by ESU",
    x = "",
    y = "log(NOSA)"
  ) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "none"
  ) + 
  theme(
    panel.grid = element_blank()
  )

# plot em up! - steelies
nosa_stel_plotted <- nosa_stel %>%
  mutate(
    lower = if_else(Dataset == "State Estimate", lnnosa - (1.96 * SE), NA_real_),
    upper = if_else(Dataset == "State Estimate", lnnosa + (1.96 * SE), NA_real_)
  )

ggplot(data = nosa_stel_plotted, aes(x = Year, y = lnnosa, color = Dataset)) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper, fill = Dataset), 
    alpha = 0.2, 
    color = NA
  ) +
  geom_line(linewidth = 1) +
  facet_wrap(~ ESANAME, scales = "free_y") +
  theme_minimal() +
  scale_color_manual(values = c("State Estimate" = "#1f77b4")) +
  scale_fill_manual(values = c("State Estimate" = "#1f77b4")) +
  labs(
    title = "Steelhead - State Estimates by DPS",
    x = "",
    y = "log(NOSA)"
  ) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "none"
  ) + 
  theme(
    panel.grid = element_blank()
  )


# let's try using aggregated data from pop level modeling..
# pull in data - model objects
ssm_chin <- readRDS(file=here::here("data", "clean", "ssm_chinM9.rds"))
ssm_coho <- readRDS(file=here::here("data", "clean", "ssm_cohoM9.rds"))
ssm_stel <- readRDS(file=here::here("data", "clean", "ssm_stelM9.rds"))

# pull in data - pop list
load(file=here::here("data", "clean", "populations_list.Rda"))
pop_list <- pop_list |> 
  filter(CommonPopName !="Lostine River Spring Chinook")
pop_list$COMMONPOPNAME[pop_list$ESAPOPNAME == "Salmon, Chinook (Upper Willamette River ESU) Clackamas River - spring"] <- "Clackamas River - Spring"
pop_list$COMMONPOPNAME[pop_list$ESAPOPNAME == "Salmon, Chinook (Lower Columbia River ESU) Clackamas River - fall"] <- "Clackamas River - Fall"
pop_list$COMMONPOPNAME[pop_list$ESAPOPNAME == "Salmon, Chinook (Lower Columbia River ESU) Sandy River - late fall"] <- "Sandy River - Fall"
pop_list$COMMONPOPNAME[pop_list$ESAPOPNAME == "Salmon, Chinook (Lower Columbia River ESU) Sandy River - spring"] <- "Sandy River - Spring"

esu_list <- pop_list[-c(1, 3, 5:10)]
esu_list$ESAPOPNAME <- sub("\\).*$", ")", esu_list$ESAPOPNAME)

# pull in data - state key for populations
key_chin <- read_excel(here("data", "clean", "xtT_statekey.xlsx"), sheet = "chin")
key_coho <- read_excel(here("data", "clean", "xtT_statekey.xlsx"), sheet = "coho")
key_stel <- read_excel(here("data", "clean", "xtT_statekey.xlsx"), sheet = "stel")

# pull in data - observed time series
load(file=here::here("data", "clean", "nosa_chinPOP.Rda"))
load(file=here::here("data", "clean", "nosa_cohoPOP.Rda"))
load(file=here::here("data", "clean", "nosa_stelPOP.Rda"))

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

names(states_coho)[names(states_coho) == ".rownames"] <- "state"
states_coho$state <- as.numeric(sub("^X", "", states_coho$state))
states_coho <- states_coho %>%
  left_join(key_coho, by = c("state" = "State")) %>%
  select(-state)

names(states_stel)[names(states_stel) == ".rownames"] <- "state"
states_stel$state <- as.numeric(sub("^X", "", states_stel$state))
states_stel <- states_stel %>%
  left_join(key_stel, by = c("state" = "State")) %>%
  select(-state)

# set data wide (rows = popid, columns = year)
states_chinW <- states_chin[-c(3)]
states_chinW <- panel_data(states_chinW, id = PopID, wave = t)
states_chinW <- widen_panel(states_chinW, separator = "_")
names(states_chinW)[2:46] <- paste0("t", 1980:2024)
names(nosa_chinPOP)[2:46] <- paste0("t", 1980:2024)

states_cohoW <- states_coho[-c(3)]
states_cohoW <- panel_data(states_cohoW, id = PopID, wave = t)
states_cohoW <- widen_panel(states_cohoW, separator = "_")
names(states_cohoW)[2:46] <- paste0("t", 1980:2024)
names(nosa_cohoPOP)[2:46] <- paste0("t", 1980:2024)

states_stelW <- states_stel[-c(3)]
states_stelW <- panel_data(states_stelW, id = PopID, wave = t)
states_stelW <- widen_panel(states_stelW, separator = "_")
names(states_stelW)[2:46] <- paste0("t", 1980:2024)
names(nosa_stelPOP)[2:46] <- paste0("t", 1980:2024)

# create long format data to plot
longstates_chin <- states_chinW %>%
  pivot_longer(
    cols = starts_with("t"), 
    names_to = "Year", 
    values_to = "States_Value"
  ) %>%
  mutate(Year = as.numeric(str_remove(Year, "t"))) # Convert "t1980" to 1980

longnosa_chin <- nosa_chinPOP %>%
  pivot_longer(
    cols = starts_with("t"), 
    names_to = "Year", 
    values_to = "Nosa_Value"
  ) %>%
  mutate(Year = as.numeric(str_remove(Year, "t"))) # Convert "t1980" to 1980

longstates_coho <- states_cohoW %>%
  pivot_longer(
    cols = starts_with("t"), 
    names_to = "Year", 
    values_to = "States_Value"
  ) %>%
  mutate(Year = as.numeric(str_remove(Year, "t"))) # Convert "t1980" to 1980

longnosa_coho <- nosa_cohoPOP %>%
  pivot_longer(
    cols = starts_with("t"), 
    names_to = "Year", 
    values_to = "Nosa_Value"
  ) %>%
  mutate(Year = as.numeric(str_remove(Year, "t"))) # Convert "t1980" to 1980

longstates_stel <- states_stelW %>%
  pivot_longer(
    cols = starts_with("t"), 
    names_to = "Year", 
    values_to = "States_Value"
  ) %>%
  mutate(Year = as.numeric(str_remove(Year, "t"))) # Convert "t1980" to 1980

longnosa_stel <- nosa_stelPOP %>%
  pivot_longer(
    cols = starts_with("t"), 
    names_to = "Year", 
    values_to = "Nosa_Value"
  ) %>%
  mutate(Year = as.numeric(str_remove(Year, "t"))) # Convert "t1980" to 1980

# join
combineddata_chin <- left_join(longstates_chin, longnosa_chin, by = c("PopID", "Year"))
plotdata_chin <- combineddata_chin %>%
  pivot_longer(
    cols = c(States_Value, Nosa_Value),
    names_to = "Dataset",
    values_to = "Value"
  ) %>%
  mutate(Dataset = recode(Dataset, 
                          "States_Value" = "State Estimate", 
                          "Nosa_Value" = "Observation"))

combineddata_coho <- left_join(longstates_coho, longnosa_coho, by = c("PopID", "Year"))
plotdata_coho <- combineddata_coho %>%
  pivot_longer(
    cols = c(States_Value, Nosa_Value),
    names_to = "Dataset",
    values_to = "Value"
  ) %>%
  mutate(Dataset = recode(Dataset, 
                          "States_Value" = "State Estimate", 
                          "Nosa_Value" = "Observation"))

combineddata_stel <- left_join(longstates_stel, longnosa_stel, by = c("PopID", "Year"))
plotdata_stel <- combineddata_stel %>%
  pivot_longer(
    cols = c(States_Value, Nosa_Value),
    names_to = "Dataset",
    values_to = "Value"
  ) %>%
  mutate(Dataset = recode(Dataset, 
                          "States_Value" = "State Estimate", 
                          "Nosa_Value" = "Observation"))

# drop fitted estimates from before any observations were made
plotdata_chin <- plotdata_chin %>%
  group_by(PopID) %>%
  filter(
    trimws(Dataset) == "Observation" | 
      (trimws(Dataset) == "State Estimate" & Year >= min(Year[trimws(Dataset) == "Observation" & !is.na(Value)], na.rm = TRUE) &
         Year <= max(Year[trimws(Dataset) == "Observation" & !is.na(Value)], na.rm = TRUE))) %>%
  ungroup()

plotdata_coho <- plotdata_coho %>%
  group_by(PopID) %>%
  filter(
    trimws(Dataset) == "Observation" | 
      (trimws(Dataset) == "State Estimate" & Year >= min(Year[trimws(Dataset) == "Observation" & !is.na(Value)], na.rm = TRUE) &
         Year <= max(Year[trimws(Dataset) == "Observation" & !is.na(Value)], na.rm = TRUE))) %>%
  ungroup()

plotdata_stel <- plotdata_stel %>%
  group_by(PopID) %>%
  filter(
    trimws(Dataset) == "Observation" | 
      (trimws(Dataset) == "State Estimate" & Year >= min(Year[trimws(Dataset) == "Observation" & !is.na(Value)], na.rm = TRUE) &
         Year <= max(Year[trimws(Dataset) == "Observation" & !is.na(Value)], na.rm = TRUE))) %>%
  ungroup()

# pull in esu names
plotdata_chin <- plotdata_chin %>%
  left_join(
    esu_list %>% mutate(PopID = as.character(PopID)),
    by = "PopID"
  ) 

# it looks like there are "N/A" strings written in the data for john day
plotdata_chin <- plotdata_chin[plotdata_chin$ESAPOPNAME != "N/A", ]

plotdata_coho <- plotdata_coho %>%
  left_join(
    esu_list %>% mutate(PopID = as.character(PopID)),
    by = "PopID"
  ) 

plotdata_stel <- plotdata_stel %>%
  left_join(
    esu_list %>% mutate(PopID = as.character(PopID)),
    by = "PopID"
  ) 
plotdata_stel <- plotdata_stel[plotdata_stel$ESAPOPNAME != "N/A", ]

# exponentiate values to sum
plotdata_chin$Value <- exp(plotdata_chin$Value)
plotdata_coho$Value <- exp(plotdata_coho$Value)
plotdata_stel$Value <- exp(plotdata_stel$Value)

# sumamrize
plotdata_chinESU <- plotdata_chin %>%
  group_by(Year, Dataset, ESAPOPNAME) |> 
  summarise(
    Value = sum(Value, na.rm = TRUE)
  )

plotdata_cohoESU <- plotdata_coho %>%
  group_by(Year, Dataset, ESAPOPNAME) |> 
  summarise(
    Value = sum(Value, na.rm = TRUE)
  )
# some zero values in the 80s on th ecoast originating from n/a's in the data
plotdata_cohoESU <- plotdata_cohoESU[plotdata_cohoESU$Value != 0, ]

plotdata_stelESU <- plotdata_stel %>%
  group_by(Year, Dataset, ESAPOPNAME) |> 
  summarise(
    Value = sum(Value, na.rm = TRUE)
  )
# some zero values in the 2010s - looks like monitoring stopped in the upper willamette and then the snake later?
plotdata_stelESU <- plotdata_stelESU[plotdata_stelESU$Value != 0, ]

# back to log space
plotdata_chinESU$Value <- log(plotdata_chinESU$Value)
plotdata_cohoESU$Value <- log(plotdata_cohoESU$Value)
plotdata_stelESU$Value <- log(plotdata_stelESU$Value)

# some new plots
ESUcompare_chin <- ggplot(plotdata_chinESU, aes(x = Year, y = Value, color = Dataset)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ ESAPOPNAME, scales = "free_y") + # 'free_y' adjusts vertical scales for each population
  labs(
    title = "Chinook ESU Time Series Comparison (1980-2024)",
    x = "",
    y = "ln(NOSA)",
    color = ""
  ) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold") # Makes PopID headers bold
  ) +
  theme_minimal() + 
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold") # Makes PopID headers bold
  ) + 
  theme(
    panel.grid = element_blank()
  )
ESUcompare_chin

ESUcompare_coho <- ggplot(plotdata_cohoESU, aes(x = Year, y = Value, color = Dataset)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ ESAPOPNAME, scales = "free_y") + # 'free_y' adjusts vertical scales for each population
  labs(
    title = "Coho ESU Time Series Comparison (1980-2024)",
    x = "",
    y = "ln(NOSA)",
    color = ""
  ) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold") # Makes PopID headers bold
  ) +
  theme_minimal() +  
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold") # Makes PopID headers bold
  ) + 
  theme(
    panel.grid = element_blank()
  )
ESUcompare_coho

ESUcompare_stel <- ggplot(plotdata_stelESU, aes(x = Year, y = Value, color = Dataset)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ ESAPOPNAME, scales = "free_y") + # 'free_y' adjusts vertical scales for each population
  labs(
    title = "Steelhead DPS Time Series Comparison (1980-2024)",
    x = "",
    y = "ln(NOSA)",
    color = ""
  ) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold") # Makes PopID headers bold
  ) +
  theme_minimal() + 
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold") # Makes PopID headers bold
  ) + 
  theme(
    panel.grid = element_blank()
  )
ESUcompare_stel

# total difference between observed and estimated by ESU throughout study period
# chinook
ESUdiff_chin <- plotdata_chinESU %>%
  group_by(Year, ESAPOPNAME, Dataset) %>% 
  mutate(row_id = row_number()) %>% 
  ungroup() %>% 
  pivot_wider(
    names_from = Dataset, 
    values_from = Value
  ) %>%
  select(-row_id) %>% 
  # fix spaces in the new column names so they are easier to work with
  rename(Nosa = `Observation`, States = `State Estimate`) %>%
  mutate(
    Nosa = exp(Nosa),
    States = exp(States),
    yearly_diff = Nosa - States,          # estimates are over/under-counting
    abs_yearly_diff = abs(Nosa - States)  # magnitude of error, ignoring direction
  ) %>%
  # group by population to calculate the total totals
  group_by(ESAPOPNAME) %>%
  summarize(
    total_net_difference = sum(yearly_diff, na.rm = TRUE),
    total_absolute_difference = sum(abs_yearly_diff, na.rm = TRUE),
    years_compared = sum(!is.na(yearly_diff)) # how many years actually had data for both
  )
ESUdiff_chin$avg_netdiff <- ESUdiff_chin$total_net_difference/ESUdiff_chin$years_compared
ESUdiff_chin <- ESUdiff_chin |> 
  group_by(ESAPOPNAME) %>%  
  mutate(
    LNtotal_net_difference = log(total_net_difference),
    LNtotal_absolute_difference = log(total_absolute_difference),
    LNavg_netdiff = log(avg_netdiff)
  )
print(ESUdiff_chin)

# coho
ESUdiff_coho <- plotdata_cohoESU %>%
  group_by(Year, ESAPOPNAME, Dataset) %>% 
  mutate(row_id = row_number()) %>% 
  ungroup() %>% 
  pivot_wider(
    names_from = Dataset, 
    values_from = Value
  ) %>%
  select(-row_id) %>% 
  # fix spaces in the new column names so they are easier to work with
  rename(Nosa = `Observation`, States = `State Estimate`) %>%
  mutate(
    Nosa = exp(Nosa),
    States = exp(States),
    yearly_diff = Nosa - States,          # estimates are over/under-counting
    abs_yearly_diff = abs(Nosa - States)  # magnitude of error, ignoring direction
  ) %>%
  # group by population to calculate the total totals
  group_by(ESAPOPNAME) %>%
  summarize(
    total_net_difference = sum(yearly_diff, na.rm = TRUE),
    total_absolute_difference = sum(abs_yearly_diff, na.rm = TRUE),
    years_compared = sum(!is.na(yearly_diff)) # how many years actually had data for both
  )
ESUdiff_coho$avg_netdiff <- ESUdiff_coho$total_net_difference/ESUdiff_coho$years_compared
ESUdiff_coho <- ESUdiff_coho |> 
  group_by(ESAPOPNAME) %>%  
  mutate(
    LNtotal_net_difference = log(total_net_difference),
    LNtotal_absolute_difference = log(total_absolute_difference),
    LNavg_netdiff = log(avg_netdiff)
  )
print(ESUdiff_coho)

# steelhead
ESUdiff_stel <- plotdata_stelESU %>%
  group_by(Year, ESAPOPNAME, Dataset) %>% 
  mutate(row_id = row_number()) %>% 
  ungroup() %>% 
  pivot_wider(
    names_from = Dataset, 
    values_from = Value
  ) %>%
  select(-row_id) %>% 
  # fix spaces in the new column names so they are easier to work with
  rename(Nosa = `Observation`, States = `State Estimate`) %>%
  mutate(
    Nosa = exp(Nosa),
    States = exp(States),
    yearly_diff = Nosa - States,          # estimates are over/under-counting
    abs_yearly_diff = abs(Nosa - States)  # magnitude of error, ignoring direction
  ) %>%
  # group by population to calculate the total totals
  group_by(ESAPOPNAME) %>%
  summarize(
    total_net_difference = sum(yearly_diff, na.rm = TRUE),
    total_absolute_difference = sum(abs_yearly_diff, na.rm = TRUE),
    years_compared = sum(!is.na(yearly_diff)) # how many years actually had data for both
  )
ESUdiff_stel$avg_netdiff <- ESUdiff_stel$total_net_difference/ESUdiff_stel$years_compared
ESUdiff_stel <- ESUdiff_stel |> 
  group_by(ESAPOPNAME) %>%  
  mutate(
    LNtotal_net_difference = log(total_net_difference),
    LNtotal_absolute_difference = log(total_absolute_difference),
    LNavg_netdiff = log(avg_netdiff)
  )
print(ESUdiff_stel)

# save difference data
save(ESUdiff_chin, file=here("data", "clean", "ESUdiff_chin.Rda"))
save(ESUdiff_coho, file=here("data", "clean", "ESUdiff_coho.Rda"))
save(ESUdiff_stel, file=here("data", "clean", "ESUdiff_stel.Rda"))