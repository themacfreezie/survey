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
states_chin$Dataset <- "fitted"

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
states_coho$Dataset <- "fitted"

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
states_stel$Dataset <- "fitted"

# structure observed data to drop fitted obs w/ no survey observations
nosa_chinESU$Dataset <- "observed"
nosa_cohoESU$Dataset <- "observed"
nosa_stelESU$Dataset <- "observed"

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
    trimws(Dataset) == "observed" | 
      (trimws(Dataset) == "fitted" & Year >= min(Year[trimws(Dataset) == "observed" & !is.na(lnnosa)], na.rm = TRUE) &
         Year <= max(Year[trimws(Dataset) == "observed" & !is.na(lnnosa)], na.rm = TRUE))) %>%
  ungroup()
nosa_chin <- nosa_chin[nosa_chin$Dataset != "observed", ]

nosa_coho <- nosa_coho %>%
  group_by(ESANAME) %>%
  filter(
    trimws(Dataset) == "observed" | 
      (trimws(Dataset) == "fitted" & Year >= min(Year[trimws(Dataset) == "observed" & !is.na(lnnosa)], na.rm = TRUE) &
         Year <= max(Year[trimws(Dataset) == "observed" & !is.na(lnnosa)], na.rm = TRUE))) %>%
  ungroup()
nosa_coho <- nosa_coho[nosa_coho$Dataset != "observed", ]

nosa_stel <- nosa_stel %>%
  group_by(ESANAME) %>%
  filter(
    trimws(Dataset) == "observed" | 
      (trimws(Dataset) == "fitted" & Year >= min(Year[trimws(Dataset) == "observed" & !is.na(lnnosa)], na.rm = TRUE) &
         Year <= max(Year[trimws(Dataset) == "observed" & !is.na(lnnosa)], na.rm = TRUE))) %>%
  ungroup()
nosa_stel <- nosa_stel[nosa_stel$Dataset != "observed", ]

# plot em up! - chinook
nosa_chin_plotted <- nosa_chin %>%
  mutate(
    lower = if_else(Dataset == "fitted", lnnosa - (1.96 * SE), NA_real_),
    upper = if_else(Dataset == "fitted", lnnosa + (1.96 * SE), NA_real_)
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
  scale_color_manual(values = c("fitted" = "#1f77b4")) +
  scale_fill_manual(values = c("fitted" = "#1f77b4")) +
  labs(
    title = "Chinook - Fitted Values by ESU",
    x = "Year",
    y = "ln(NOSA)"
    # color = "Data Type",
    # fill = "Data Type"
  ) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "none"
  )
  # these aren't comparable because the fitted ESU is identifying an underlying state but not aggregating 
  # all sampled populations
    ## tweaked

# plot em up! - coho
nosa_coho_plotted <- nosa_coho %>%
  mutate(
    lower = if_else(Dataset == "fitted", lnnosa - (1.96 * SE), NA_real_),
    upper = if_else(Dataset == "fitted", lnnosa + (1.96 * SE), NA_real_)
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
  scale_color_manual(values = c("fitted" = "#1f77b4")) +
  scale_fill_manual(values = c("fitted" = "#1f77b4")) +
  labs(
    title = "Coho - Fitted Values by ESU",
    x = "Year",
    y = "ln(NOSA)"
  ) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "none"
  )

# plot em up! - steelies
nosa_stel_plotted <- nosa_stel %>%
  mutate(
    lower = if_else(Dataset == "fitted", lnnosa - (1.96 * SE), NA_real_),
    upper = if_else(Dataset == "fitted", lnnosa + (1.96 * SE), NA_real_)
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
  scale_color_manual(values = c("fitted" = "#1f77b4")) +
  scale_fill_manual(values = c("fitted" = "#1f77b4")) +
  labs(
    title = "Steelhead - Fitted Values by DPS",
    x = "Year",
    y = "ln(NOSA)"
  ) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "none"
  )






# let's do the same but by ESU/DPS
esu_list <- pop_list[-c(1, 3, 5:10)]
esu_list$ESAPOPNAME <- sub("\\).*$", ")", esu_list$ESAPOPNAME)

# pull in esu names
plotdata_chin <- plotdata_chin %>%
  left_join(
    esu_list %>% mutate(PopID = as.character(PopID)),
    by = "PopID"
  ) 
# john day is notable here - no ESU? question for kasey and jake
# google says they are not esa listed - wild!

# it looks like there are "N/A" strings written in the data for john day
plotdata_chin <- plotdata_chin[plotdata_chin$ESAPOPNAME != "N/A", ]

plotdata_chinESU <- plotdata_chin %>%
  group_by(Year, Dataset, ESAPOPNAME) |> 
  summarise(
    Value = sum(Value, na.rm = TRUE)
  )

plotdata_coho <- plotdata_coho %>%
  left_join(
    esu_list %>% mutate(PopID = as.character(PopID)),
    by = "PopID"
  ) 

plotdata_cohoESU <- plotdata_coho %>%
  group_by(Year, Dataset, ESAPOPNAME) |> 
  summarise(
    Value = sum(Value, na.rm = TRUE)
  )

# some zero values in the 80s on th ecoast originating from n/a's in the data
plotdata_cohoESU <- plotdata_cohoESU[plotdata_cohoESU$Value != 0, ]

plotdata_stel2 <- plotdata_stel2 %>%
  left_join(
    esu_list %>% mutate(PopID = as.character(PopID)),
    by = "PopID"
  ) 

plotdata_stel2 <- plotdata_stel2[plotdata_stel2$ESAPOPNAME != "N/A", ]

plotdata_stelESU <- plotdata_stel2 %>%
  group_by(Year, Dataset, ESAPOPNAME) |> 
  summarise(
    Value = sum(Value, na.rm = TRUE)
  )

# some zero values in the 2010s - looks like monitoring stopped in the upper willamette and then the snake later?
plotdata_stelESU <- plotdata_stelESU[plotdata_stelESU$Value != 0, ]

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
  rename(Nosa = `Observed Value`, States = `Estimated State`)  %>%
  mutate(
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
  rename(Nosa = `Observed Value`, States = `Estimated State`)  %>%
  mutate(
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
  rename(Nosa = `Observed Value`, States = `Estimated State`)  %>%
  mutate(
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
print(ESUdiff_stel)

# save difference data
save(ESUdiff_chin, file=here("data", "clean", "ESUdiff_chin.Rda"))
save(ESUdiff_coho, file=here("data", "clean", "ESUdiff_coho.Rda"))
save(ESUdiff_stel, file=here("data", "clean", "ESUdiff_stel.Rda"))