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

# structure observed data to match
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

# plot em up!
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
  scale_color_manual(values = c("fitted" = "#1f77b4", "observed" = "#ff7f0e")) +
  scale_fill_manual(values = c("fitted" = "#1f77b4", "observed" = NA)) +
  labs(
    title = "Chinook - Observed vs. Fitted Values by ESU",
    x = "Year",
    y = "ln(NOSA)",
    color = "Data Type",
    fill = "Data Type"
  ) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "bottom"
  )