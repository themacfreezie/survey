## SET WORKING DIR & PACKAGES
library(biscale)
library(cowplot)
library(ggpattern)
library(ggspatial)
library(here)
library(MARSS)
library(pals)
library(patchwork)
library(prettymapr)
library(readxl)
library(sf)
library(stringr)
library(tidyverse)
library(tigris)

here::i_am("code/primary/06.4.2-multipanelFIRSTLASTx.R")
options(max.print = 2000)

# pull in data - model objects
ssm_chin <- readRDS(file=here::here("data", "clean", "ssm_chin_Afull.rds"))
ssm_coho <- readRDS(file=here::here("data", "clean", "ssm_coho_Afull.rds"))
ssm_stel <- readRDS(file=here::here("data", "clean", "ssm_stel_Afull.rds"))

# pull in data - state key
key_chin <- read_excel(here("data", "clean", "xtT_statekey.xlsx"), sheet = "chin")
key_coho <- read_excel(here("data", "clean", "xtT_statekey.xlsx"), sheet = "coho")
key_stel <- read_excel(here("data", "clean", "xtT_statekey.xlsx"), sheet = "stel")

# # pull in data - population list
# load(here("data", "clean", "populations_list.Rda"))
# pop_list <- pop_list[-c(52, 68, 69), ]
#   # quick clean to drop duplicates
# pop_list <- pop_list[-c(1, 3:4, 6:length(pop_list))]

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
states_chin$species <- "chinook"

names(states_coho)[names(states_coho) == ".rownames"] <- "state"
states_coho$state <- as.numeric(sub("^X", "", states_coho$state))
states_coho <- states_coho %>%
  left_join(key_coho, by = c("state" = "State")) %>%
  select(-state)
states_coho$Year <- states_coho$t + 1979
states_coho <- rename(states_coho, Value = .estimate, SE = .se)
states_coho <- states_coho[-c(1)]
states_coho$species <- "coho"

names(states_stel)[names(states_stel) == ".rownames"] <- "state"
states_stel$state <- as.numeric(sub("^X", "", states_stel$state))
states_stel <- states_stel %>%
  left_join(key_stel, by = c("state" = "State")) %>%
  select(-state)
states_stel$Year <- states_stel$t + 1979
states_stel <- rename(states_stel, Value = .estimate, SE = .se)
states_stel <- states_stel[-c(1)]
states_stel$species <- "steelhead"

# bind these together
states <- bind_rows(states_chin, states_coho, states_stel)
duplicated(states)

# pulling averages for first and last decade of observations by population
ordered_states <- states %>%
  group_by(PopID, species) %>%
  arrange(Year, .by_group = TRUE) %>% # Sort chronologically within each group
  ungroup()

# get averages of first and last ten years
first10_means <- ordered_states %>%
  group_by(PopID, species) %>%
  slice_head(n = 10) %>% # Grabs the earliest 10 available records per group
  summarize(
    first10_mean = mean(Value, na.rm = TRUE),
    .groups             = "drop"
  )

last10_means <- ordered_states %>%
  group_by(PopID, species) %>%
  slice_tail(n = 10) %>% # Grabs the most recent 10 available records per group
  summarize(
    last10_mean = mean(Value, na.rm = TRUE),
    .groups            = "drop"
  )

FLstates <- full_join(first10_means, last10_means, by = c("PopID", "species"))

## bringing in spatial stuff

# set palette
palette <- "RdBu"

# Base geographical boundaries
gdb_path <- here("data", "raw", "WCR_Salmon_Steelhead_gdb_2015", "WCR_Salmon_Steelhead_gdb_2015.gdb")
sf_fish  <- read_sf(dsn = gdb_path, layer = "fish")

sf_fish <- sf_fish %>%
  mutate(
    DPS_IDtrunc = substr(DPS_ID, 1, 5),
    DPStrunc    = str_remove(DPS, " - Outside legal area$")
  )

# Species attributes
species_list <- list(
  Chinook   = FLstates %>% filter(species=="chinook"),
  Coho      = FLstates %>% filter(species=="coho"),
  Steelhead = FLstates %>% filter(species=="steelhead")
)

# Background Regional Context Map
region_states <- states(cb = TRUE, resolution = "20m") %>%
  filter(STUSPS %in% c("OR", "WA", "ID")) %>%
  st_transform(4269)

basin_xlim <- c(-125, -110)
basin_ylim <- c(41.5, 49.5)
