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

## data cleaning
# pull in data - model objects
ssm_chin <- readRDS(file=here::here("data", "clean", "ssm_chin_Afull.rds"))
ssm_coho <- readRDS(file=here::here("data", "clean", "ssm_coho_Afull.rds"))
ssm_stel <- readRDS(file=here::here("data", "clean", "ssm_stel_Afull.rds"))

# pull in data - state key
key_chin <- read_excel(here("data", "clean", "xtT_statekey.xlsx"), sheet = "chin")
key_coho <- read_excel(here("data", "clean", "xtT_statekey.xlsx"), sheet = "coho")
key_stel <- read_excel(here("data", "clean", "xtT_statekey.xlsx"), sheet = "stel")

# pull in data - observed time series
load(file=here::here("data", "clean", "nosa_chinPOP.Rda"))
load(file=here::here("data", "clean", "nosa_cohoPOP.Rda"))
load(file=here::here("data", "clean", "nosa_stelPOP.Rda"))

# # pull in data - population list
nmfs_popid <- read_excel(here("data", "raw", "cap-hli.xls"), sheet = "NOSA")

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
states$Dataset <- "fitted"

# create long format nosa data to drop fitted values from before (or after) observed values exist
names(nosa_chinPOP)[2:46] <- paste0("t", 1980:2024)
longnosa_chin <- nosa_chinPOP %>%
  pivot_longer(
    cols = starts_with("t"), 
    names_to = "Year", 
    values_to = "Value"
  ) %>%
  mutate(Year = as.numeric(str_remove(Year, "t"))) # Convert "t1980" to 1980
longnosa_chin$SE <- NA
longnosa_chin$species <- "chinook"
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
longnosa_coho$species <- "coho"
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
longnosa_stel$species <- "steelhead"
longnosa_stel$Dataset <- "observed"

combineddata <- rbind(states, longnosa_chin, longnosa_coho, longnosa_stel)

# drop estimates from before any observations were made
combineddata <- combineddata %>%
  group_by(PopID) %>%
  filter(
    trimws(Dataset) == "observed" | 
      (trimws(Dataset) == "fitted" & Year >= min(Year[trimws(Dataset) == "observed" & !is.na(Value)], na.rm = TRUE) &
         Year <= max(Year[trimws(Dataset) == "observed" & !is.na(Value)], na.rm = TRUE))) %>%
  ungroup()

states <- combineddata |> filter(Dataset=="fitted")

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
FLstates$PopID <- as.numeric(FLstates$PopID)

# bring in nmfs popid
nmfs_popid <- nmfs_popid[-c(1:4, 7:112)]

lookup_nmfs <- nmfs_popid %>%
  distinct(POPID, NMFS_POPID)

length(unique(lookup_nmfs$POPID))
table(lookup_nmfs$POPID)

length(unique(lookup_nmfs$NMFS_POPID))
table(lookup_nmfs$NMFS_POPID)
  # popid 500 to 506 has na for NMFS popID 
  # 3 steelhead and 3 chinook pops - john day and the lower columbia

lookup_nmfs <- lookup_nmfs %>%
  rename(PopID = POPID,
         NWFSC_POP_ID = NMFS_POPID)

# merge in NWFSC pop IDs (to match with gis)
FLstates <- left_join(FLstates, lookup_nmfs, by = "PopID")

# for some reason 128 and 146 aren't present for observed code?
# why is this?
FLstates <- FLstates |> filter(NWFSC_POP_ID != 128)
FLstates <- FLstates |> filter(NWFSC_POP_ID != 146)

saveRDS(FLstates %>% filter(species=="chinook"), here("data", "clean", "FLstates_chin.rds"))
saveRDS(FLstates %>% filter(species=="coho"), here("data", "clean", "FLstates_coho.rds"))
saveRDS(FLstates %>% filter(species=="steelhead"), here("data", "clean", "FLstates_stel.rds"))

# Species attributes
species_list <- list(
  Chinook   = here("data", "clean", "FLstates_chin.rds"),
  Coho      = here("data", "clean", "FLstates_coho.rds"),
  Steelhead = here("data", "clean", "FLstates_stel.rds")
)

## spatial stuff
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

# Background Regional Context Map
region_states <- states(cb = TRUE, resolution = "20m") %>%
  filter(STUSPS %in% c("OR", "WA", "ID")) %>%
  st_transform(4269)

basin_xlim <- c(-125, -110)
basin_ylim <- c(41.5, 49.5)

# processing pipeline
process_species_data <- function(file_path) {
  ar_data <- readRDS(file_path) %>% filter(!is.na(NWFSC_POP_ID))
  
  sf_combined <- sf_fish %>% 
    left_join(ar_data, by = "NWFSC_POP_ID") %>%
    filter(!is.na(first10_mean)) %>%
    st_transform(crs = 4269)
  
  sf_collapsed <- sf_combined %>%
    group_by(NWFSC_POP_ID, DPS_IDtrunc, DPStrunc) %>%
    summarize(
      first10_mean = mean(first10_mean, na.rm = TRUE),
      last10_mean  = mean(last10_mean, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      pctchange_lnnosa = ((last10_mean - first10_mean) / first10_mean) * 100
    )
  
  return(sf_collapsed)
}

# process all three data sets into a structured list
processed_data <- map(species_list, process_species_data)

# unify color scale
# bind values together to detect absolute global limits across all datasets
raw_range <- map_dfr(processed_data, ~ as_tibble(.x) %>% select(pctchange_lnnosa)) %>%
  pull(pctchange_lnnosa) %>%
  range(na.rm = TRUE)

# make limits symmetrical around 0 for the diverging palette
max_abs_val <- max(abs(raw_range), na.rm = TRUE)
global_range <- c(-max_abs_val, max_abs_val)

# custom limits, because global range washes out most observations
custom_limits <- c(-25, 25) #difference MUST be multiple of ten

## plot


generate_species_plot <- function(data_sf, title_text, limits) {
  
  # Outlines and borders
  sf_outlines <- data_sf %>%
    group_by(DPS_IDtrunc, DPStrunc) %>%
    summarize(SHAPE = st_union(SHAPE), .groups = "drop")
  
  # Safe intersection for internal borders
  suppressWarnings({
    intersections <- st_intersection(sf_outlines, sf_outlines)
  })
  
  shared_borders <- intersections %>%
    filter(DPS_IDtrunc != DPS_IDtrunc.1) %>%
    filter(st_geometry_type(SHAPE) %in% c("POLYGON", "MULTIPOLYGON")) %>%
    st_boundary() %>%
    st_union()
  
  # Split logic for the striped Population 104
  sf_base   <- data_sf %>% filter(NWFSC_POP_ID != 104)
  sf_stripe <- data_sf %>% filter(NWFSC_POP_ID == 104)
  
  # Inset block configuration
  bbox <- st_bbox(data_sf)
  inset_context <- ggplot() +
    geom_sf(data = region_states, fill = "gray95", color = "gray60", linewidth = 0.3) +
    annotate("rect", xmin = bbox["xmin"], xmax = bbox["xmax"], ymin = bbox["ymin"], ymax = bbox["ymax"], 
             color = "red", fill = NA, linewidth = 0.6) +
    coord_sf(xlim = basin_xlim, ylim = basin_ylim, expand = FALSE) +
    theme_void() +
    theme(
      panel.background = element_rect(fill = "white", color = "black"),
      panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.8),
      plot.margin      = margin(1, 1, 1, 1)
    )
  
  # Base Main Map Engine
  main_map <- ggplot() +
    annotation_map_tile(type = "hotstyle", zoom = 10) +
    geom_sf(data = sf_base, aes(fill = pctchange_lnnosa), alpha = 0.8, color = "white", size = 0.1)
  
  # Append pattern overlay if population 104 exists in dataset
  if (nrow(sf_stripe) > 0) {
    main_map <- main_map +
      geom_sf_pattern(
        data = sf_stripe, aes(pattern_fill = pctchange_lnnosa),
        pattern = 'stripe', pattern_color = NA, 
        pattern_density = 0.25, pattern_spacing = 0.015, pattern_angle = 45,
        fill = NA, alpha = 1
      )
  }
  
  # Top layers and theme styling
  main_map <- main_map +
    geom_sf(data = shared_borders, color = "black", linetype = "dashed", linewidth = 0.6) +
    geom_sf(data = sf_outlines, fill = NA, color = "black", linewidth = 1.2) +
    scale_fill_distiller(
      palette = palette,
      aesthetics = c("fill", "pattern_fill"),
      name = "% Change",
      limits = custom_limits,
      oob = scales::squish, # keeps outliers clamped to endpoints
      breaks = c(custom_limits[1], seq(from = custom_limits[1], to = custom_limits[2], by = 10), custom_limits[2]),
      labels = function(x) {
        min_lim <- custom_limits[1]
        max_lim <- custom_limits[2]
        # format labels conditionally based on limits
        case_when(
          x == min_lim ~ paste0("≤ ", x, "%"),
          x == max_lim ~ paste0("≥ ", x, "%"),
          TRUE         ~ paste0(x, "%")
        )
      }
    ) +
    coord_sf(crs = 4269) +
    labs(title = title_text) +
    theme_minimal() +
    theme(       
      plot.title = element_text(face = "bold", size = 28),       
      legend.position = "none",       
      axis.text.x = element_text(size = 16, color = "black"),       
      axis.text.y= element_text(size = 16, color = "black"),     
    ) 
  
  # Composite map with inset
  final_panel <- main_map + 
    inset_element(inset_context, left = 0.68, bottom = 0.03, right = 0.98, top = 0.33)
  
  return(final_panel)
}

# ==========================================
# 5. ASSEMBLE MULTIPANEL USING PATCHWORK
# ==========================================

# Generate individual plots utilizing global shared limits
p1 <- generate_species_plot(processed_data$Chinook, "Chinook", global_range)
p2 <- generate_species_plot(processed_data$Coho, "Coho", global_range)
p3 <- generate_species_plot(processed_data$Steelhead, "Steelhead", global_range)

# Append the specific caption to p1 (Chinook) only and style it
# p1 <- p1 +
#   labs(caption = "Solid color = Lower Columbia ESU | Striped color = Upper Willamette ESU")

# Extract a shared legend using a dummy plot setup
legend_plot <- ggplot(processed_data$Chinook) +
  geom_sf(aes(fill = pctchange_lnnosa)) +
  scale_fill_distiller(
    palette = palette,
    name = "Pct Change\nPop Size\n(1980-2024)",
    limits = custom_limits,
    oob = scales::squish, # keeps outliers clamped to endpoints
    breaks = c(custom_limits[1], seq(from = custom_limits[1], to = custom_limits[2], by = 10), custom_limits[2]),
    labels = function(x) {
      min_lim <- custom_limits[1]
      max_lim <- custom_limits[2]
      # format labels conditionally based on limits
      case_when(
        x == min_lim ~ paste0("≤ ", x, "%"),
        x == max_lim ~ paste0("≥ ", x, "%"),
        TRUE         ~ paste0(x, "%")
      )
    }
  ) +
  theme_minimal() +
  theme(
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 12, face = "bold"),
    legend.key.height = unit(1.5, "cm")
  )
shared_legend <- cowplot::get_legend(legend_plot)

# Convert the grob into a patchwork-compatible element and apply a negative margin
# This forcefully shifts the standalone color bar to the left, closing the white gap
clean_legend_panel <- patchwork::wrap_elements(shared_legend) +
  theme(plot.margin = margin(l = -40, r = 0, t = 0, b = 0))

# --- ASSEMBLE VIA PURE PATCHWORK ---
# Column 1: Stacked maps | Column 2: Large Coho map | Column 3: Tight clean legend
final_output <- (p1 / p3) | p2 | clean_legend_panel +
  plot_layout(
    widths = c(1.2, 1, 0.2), # Sharp layout boundary definitions
    guides = "keep"
  )

# Render complete layout
final_output
