## SET WORKING DIR & PACKAGES
library(ggpattern)
library(ggspatial)
library(here)
library(patchwork)
library(sf)
library(tidyverse)
library(tigris)

here::i_am("code/primary/10-TSmap.R")
# what do I mean by TS?
options(max.print=2000)

# set palette of choice
palette <- "cividis"

# pull in diff data
load(file=here("data", "clean", "popdiff_chin.Rda"))
load(file=here("data", "clean", "popdiff_coho.Rda"))
load(file=here("data", "clean", "popdiff_stel.Rda"))

load(file=here("data", "clean", "ESUdiff_chin.Rda"))
load(file=here("data", "clean", "ESUdiff_coho.Rda"))
load(file=here("data", "clean", "ESUdiff_stel.Rda"))

# pull in AR data as key to connect NWFSC pop IDs to ODFW pop IDs (silly)
ARchin <- readRDS(here("data", "clean", "popavgAR_chin.rds"))
chinIDkey <- ARchin[-c(2:13)]
ARcoho <- readRDS(here("data", "clean", "popavgAR_coho.rds"))
cohoIDkey <- ARcoho[-c(2:13)]
ARstel <- readRDS(here("data", "clean", "popavgAR_stel.rds"))
stelIDkey <- ARstel[-c(2:13)]

# pull in spatial dataframes & merge w/ popdiff
sf_chin <- readRDS(file=here::here("data", "clean", "sf_chin_nad83.rds"))
st_crs(sf_chin)
# sf_chin <- st_transform(sf_chin, crs = 3857)
popdiff_chin <- popdiff_chin %>% mutate(PopID = as.character(PopID))
chinIDkey <- chinIDkey %>% mutate(PopID = as.character(PopID))
popdiff_chin <- popdiff_chin %>%
  left_join(chinIDkey, by = "PopID")
sf_chin <- sf_chin %>% mutate(NWFSC_POP_ID = as.character(NWFSC_POP_ID))
popdiff_chin <- popdiff_chin %>% mutate(NWFSC_POP_ID = as.character(NWFSC_POP_ID))
sf_chin_combined <- sf_chin %>%
  left_join(popdiff_chin, by = "NWFSC_POP_ID")

sf_coho <- readRDS(file=here::here("data", "clean", "sf_coho_nad83.rds"))
st_crs(sf_coho)
# sf_coho <- st_transform(sf_coho, crs = 3857)
popdiff_coho <- popdiff_coho %>% mutate(PopID = as.character(PopID))
cohoIDkey <- cohoIDkey %>% mutate(PopID = as.character(PopID))
popdiff_coho <- popdiff_coho %>%
  left_join(cohoIDkey, by = "PopID")
sf_coho <- sf_coho %>% mutate(NWFSC_POP_ID = as.character(NWFSC_POP_ID))
popdiff_coho <- popdiff_coho %>% mutate(NWFSC_POP_ID = as.character(NWFSC_POP_ID))
sf_coho_combined <- sf_coho %>%
  left_join(popdiff_coho, by = "NWFSC_POP_ID")

sf_stel <- readRDS(file=here::here("data", "clean", "sf_stel_nad83.rds"))
st_crs(sf_stel)
# sf_stel <- st_transform(sf_stel, crs = 3857)
popdiff_stel <- popdiff_stel %>% mutate(PopID = as.character(PopID))
stelIDkey <- stelIDkey %>% mutate(PopID = as.character(PopID))
popdiff_stel <- popdiff_stel %>%
  left_join(stelIDkey, by = "PopID")
sf_stel <- sf_stel %>% mutate(NWFSC_POP_ID = as.character(NWFSC_POP_ID))
popdiff_stel <- popdiff_stel %>% mutate(NWFSC_POP_ID = as.character(NWFSC_POP_ID))
sf_stel_combined <- sf_stel %>%
  left_join(popdiff_stel, by = "NWFSC_POP_ID")

sf_fish_combined <- rbind(sf_chin_combined, sf_coho_combined)
sf_fish_combined <- rbind(sf_fish_combined, sf_stel_combined)
class(sf_fish_combined)

# collapse
## WARNING - all this needs to be rethought since it is using added natural log
  # got to go back to 09-TScomparison.R
sf_chin_combined <- sf_chin_combined %>%
  group_by(NWFSC_POP_ID, DPS_IDtrunc, DPStrunc) %>%
  summarize(
    total_net_difference = mean(total_net_difference, na.rm = TRUE),
    total_absolute_difference  = mean(total_absolute_difference, na.rm = TRUE),
    avg_netdiff = mean(avg_netdiff, na.rm = TRUE),
    .groups = "drop"
  )
chinIDkey <- chinIDkey %>% mutate(NWFSC_POP_ID = as.character(NWFSC_POP_ID))
sf_chin_combined <- sf_chin_combined %>%
  left_join(chinIDkey, by = "NWFSC_POP_ID")

sf_coho_combined <- sf_coho_combined %>%
  group_by(NWFSC_POP_ID, DPS_IDtrunc, DPStrunc) %>%
  summarize(
    total_net_difference = mean(total_net_difference, na.rm = TRUE),
    total_absolute_difference  = mean(total_absolute_difference, na.rm = TRUE),
    avg_netdiff = mean(avg_netdiff, na.rm = TRUE),
    .groups = "drop"
  )
cohoIDkey <- cohoIDkey %>% mutate(NWFSC_POP_ID = as.character(NWFSC_POP_ID))
sf_coho_combined <- sf_coho_combined %>%
  left_join(cohoIDkey, by = "NWFSC_POP_ID")

sf_stel_combined <- sf_stel_combined %>%
  group_by(NWFSC_POP_ID, DPS_IDtrunc, DPStrunc) %>%
  summarize(
    total_net_difference = mean(total_net_difference, na.rm = TRUE),
    total_absolute_difference  = mean(total_absolute_difference, na.rm = TRUE),
    avg_netdiff = mean(avg_netdiff, na.rm = TRUE),
    .groups = "drop"
  )
stelIDkey <- stelIDkey %>% mutate(NWFSC_POP_ID = as.character(NWFSC_POP_ID))
sf_stel_combined <- sf_stel_combined %>%
  left_join(stelIDkey, by = "NWFSC_POP_ID")

# preplots
bbox_chin <- st_bbox(sf_chin_combined)
region_states_chin <- states(cb = TRUE, resolution = "20m") %>%
  filter(STUSPS %in% c("OR", "WA", "ID")) %>%
  st_transform(4269) # match main map's CRS (NAD83)
# these bounds roughly cover the columbia basin
basin_xlim_chin <- c(-125, -110)
basin_ylim_chin <- c(41.5, 49.5)

bbox_coho <- st_bbox(sf_coho_combined)
region_states_coho <- states(cb = TRUE, resolution = "20m") %>%
  filter(STUSPS %in% c("OR", "WA")) %>%
  st_transform(4269) # match main map's CRS (NAD83)
# these bounds roughly cover the columbia basin
basin_xlim_coho <- c(-125.0, -116.0)
basin_ylim_coho <- c(41.5, 49.5)

bbox_stel <- st_bbox(sf_stel_combined)
region_states_stel <- states(cb = TRUE, resolution = "20m") %>%
  filter(STUSPS %in% c("OR", "WA", "ID")) %>%
  st_transform(4269) # match main map's CRS (NAD83)
# these bounds roughly cover the columbia basin
basin_xlim_stel <- c(-125, -110)
basin_ylim_stel <- c(41.5, 49.5)

# create basin-centered inset
inset_context_chin <- ggplot() +
  geom_sf(data = region_states_chin, fill = "gray95", color = "gray60", linewidth = 0.3) +
  # red box representing your specific study area
  annotate("rect", 
           xmin = bbox_chin["xmin"], xmax = bbox_chin["xmax"], 
           ymin = bbox_chin["ymin"], ymax = bbox_chin["ymax"], 
           color = "red", fill = NA, linewidth = 0.8) +
  # Crop the map to the Columbia River Basin extent
  coord_sf(xlim = basin_xlim_chin, ylim = basin_ylim_chin, expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    plot.margin = margin(1, 1, 1, 1)
  )

inset_context_coho <- ggplot() +
  geom_sf(data = region_states_coho, fill = "gray95", color = "gray60", linewidth = 0.3) +
  # red box representing your specific study area
  annotate("rect", 
           xmin = bbox_coho["xmin"], xmax = bbox_coho["xmax"], 
           ymin = bbox_coho["ymin"], ymax = bbox_coho["ymax"], 
           color = "red", fill = NA, linewidth = 0.8) +
  # Crop the map to the Columbia River Basin extent
  coord_sf(xlim = basin_xlim_coho, ylim = basin_ylim_coho, expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    plot.margin = margin(1, 1, 1, 1)
  )

inset_context_stel <- ggplot() +
  geom_sf(data = region_states_stel, fill = "gray95", color = "gray60", linewidth = 0.3) +
  # red box representing your specific study area
  annotate("rect", 
           xmin = bbox_stel["xmin"], xmax = bbox_stel["xmax"], 
           ymin = bbox_stel["ymin"], ymax = bbox_stel["ymax"], 
           color = "red", fill = NA, linewidth = 0.8) +
  # Crop the map to the Columbia River Basin extent
  coord_sf(xlim = basin_xlim_stel, ylim = basin_ylim_stel, expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    plot.margin = margin(1, 1, 1, 1)
  )

# create ESU outlines
sf_outlines_chin <- sf_chin_combined %>%
  group_by(DPS_IDtrunc, DPStrunc) %>%
  summarize(SHAPE = st_union(SHAPE))

sf_outlines_coho <- sf_coho_combined %>%
  group_by(DPS_IDtrunc, DPStrunc) %>%
  summarize(SHAPE = st_union(SHAPE))

sf_outlines_stel <- sf_stel_combined %>%
  group_by(DPS_IDtrunc, DPStrunc) %>%
  summarize(SHAPE = st_union(SHAPE))

# overlap - for chinook
sf_base <- sf_chin_combined %>% filter(NWFSC_POP_ID != 104)
sf_stripe <- sf_chin_combined %>% filter(NWFSC_POP_ID == 104)
# stripe layer: ONLY population 104
shared_borders <- st_intersection(sf_outlines_chin) %>% 
  filter(n.overlaps > 1) %>% 
  st_cast("MULTILINESTRING")

# plotting
main_map <- ggplot() +
  annotation_map_tile(type = "hotstyle", zoom = 10) +
  geom_sf(data = sf_base, aes(fill = total_net_difference), alpha = 0.8, color = "white", size = 0.1) +
  geom_sf_pattern(
    data = sf_stripe,
    aes(pattern_fill = total_net_difference), 
    pattern = 'stripe',
    pattern_color = NA,       # removes the default white border around stripes
    pattern_density = 0.25,    # adjust for stripe thickness
    pattern_spacing = 0.015,
    pattern_angle = 45,
    fill = NA,                # transparent fill so Pop 4's color shows between stripes
    alpha = 1                 # keep stripes opaque to see their specific color clearly
  ) +
  geom_sf(data = shared_borders, color = "black", linetype = "dashed", linewidth = 0.6) + # Shared internal DPS borders (dashed)
  geom_sf(data = sf_outlines_chin, fill = NA, color = "black", linewidth = 1.2) +   # Standard DPS outlines (solid)
  scale_fill_viridis_c(
    option = palette, 
    aesthetics = c("fill", "pattern_fill"), # Apply one scale to BOTH fill and pattern_fill
    name = "Difference"
  ) +
  coord_sf(crs = 4269) +
  labs(title = "Chinook - Sum of difference between observed and estimataed states",
       caption = "1980-2024, Solid color = Lower Columbia ESU | Striped color = Upper Willamette ESU") +
  theme_minimal()
chin_dif <- main_map + inset_element(inset_context_chin, 
                                     left = 0.7, bottom = 0.05, 
                                     right = 0.98, top = 0.3)
# chin_dif
  # unitl concerns regarding adding ln(NOSA) values can be addressed

main_map <- ggplot(data = sf_coho_combined) +
  annotation_map_tile(type = "hotstyle", zoom = 10) +
  geom_sf(aes(fill = total_net_difference), alpha = 0.8, color = "white", size = 0.1) + 
  geom_sf(data = sf_outlines_coho, fill = NA, color = "black", linewidth = 1.2) + 
  coord_sf(crs = 4269) +
  scale_fill_viridis_c(option = palette) + 
  labs(title = "Coho - Sum of difference between observed\n and estimataed states",
       caption = "1980-2024",
       fill = "Difference") +
  theme_minimal()
coho_dif <- main_map + inset_element(inset_context_coho, 
                                     left = 0.85, bottom = 0.05, 
                                     right = 1.1, top = 0.3)
# coho_dif
  # unitl concerns regarding adding ln(NOSA) values can be addressed

main_map <- ggplot(data = sf_stel_combined) +
  annotation_map_tile(type = "hotstyle", zoom = 10) +
  geom_sf(aes(fill = total_net_difference), alpha = 0.8, color = "white", size = 0.1) + 
  geom_sf(data = sf_outlines_stel, fill = NA, color = "black", linewidth = 1.2) + 
  coord_sf(crs = 4269) +
  scale_fill_viridis_c(option = palette) + 
  labs(title = "Steelhead - Sum of difference between observed and estimataed states",
       caption = "1980-2024",
       fill = "Difference") +
  theme_minimal()
stel_dif <- main_map + inset_element(inset_context_stel, 
                                     left = 0.85, bottom = 0.05, 
                                     right = 1.1, top = 0.3)
# stel_dif 
  # unitl concerns regarding adding ln(NOSA) values can be addressed

# let's get some ESU maps too
ESUchin <- sf_outlines_chin %>%
  left_join(ESUdiff_chin, by = c("DPStrunc" = "ESAPOPNAME"))
ESUchin <- ESUchin[-c(1)]

ESUcoho <- sf_outlines_coho %>%
  left_join(ESUdiff_coho, by = c("DPStrunc" = "ESAPOPNAME"))
ESUcoho <- ESUcoho[-c(1)]

ESUstel <- sf_outlines_stel %>%
  left_join(ESUdiff_stel, by = c("DPStrunc" = "ESAPOPNAME"))
ESUstel <- ESUstel[-c(1)]

# # overlap - for chinook
sf_base <- ESUchin %>% filter(DPStrunc != "Salmon, Chinook (Upper Willamette River ESU)")
sf_stripe <- ESUchin %>% filter(DPStrunc == "Salmon, Chinook (Upper Willamette River ESU)")
shared_borders <- st_intersection(sf_outlines_chin) %>%
  filter(n.overlaps > 1) %>%
  st_cast("MULTILINESTRING")
# 
# # plots plots plots
# main_map <- ggplot() +
#   annotation_map_tile(type = "hotstyle", zoom = 10) +
#   geom_sf(data = sf_base, aes(fill = total_net_difference), alpha = 0.8, color = "white", size = 0.1) +
#   geom_sf_pattern(
#     data = sf_stripe,
#     aes(pattern_fill = total_net_difference), 
#     pattern = 'stripe',
#     pattern_color = NA,       # removes the default white border around stripes
#     pattern_density = 0.25,    # adjust for stripe thickness
#     pattern_spacing = 0.015,
#     pattern_angle = 45,
#     fill = NA,                # transparent fill so Pop 4's color shows between stripes
#     alpha = 1                 # keep stripes opaque to see their specific color clearly
#   ) +
#   geom_sf(data = shared_borders, color = "black", linetype = "dashed", linewidth = 0.6) + # Shared internal DPS borders (dashed)
#   geom_sf(data = sf_outlines_chin, fill = NA, color = "black", linewidth = 1.2) +   # Standard DPS outlines (solid)
#   scale_fill_viridis_c(
#     option = palette, 
#     aesthetics = c("fill", "pattern_fill"), # Apply one scale to BOTH fill and pattern_fill
#     name = "Difference"
#   ) +
#   coord_sf(crs = 4269) +
#   labs(title = "Chinook ESUs - Sum of difference between observed and estimataed states",
#        caption = "1980-2024, Solid color = Lower Columbia ESU | Striped color = Upper Willamette ESU") +
#   theme_minimal()
# chin_dif <- main_map + inset_element(inset_context_chin, 
#                                      left = 0.7, bottom = 0.05, 
#                                      right = 0.98, top = 0.3)
# # main_map <- ggplot(data = ESUchin) +
# #   annotation_map_tile(type = "hotstyle", zoom = 10) +
# #   geom_sf(aes(fill = total_net_difference), alpha = 0.8, color = "white", size = 0.1) + 
# #   geom_sf(data = sf_outlines_chin, fill = NA, color = "black", linewidth = 1.2) + 
# #   coord_sf(crs = 4269) +
# #   scale_fill_viridis_c(option = palette) + 
# #   labs(title = "Chinook ESUs - Sum of difference between observed\n and estimataed states",
# #        caption = "1980-2024",
# #        fill = "Difference") +
# #   theme_minimal()
# # chin_dif <- main_map + inset_element(inset_context_chin, 
# #                                      left = 0.85, bottom = 0.05, 
# #                                      right = 1.1, top = 0.3)
# chin_dif

# overlap - for chinook
poly_stripe <- st_union(sf_base, sf_stripe)
  # isolate polygons
sf_overlap_region <- st_intersection(sf_stripe, sf_base)
  # extract overlap
sf_non_overlap_region <- st_difference(ESUchin, sf_overlap_region)

# plots plots plots
main_map <- ggplot() +
  annotation_map_tile(type = "hotstyle", zoom = 10) +
  geom_sf(data = sf_base, aes(fill = total_net_difference), alpha = 0.8, color = "white", size = 0.1) +
  geom_sf(data = sf_non_overlap_region, aes(fill = total_net_difference), alpha = 0.8, color = "white", size = 0.1) +
  geom_sf_pattern(
    data = sf_overlap_region,
    aes(pattern_fill = total_net_difference), 
    pattern = 'stripe',
    pattern_color = NA,       
    pattern_density = 0.25,    
    pattern_spacing = 0.015,
    pattern_angle = 45,
    fill = NA,               
    alpha = 1                 
  ) +
  geom_sf(data = shared_borders, color = "black", linetype = "dashed", linewidth = 0.6) + 
  geom_sf(data = sf_outlines_chin, fill = NA, color = "black", linewidth = 1.2) +   
  scale_fill_viridis_c(
    option = palette, 
    aesthetics = c("fill", "pattern_fill"), 
    name = "Difference"
  ) +
  coord_sf(crs = 4269) +
  labs(
    title = "Chinook ESUs - Sum of difference between observed and estimated states",
    caption = "1980-2024, Solid color = Lower Columbia ESU | Striped color = Upper Willamette ESU"
  ) +
  theme_minimal()
ESUchin_dif <- main_map + inset_element(inset_context_chin, 
                                     left = 0.7, bottom = 0.05, 
                                     right = 0.98, top = 0.3)
ESUchin_dif

main_map <- ggplot(data = ESUcoho) +
  annotation_map_tile(type = "hotstyle", zoom = 10) +
  geom_sf(aes(fill = total_net_difference), alpha = 0.8, color = "white", size = 0.1) + 
  geom_sf(data = sf_outlines_coho, fill = NA, color = "black", linewidth = 1.2) + 
  coord_sf(crs = 4269) +
  scale_fill_viridis_c(option = palette) + 
  labs(title = "Coho ESUs - Sum of difference between observed\n and estimataed states",
       caption = "1980-2024",
       fill = "Difference") +
  theme_minimal()
ESUcoho_dif <- main_map + inset_element(inset_context_coho, 
                                     left = 0.85, bottom = 0.05, 
                                     right = 1.1, top = 0.3)
ESUcoho_dif

main_map <- ggplot(data = ESUstel) +
  annotation_map_tile(type = "hotstyle", zoom = 10) +
  geom_sf(aes(fill = total_net_difference), alpha = 0.8, color = "white", size = 0.1) + 
  geom_sf(data = sf_outlines_stel, fill = NA, color = "black", linewidth = 1.2) + 
  coord_sf(crs = 4269) +
  scale_fill_viridis_c(option = palette) + 
  labs(title = "Steelhead DSPs - Sum of difference between observed\n and estimataed states",
       caption = "1980-2024",
       fill = "Difference") +
  theme_minimal()
ESUstel_dif <- main_map + inset_element(inset_context_stel, 
                                     left = 0.85, bottom = 0.05, 
                                     right = 1.1, top = 0.3)
ESUstel_dif

main_map <- ggplot() +
  annotation_map_tile(type = "hotstyle", zoom = 10) +
  geom_sf(data = sf_base, aes(fill = avg_netdiff), alpha = 0.8, color = "white", size = 0.1) +
  geom_sf(data = sf_non_overlap_region, aes(fill = avg_netdiff), alpha = 0.8, color = "white", size = 0.1) +
  geom_sf_pattern(
    data = sf_overlap_region,
    aes(pattern_fill = avg_netdiff), 
    pattern = 'stripe',
    pattern_color = NA,       
    pattern_density = 0.25,    
    pattern_spacing = 0.015,
    pattern_angle = 45,
    fill = NA,               
    alpha = 1                 
  ) +
  geom_sf(data = shared_borders, color = "black", linetype = "dashed", linewidth = 0.6) + 
  geom_sf(data = sf_outlines_chin, fill = NA, color = "black", linewidth = 1.2) +   
  scale_fill_viridis_c(
    option = palette, 
    aesthetics = c("fill", "pattern_fill"), 
    name = "Difference - \nln(NOSA)"
  ) +
  coord_sf(crs = 4269) +
  labs(
    title = "Chinook ESUs - Average annual difference between observed and estimated states",
    caption = "1980-2024, Solid color = Lower Columbia ESU | Striped color = Upper Willamette ESU"
  ) +
  theme_minimal()
ESUchin_avg <- main_map + inset_element(inset_context_chin, 
                                     left = 0.7, bottom = 0.05, 
                                     right = 0.98, top = 0.3)
ESUchin_avg

main_map <- ggplot(data = ESUcoho) +
  annotation_map_tile(type = "hotstyle", zoom = 10) +
  geom_sf(aes(fill = avg_netdiff), alpha = 0.8, color = "white", size = 0.1) + 
  geom_sf(data = sf_outlines_coho, fill = NA, color = "black", linewidth = 1.2) + 
  coord_sf(crs = 4269) +
  scale_fill_viridis_c(option = palette) + 
  labs(title = "Coho ESUs - Average annual difference between observed\n and estimataed states",
       caption = "1980-2024",
       fill = "Difference - \nln(NOSA)") +
  theme_minimal()
ESUcoho_avg <- main_map + inset_element(inset_context_coho, 
                                     left = 0.85, bottom = 0.05, 
                                     right = 1.1, top = 0.3)
ESUcoho_avg

main_map <- ggplot(data = ESUstel) +
  annotation_map_tile(type = "hotstyle", zoom = 10) +
  geom_sf(aes(fill = avg_netdiff), alpha = 0.8, color = "white", size = 0.1) + 
  geom_sf(data = sf_outlines_stel, fill = NA, color = "black", linewidth = 1.2) + 
  coord_sf(crs = 4269) +
  scale_fill_viridis_c(option = palette) + 
  labs(title = "Steelhead DSPs - Average annual difference between observed\n and estimataed states",
       caption = "1980-2024",
       fill = "Difference - \nln(NOSA)") +
  theme_minimal()
ESUstel_avg <- main_map + inset_element(inset_context_stel, 
                                     left = 0.85, bottom = 0.05, 
                                     right = 1.1, top = 0.3)
ESUstel_avg
