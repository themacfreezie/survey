# ## SET WORKING DIR & PACKAGES
# library(biscale)
# library(cowplot)
# library(ggpattern)
# library(ggspatial)
# library(here)
# library(pals)
# library(patchwork)
# library(prettymapr)
# library(sf)
# library(stringr)
# library(tidyverse)
# library(tigris)
# 
# here::i_am("code/primary/06.4-multipanelFIRSTLAST.R")
# options(max.print=2000)
# 
# # pull in AR data
# ARchin <- readRDS(here("data", "clean", "FLavgAR_chin.rds"))
# ARcoho <- readRDS(here("data", "clean", "FLavgAR_coho.rds"))
# ARstel <- readRDS(here("data", "clean", "FLavgAR_stel.rds"))
# 
# ## chinook
# # pull in spatial layers
# gdb_path <- here("data", "raw", "WCR_Salmon_Steelhead_gdb_2015", "WCR_Salmon_Steelhead_gdb_2015.gdb")
# st_layers(gdb_path)
# # I want 'fish' datalayer
# sf_fish <- read_sf(dsn = gdb_path, layer = "fish")
# sf_fish$DPS_IDtrunc <- substr(sf_fish$DPS_ID, 1, 5)
# # grab DPS_ID
# sf_fish$DPStrunc <- str_remove(sf_fish$DPS, " - Outside legal area$")
# 
# ARchin <- ARchin %>%
#   filter(!is.na(NWFSC_POP_ID))
# 
# sf_fish_combined <- sf_fish %>%
#   left_join(ARchin, by = "NWFSC_POP_ID")
# 
# sf_chin <- sf_fish_combined %>%
#   filter(!is.na(first10_mean_a))
# 
# # make sure crs is good
# sf_chin_nad83 <- st_transform(sf_chin, crs = 4269)
# 
# # can this be collapsed?
# sf_chin_nad83col <- sf_chin_nad83 %>%
#   group_by(NWFSC_POP_ID, DPS_IDtrunc, DPStrunc) %>%
#   summarize(
#     first10mean_lnnosa = mean(first10_mean_lnnosa, na.rm = TRUE),
#     first10mean_a      = mean(first10_mean_a, na.rm = TRUE),
#     first10mean_R      = mean(first10_mean_R, na.rm = TRUE),
#     last10mean_lnnosa = mean(last10_mean_lnnosa, na.rm = TRUE),
#     last10mean_a      = mean(last10_mean_a, na.rm = TRUE),
#     last10mean_R      = mean(last10_mean_R, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# # Calculate % change from 'first10' to 'last10' variables
# sf_chin_nad83col <- sf_chin_nad83col%>%
#   mutate(
#     # Percentage format (e.g., 15.4 for a 15.4% increase)
#     change_a = last10mean_a - first10mean_a,
#     change_R = last10mean_R - first10mean_R,
#     pctchange_lnnosa = ((last10mean_lnnosa  - first10mean_lnnosa ) / first10mean_lnnosa ) * 100
#   )
# 
# # create ESU outlines
# sf_outlines <- sf_chin_nad83col %>%
#   group_by(DPS_IDtrunc, DPStrunc) %>%
#   summarize(SHAPE = st_union(SHAPE))
# 
# # esu outlines?
# outline_panels_clipped <- lapply(1:nrow(sf_outlines), function(i) {
#   focus_outline <- sf_outlines[i, ]
#   esu_data_clipped <- st_intersection(sf_chin_nad83col, focus_outline)
#   ggplot() +
#     geom_sf(data = esu_data_clipped, aes(fill = change_a), alpha = 0.7, color = "white", size = 0.1) +
#     geom_sf(data = focus_outline, fill = NA, color = "black", linewidth = 1.2) +
#     scale_fill_viridis_c(option = "inferno") +
#     labs(title = focus_outline$DPStrunc) +
#     theme_minimal() +
#     theme(
#       legend.position = "none",
#       plot.title = element_text(size = 10, face = "bold")
#     )
# })
# esu_panels_clipped <- wrap_plots(outline_panels_clipped, nrow = 3)
# esu_panels_clipped
# # I think it's 4 and 104
# 
# # preplots
# bbox <- st_bbox(sf_chin_nad83col)
# region_states <- states(cb = TRUE, resolution = "20m") %>%
#   filter(STUSPS %in% c("OR", "WA", "ID")) %>%
#   st_transform(4269) # match main map's CRS (NAD83)
# # these bounds roughly cover the columbia basin
# basin_xlim <- c(-125, -110)
# basin_ylim <- c(41.5, 49.5)
# 
# # create the basin-centered inset
# inset_context <- ggplot() +
#   geom_sf(data = region_states, fill = "gray95", color = "gray60", linewidth = 0.3) +
#   # red box representing your specific study area
#   annotate("rect", 
#            xmin = bbox["xmin"], xmax = bbox["xmax"], 
#            ymin = bbox["ymin"], ymax = bbox["ymax"], 
#            color = "red", fill = NA, linewidth = 0.8) +
#   # Crop the map to the Columbia River Basin extent
#   coord_sf(xlim = basin_xlim, ylim = basin_ylim, expand = FALSE) +
#   theme_void() +
#   theme(
#     panel.background = element_rect(fill = "white", color = "black"),
#     panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
#     plot.margin = margin(1, 1, 1, 1)
#   )
# 
# # overlap
# sf_base <- sf_chin_nad83col %>% filter(NWFSC_POP_ID != 104)
# sf_stripe <- sf_chin_nad83col %>% filter(NWFSC_POP_ID == 104)
# # stripe layer: ONLY population 104
# shared_borders <- st_intersection(sf_outlines) %>% 
#   filter(n.overlaps > 1) %>% 
#   st_cast("MULTILINESTRING")
# 
# # chin plot
# main_map <- ggplot() +
#   annotation_map_tile(type = "hotstyle", zoom = 10) +
#   geom_sf(data = sf_base, aes(fill = pctchange_lnnosa), alpha = 0.8, color = "white", size = 0.1) +
#   geom_sf_pattern(
#     data = sf_stripe,
#     aes(pattern_fill = pctchange_lnnosa), 
#     pattern = 'stripe',
#     pattern_color = NA,       # removes the default white border around stripes
#     pattern_density = 0.25,    # adjust for stripe thickness
#     pattern_spacing = 0.015,
#     pattern_angle = 45,
#     fill = NA,                # transparent fill so Pop 4's color shows between stripes
#     alpha = 1                 # keep stripes opaque to see their specific color clearly
#   ) +
#   geom_sf(data = shared_borders, color = "black", linetype = "dashed", linewidth = 0.6) + # Shared internal DPS borders (dashed)
#   geom_sf(data = sf_outlines, fill = NA, color = "black", linewidth = 1.2) +   # Standard DPS outlines (solid)
#   scale_fill_viridis_c(
#     option = "inferno", 
#     aesthetics = c("fill", "pattern_fill"), # Apply one scale to BOTH fill and pattern_fill
#     name = "Pct change"
#   ) +
#   coord_sf(crs = 4269) +
#   labs(title = "Chinook",
#        # title = "Percent change in population size - Chinook (1980-2024)",
#        caption = "Solid color = Lower Columbia ESU | Striped color = Upper Willamette ESU") +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(face = "bold", size = 28),
#     legend.title = element_text(size = 20),
#     legend.text = element_text(size = 18),
#     axis.text.x = element_text(size = 18, color = "black"),
#     axis.text.y= element_text(size = 18, color = "black"),
#   ) 
# chin_pop <- main_map + inset_element(inset_context, 
#                                      left = 0.7, bottom = 0.05, 
#                                      right = 0.98, top = 0.3)
# chin_pop
# 
# ## coho
# # pull in spatial layers
# gdb_path <- here("data", "raw", "WCR_Salmon_Steelhead_gdb_2015", "WCR_Salmon_Steelhead_gdb_2015.gdb")
# st_layers(gdb_path)
# # I want 'fish' datalayer
# sf_fish <- read_sf(dsn = gdb_path, layer = "fish")
# sf_fish$DPS_IDtrunc <- substr(sf_fish$DPS_ID, 1, 5)
# # grab DPS_ID
# sf_fish$DPStrunc <- str_remove(sf_fish$DPS, " - Outside legal area$")
# 
# ARcoho <- ARcoho %>%
#   filter(!is.na(NWFSC_POP_ID))
# 
# sf_fish_combined <- sf_fish %>%
#   left_join(ARcoho, by = "NWFSC_POP_ID")
# 
# sf_coho <- sf_fish_combined %>%
#   filter(!is.na(first10_mean_a))
# 
# # make sure crs is good
# sf_coho_nad83 <- st_transform(sf_coho, crs = 4269)
# 
# # can this be collapsed?
# sf_coho_nad83col <- sf_coho_nad83 %>%
#   group_by(NWFSC_POP_ID, DPS_IDtrunc, DPStrunc) %>%
#   summarize(
#     first10mean_lnnosa = mean(first10_mean_lnnosa, na.rm = TRUE),
#     first10mean_a      = mean(first10_mean_a, na.rm = TRUE),
#     first10mean_R      = mean(first10_mean_R, na.rm = TRUE),
#     last10mean_lnnosa = mean(last10_mean_lnnosa, na.rm = TRUE),
#     last10mean_a      = mean(last10_mean_a, na.rm = TRUE),
#     last10mean_R      = mean(last10_mean_R, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# # Calculate % change from 'first10' to 'last10' variables
# sf_coho_nad83col <- sf_coho_nad83col%>%
#   mutate(
#     # Percentage format (e.g., 15.4 for a 15.4% increase)
#     change_a = last10mean_a - first10mean_a,
#     change_R = last10mean_R - first10mean_R,
#     pctchange_lnnosa = ((last10mean_lnnosa  - first10mean_lnnosa ) / first10mean_lnnosa ) * 100
#   )
# 
# # create ESU outlines
# sf_outlines <- sf_coho_nad83col %>%
#   group_by(DPS_IDtrunc, DPStrunc) %>%
#   summarize(SHAPE = st_union(SHAPE))
# 
# # esu outlines?
# outline_panels_clipped <- lapply(1:nrow(sf_outlines), function(i) {
#   focus_outline <- sf_outlines[i, ]
#   esu_data_clipped <- st_intersection(sf_coho_nad83col, focus_outline)
#   ggplot() +
#     geom_sf(data = esu_data_clipped, aes(fill = change_a), alpha = 0.7, color = "white", size = 0.1) +
#     geom_sf(data = focus_outline, fill = NA, color = "black", linewidth = 1.2) +
#     scale_fill_viridis_c(option = "inferno") +
#     labs(title = focus_outline$DPStrunc) +
#     theme_minimal() +
#     theme(
#       legend.position = "none",
#       plot.title = element_text(size = 10, face = "bold")
#     )
# })
# esu_panels_clipped <- wrap_plots(outline_panels_clipped, nrow = 3)
# esu_panels_clipped
# # I think it's 4 and 104
# 
# # preplots
# bbox <- st_bbox(sf_coho_nad83col)
# region_states <- states(cb = TRUE, resolution = "20m") %>%
#   filter(STUSPS %in% c("OR", "WA", "ID")) %>%
#   st_transform(4269) # match main map's CRS (NAD83)
# # these bounds roughly cover the columbia basin
# basin_xlim <- c(-125, -110)
# basin_ylim <- c(41.5, 49.5)
# 
# # create the basin-centered inset
# inset_context <- ggplot() +
#   geom_sf(data = region_states, fill = "gray95", color = "gray60", linewidth = 0.3) +
#   # red box representing your specific study area
#   annotate("rect", 
#            xmin = bbox["xmin"], xmax = bbox["xmax"], 
#            ymin = bbox["ymin"], ymax = bbox["ymax"], 
#            color = "red", fill = NA, linewidth = 0.8) +
#   # Crop the map to the Columbia River Basin extent
#   coord_sf(xlim = basin_xlim, ylim = basin_ylim, expand = FALSE) +
#   theme_void() +
#   theme(
#     panel.background = element_rect(fill = "white", color = "black"),
#     panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
#     plot.margin = margin(1, 1, 1, 1)
#   )
# 
# # overlap
# sf_base <- sf_coho_nad83col %>% filter(NWFSC_POP_ID != 104)
# sf_stripe <- sf_coho_nad83col %>% filter(NWFSC_POP_ID == 104)
# # stripe layer: ONLY population 104
# shared_borders <- st_intersection(sf_outlines) %>% 
#   filter(n.overlaps > 1) %>% 
#   st_cast("MULTILINESTRING")
# 
# # coho plot
# main_map <- ggplot() +
#   annotation_map_tile(type = "hotstyle", zoom = 10) +
#   geom_sf(data = sf_base, aes(fill = pctchange_lnnosa), alpha = 0.8, color = "white", size = 0.1) +
#   geom_sf_pattern(
#     data = sf_stripe,
#     aes(pattern_fill = pctchange_lnnosa), 
#     pattern = 'stripe',
#     pattern_color = NA,       # removes the default white border around stripes
#     pattern_density = 0.25,    # adjust for stripe thickness
#     pattern_spacing = 0.015,
#     pattern_angle = 45,
#     fill = NA,                # transparent fill so Pop 4's color shows between stripes
#     alpha = 1                 # keep stripes opaque to see their specific color clearly
#   ) +
#   geom_sf(data = shared_borders, color = "black", linetype = "dashed", linewidth = 0.6) + # Shared internal DPS borders (dashed)
#   geom_sf(data = sf_outlines, fill = NA, color = "black", linewidth = 1.2) +   # Standard DPS outlines (solid)
#   scale_fill_viridis_c(
#     option = "inferno", 
#     aesthetics = c("fill", "pattern_fill"), # Apply one scale to BOTH fill and pattern_fill
#     name = "Pct change"
#   ) +
#   coord_sf(crs = 4269) +
#   labs(title = "Coho"
#        # title = "Percent change in population size - coho (1980-2024)",
#   ) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(face = "bold", size = 28),
#     legend.title = element_text(size = 20),
#     legend.text = element_text(size = 18),
#     axis.text.x = element_text(size = 18, color = "black"),
#     axis.text.y= element_text(size = 18, color = "black"),
#   ) 
# coho_pop <- main_map + inset_element(inset_context, 
#                                      left = 0.7, bottom = 0.05, 
#                                      right = 0.98, top = 0.3)
# coho_pop
# 
# ## steelhead
# # pull in spatial layers
# gdb_path <- here("data", "raw", "WCR_Salmon_Steelhead_gdb_2015", "WCR_Salmon_Steelhead_gdb_2015.gdb")
# st_layers(gdb_path)
# # I want 'fish' datalayer
# sf_fish <- read_sf(dsn = gdb_path, layer = "fish")
# sf_fish$DPS_IDtrunc <- substr(sf_fish$DPS_ID, 1, 5)
# # grab DPS_ID
# sf_fish$DPStrunc <- str_remove(sf_fish$DPS, " - Outside legal area$")
# 
# 
# ###### steelhead
# ARstel <- ARstel %>%
#   filter(!is.na(NWFSC_POP_ID))
# 
# sf_fish_combined <- sf_fish %>%
#   left_join(ARstel, by = "NWFSC_POP_ID")
# 
# sf_stel <- sf_fish_combined %>%
#   filter(!is.na(first10_mean_a))
# 
# # make sure crs is good
# sf_stel_nad83 <- st_transform(sf_stel, crs = 4269)
# 
# # can this be collapsed?
# sf_stel_nad83col <- sf_stel_nad83 %>%
#   group_by(NWFSC_POP_ID, DPS_IDtrunc, DPStrunc) %>%
#   summarize(
#     first10mean_lnnosa = mean(first10_mean_lnnosa, na.rm = TRUE),
#     first10mean_a      = mean(first10_mean_a, na.rm = TRUE),
#     first10mean_R      = mean(first10_mean_R, na.rm = TRUE),
#     last10mean_lnnosa = mean(last10_mean_lnnosa, na.rm = TRUE),
#     last10mean_a      = mean(last10_mean_a, na.rm = TRUE),
#     last10mean_R      = mean(last10_mean_R, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# # Calculate % change from 'first10' to 'last10' variables
# sf_stel_nad83col <- sf_stel_nad83col%>%
#   mutate(
#     # Percentage format (e.g., 15.4 for a 15.4% increase)
#     change_a = last10mean_a - first10mean_a,
#     change_R = last10mean_R - first10mean_R,
#     pctchange_lnnosa = ((last10mean_lnnosa  - first10mean_lnnosa ) / first10mean_lnnosa ) * 100
#   )
# 
# # create ESU outlines
# sf_outlines <- sf_stel_nad83col %>%
#   group_by(DPS_IDtrunc, DPStrunc) %>%
#   summarize(SHAPE = st_union(SHAPE))
# 
# # esu outlines?
# outline_panels_clipped <- lapply(1:nrow(sf_outlines), function(i) {
#   focus_outline <- sf_outlines[i, ]
#   esu_data_clipped <- st_intersection(sf_stel_nad83col, focus_outline)
#   ggplot() +
#     geom_sf(data = esu_data_clipped, aes(fill = change_a), alpha = 0.7, color = "white", size = 0.1) +
#     geom_sf(data = focus_outline, fill = NA, color = "black", linewidth = 1.2) +
#     scale_fill_viridis_c(option = "inferno") +
#     labs(title = focus_outline$DPStrunc) +
#     theme_minimal() +
#     theme(
#       legend.position = "none",
#       plot.title = element_text(size = 10, face = "bold")
#     )
# })
# esu_panels_clipped <- wrap_plots(outline_panels_clipped, nrow = 3)
# esu_panels_clipped
# # I think it's 4 and 104
# 
# # preplots
# bbox <- st_bbox(sf_stel_nad83col)
# region_states <- states(cb = TRUE, resolution = "20m") %>%
#   filter(STUSPS %in% c("OR", "WA", "ID")) %>%
#   st_transform(4269) # match main map's CRS (NAD83)
# # these bounds roughly cover the columbia basin
# basin_xlim <- c(-125, -110)
# basin_ylim <- c(41.5, 49.5)
# 
# # create the basin-centered inset
# inset_context <- ggplot() +
#   geom_sf(data = region_states, fill = "gray95", color = "gray60", linewidth = 0.3) +
#   # red box representing your specific study area
#   annotate("rect", 
#            xmin = bbox["xmin"], xmax = bbox["xmax"], 
#            ymin = bbox["ymin"], ymax = bbox["ymax"], 
#            color = "red", fill = NA, linewidth = 0.8) +
#   # Crop the map to the Columbia River Basin extent
#   coord_sf(xlim = basin_xlim, ylim = basin_ylim, expand = FALSE) +
#   theme_void() +
#   theme(
#     panel.background = element_rect(fill = "white", color = "black"),
#     panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
#     plot.margin = margin(1, 1, 1, 1)
#   )
# 
# # overlap
# sf_base   <- sf_stel_nad83col %>% filter(NWFSC_POP_ID != 104)
# suppressWarnings({
#   intersections <- st_intersection(sf_outlines, sf_outlines)
# })
# shared_borders <- intersections %>%
#   filter(DPS_IDtrunc != DPS_IDtrunc.1) %>%
#   filter(st_geometry_type(SHAPE) %in% c("POLYGON", "MULTIPOLYGON")) %>%
#   st_boundary() %>%
#   st_union()
# 
# # stel plot
# main_map <- ggplot() +
#   annotation_map_tile(type = "hotstyle", zoom = 10) +
#   geom_sf(data = sf_base, aes(fill = pctchange_lnnosa), alpha = 0.8, color = "white", size = 0.1) +
#   geom_sf_pattern(
#     data = sf_stripe,
#     aes(pattern_fill = pctchange_lnnosa), 
#     pattern = 'stripe',
#     pattern_color = NA,       # removes the default white border around stripes
#     pattern_density = 0.25,    # adjust for stripe thickness
#     pattern_spacing = 0.015,
#     pattern_angle = 45,
#     fill = NA,                # transparent fill so Pop 4's color shows between stripes
#     alpha = 1                 # keep stripes opaque to see their specific color clearly
#   ) +
#   geom_sf(data = shared_borders, color = "black", linetype = "dashed", linewidth = 0.6) + # Shared internal DPS borders (dashed)
#   geom_sf(data = sf_outlines, fill = NA, color = "black", linewidth = 1.2) +   # Standard DPS outlines (solid)
#   scale_fill_viridis_c(
#     option = "inferno", 
#     aesthetics = c("fill", "pattern_fill"), # Apply one scale to BOTH fill and pattern_fill
#     name = "Pct change"
#   ) +
#   coord_sf(crs = 4269) +
#   labs(title = "Steelhead") +
#   # title = "Percent change in population size - steelhead (1980-2024)",
#   theme_minimal() +
#   theme(
#     plot.title = element_text(face = "bold", size = 28),
#     legend.title = element_text(size = 20),
#     legend.text = element_text(size = 18),
#     axis.text.x = element_text(size = 18, color = "black"),
#     axis.text.y= element_text(size = 18, color = "black"),
#   ) 
# stel_pop <- main_map + inset_element(inset_context, 
#                                      left = 0.7, bottom = 0.05, 
#                                      right = 0.98, top = 0.3)
# stel_pop

## SET WORKING DIR & PACKAGES
library(biscale)
library(cowplot)
library(ggpattern)
library(ggspatial)
library(here)
library(pals)
library(patchwork)
library(prettymapr)
library(sf)
library(stringr)
library(tidyverse)
library(tigris)

here::i_am("code/primary/06.4-multipanelFIRSTLAST.R")
options(max.print = 2000)

# ==========================================
# 1. LOAD DATA & SPATIAL LAYERS
# ==========================================

# set palette
palette <- "viridis"

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
  Chinook   = here("data", "clean", "FLavgAR_chin.rds"),
  Coho      = here("data", "clean", "FLavgAR_coho.rds"),
  Steelhead = here("data", "clean", "FLavgAR_stel.rds")
)

# Background Regional Context Map
region_states <- states(cb = TRUE, resolution = "20m") %>%
  filter(STUSPS %in% c("OR", "WA", "ID")) %>%
  st_transform(4269)

basin_xlim <- c(-125, -110)
basin_ylim <- c(41.5, 49.5)

# ==========================================
# 2. PROCESSING PIPELINE FUNCTION
# ==========================================

process_species_data <- function(file_path) {
  ar_data <- readRDS(file_path) %>% filter(!is.na(NWFSC_POP_ID))
  
  sf_combined <- sf_fish %>% 
    left_join(ar_data, by = "NWFSC_POP_ID") %>%
    filter(!is.na(first10_mean_a)) %>%
    st_transform(crs = 4269)
  
  sf_collapsed <- sf_combined %>%
    group_by(NWFSC_POP_ID, DPS_IDtrunc, DPStrunc) %>%
    summarize(
      first10mean_lnnosa = mean(first10_mean_lnnosa, na.rm = TRUE),
      first10mean_a      = mean(first10_mean_a, na.rm = TRUE),
      first10mean_R      = mean(first10_mean_R, na.rm = TRUE),
      last10mean_lnnosa  = mean(last10_mean_lnnosa, na.rm = TRUE),
      last10mean_a       = mean(last10_mean_a, na.rm = TRUE),
      last10mean_R       = mean(last10_mean_R, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      change_a        = last10mean_a - first10mean_a,
      change_R        = last10mean_R - first10mean_R,
      pctchange_lnnosa = ((last10mean_lnnosa - first10mean_lnnosa) / first10mean_lnnosa) * 100
    )
  
  return(sf_collapsed)
}

# Process all three data sets into a structured list
processed_data <- map(species_list, process_species_data)

# ==========================================
# 3. UNIFY COLOR SCALE (CRUCIAL STEP)
# ==========================================

# Bind values together to detect absolute global limits across all datasets
global_range <- map_dfr(processed_data, ~ as_tibble(.x) %>% select(pctchange_lnnosa)) %>%
  pull(pctchange_lnnosa) %>%
  range(na.rm = TRUE)

# ==========================================
# 4. PLOTTING FUNCTION FOR SUBPANELS
# ==========================================

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
    scale_fill_viridis_c(
      option = palette,
      aesthetics = c("fill", "pattern_fill"),
      name = "% Change",
      limits = limits # Locks identical scales across plots
    ) +
    coord_sf(crs = 4269) +
    labs(title = title_text) +
    theme_minimal() +
    theme(
      plot.title   = element_text(face = "bold", size = 20),
      axis.text.x  = element_text(size = 10, color = "black"),
      axis.text.y  = element_text(size = 10, color = "black"),
      legend.position = "none" # Suppress subpanel legends for clean layout
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

# Extract a shared legend using a dummy plot setup
legend_plot <- ggplot(processed_data$Chinook) +
  geom_sf(aes(fill = pctchange_lnnosa)) +
  scale_fill_viridis_c(
    option = palette, 
    name = "Percent Change\nPopulation Size\n(1980-2024)",
    limits = global_range
  ) +
  theme_minimal() +
  theme(
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 12),
    legend.key.height = unit(1.5, "cm")
  )
shared_legend <- cowplot::get_legend(legend_plot)

# Compile visual canvas (3 Maps side-by-side + 1 unified legend space)
multipanel_layout <- ((p1 / p3) |  p2) + 
  plot_layout(guides = "keep", widths = c(2.5, 1.5)) 

# Append unified title, captioning, and the shared legend block
final_output <- rowplot <- plot_grid(
  multipanel_layout, 
  shared_legend, 
  rel_widths = c(10, 1.5), 
  nrow = 1
)

# Render complete layout
final_output
