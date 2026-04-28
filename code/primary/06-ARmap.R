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

here::i_am("code/primary/06-ARmap.R")
options(max.print=2000)

# pull in AR data
ARchin <- readRDS(here("data", "clean", "popavgAR_chin.rds"))
ARcoho <- readRDS(here("data", "clean", "popavgAR_coho.rds"))
ARstel <- readRDS(here("data", "clean", "popavgAR_stel.rds"))

# pull in spatial layers
gdb_path <- here("data", "raw", "WCR_Salmon_Steelhead_gdb_2015", "WCR_Salmon_Steelhead_gdb_2015.gdb")
st_layers(gdb_path)
  # I want 'fish' datalayer
sf_fish <- read_sf(dsn = gdb_path, layer = "fish")
sf_fish$DPS_IDtrunc <- substr(sf_fish$DPS_ID, 1, 5)
  # grab DPS_ID
sf_fish$DPStrunc <- str_remove(sf_fish$DPS, " - Outside legal area$")

# coho
sf_fish_combined <- sf_fish %>%
  left_join(ARcoho, by = "NWFSC_POP_ID")

sf_coho <- sf_fish_combined %>%
  filter(!is.na(mean_a))

# test plot
sf_coho_nad83 <- st_transform(sf_coho, crs = 4269)

# ggplot(data = sf_coho_nad83) +
#   annotation_map_tile(type = "hotstyle", zoom = 10) + 
#   geom_sf(aes(fill = mean_a), alpha = 0.7) + 
#   coord_sf(crs = 4269) + 
#   scale_fill_viridis_c(option = "inferno") + # Provides a high-contrast, accessible gradient
#   labs(title = "Map of bias in coho populations",
#        fill = "Mean bias") +
#   theme_minimal()
# 
# ggplot(data = sf_coho_nad83) +
#   annotation_map_tile(type = "hotstyle", zoom = 10) + 
#   geom_sf(aes(fill = mean_R), alpha = 0.7) + 
#   coord_sf(crs = 4269) + 
#   scale_fill_viridis_c(option = "inferno") + # Provides a high-contrast, accessible gradient
#   labs(title = "Map of precision in coho populations",
#        fill = "Mean variance") +
#   theme_minimal()

# # can we make these contiguous?
# contiguity_test <- sf_coho_nad83 %>%
#   group_by(NWFSC_POP_ID) %>%
#   summarize(geometry = st_union(SHAPE)) %>%
#   mutate(
#     # break multipolygons into individual polygons and count them
#     piece_count = lengths(st_cast(geometry, "POLYGON", warn = FALSE)),
#     is_contiguous = piece_count == 1
#   )
# 
# # view groups that are NOT contiguous
# non_contiguous <- filter(contiguity_test, !is_contiguous)
# print(non_contiguous)
#   # it looks as though they are all contiguous..

# can this be collapsed?
sf_coho_nad83col <- sf_coho_nad83 %>%
  group_by(NWFSC_POP_ID, DPS_IDtrunc, DPStrunc) %>%
  summarize(
    mean_lnnosa = mean(mean_lnnosa, na.rm = TRUE),
    mean_a      = mean(mean_a, na.rm = TRUE),
    mean_R      = mean(mean_R, na.rm = TRUE),
    .groups = "drop"
  )

# preplots
bbox <- st_bbox(sf_coho_nad83col)
region_states <- states(cb = TRUE, resolution = "20m") %>%
  filter(STUSPS %in% c("OR", "WA")) %>%
  st_transform(4269) # match main map's CRS (NAD83)
# these bounds roughly cover the columbia basin
basin_xlim <- c(-125.0, -116.0)
basin_ylim <- c(41.5, 49.5)

# create the basin-centered inset
inset_context <- ggplot() +
  geom_sf(data = region_states, fill = "gray95", color = "gray60", linewidth = 0.3) +
  # red box representing your specific study area
  annotate("rect", 
           xmin = bbox["xmin"], xmax = bbox["xmax"], 
           ymin = bbox["ymin"], ymax = bbox["ymax"], 
           color = "red", fill = NA, linewidth = 0.8) +
  # Crop the map to the Columbia River Basin extent
  coord_sf(xlim = basin_xlim, ylim = basin_ylim, expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    plot.margin = margin(1, 1, 1, 1)
  )

# create ESU outlines
sf_outlines <- sf_coho_nad83col %>%
  group_by(DPS_IDtrunc, DPStrunc) %>%
  summarize(SHAPE = st_union(SHAPE))

# check for empty geometries
# sf_coho_nad83col <- sf_coho_nad83col[!st_is_empty(sf_coho_nad83col), ]

# plotting
main_map <- ggplot(data = sf_coho_nad83col) +
  annotation_map_tile(type = "hotstyle", zoom = 10) +
  geom_sf(aes(fill = mean_a), alpha = 0.7) + 
  geom_sf(data = sf_outlines, fill = NA, color = "black", linewidth = 1.2) + 
  coord_sf(crs = 4269) +
  scale_fill_viridis_c(option = "inferno") + 
  labs(title = "Average bias - coho surveys (1980-2024)",
       caption = "Bias measured relative to 'Dam Counts' method",
       fill = "Bias") +
  theme_minimal()
coho_a <- main_map + inset_element(inset_context, 
                                   left = 0.85, bottom = 0.05, 
                                   right = 1.1, top = 0.3)
coho_a
ggsave(here("output", "figures", "coho_a.png"), plot=coho_a, device="png", dpi=300)

main_map <- ggplot(data = sf_coho_nad83col) +
  annotation_map_tile(type = "hotstyle", zoom = 10) + 
  geom_sf(aes(fill = mean_R), alpha = 0.7) + 
  geom_sf(data = sf_outlines, fill = NA, color = "black", linewidth = 1.2) + 
  coord_sf(crs = 4269) + 
  scale_fill_viridis_c(option = "inferno") + 
  labs(title = "Average precision - coho surveys (1980-2024)",
       fill = "Variance") +
  theme_minimal()
coho_r <- main_map + inset_element(inset_context, 
                                   left = 0.85, bottom = 0.05, 
                                   right = 1.1, top = 0.3)
coho_r
ggsave(here("output", "figures", "coho_r.png"), plot=coho_r, device="png", dpi=300)

main_map <- ggplot(data = sf_coho_nad83col) +
  annotation_map_tile(type = "hotstyle", zoom = 10) + 
  geom_sf(aes(fill = mean_lnnosa), alpha = 0.7) + 
  geom_sf(data = sf_outlines, fill = NA, color = "black", linewidth = 1.2) + 
  coord_sf(crs = 4269) + 
  scale_fill_viridis_c(option = "inferno") + 
  labs(title = "Average population size - coho (1980-2024)",
       fill = "pop. size") +
  theme_minimal()
coho_pop <- main_map + inset_element(inset_context, 
                                     left = 0.85, bottom = 0.05, 
                                     right = 1.1, top = 0.3)
coho_pop
ggsave(here("output", "figures", "coho_pop.png"), plot=coho_pop, device="png", dpi=300)

# choropleth
data <- bi_class(sf_coho_nad83col, x = mean_lnnosa, y = mean_R, style = "equal", dim = 4)
# bi_class creates a new 'bi_class' column based on quantiles of two variables
map <- ggplot() +
  annotation_map_tile(
    type = "hotstyle",
    zoom = 10
  ) + 
  geom_sf(data, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  geom_sf(data = sf_outlines, fill = NA, color = "black", linewidth = 1.2) + 
  bi_scale_fill(pal = "Brown2", dim = 4) + # Choose a built-in bivariate palette
  bi_theme()
legend <- bi_legend(pal = "Brown2",
                    dim = 4,
                    xlab = "Population",
                    ylab = "Variance",
                    size = 8) +
  theme(plot.background = element_rect(color = "black", fill = "white", linewidth = 1))
final_plot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.7, 0.35, 0.2, 0.2) # Adjust coordinates and size as needed
coho_ARchoro <- final_plot + inset_element(inset_context, 
                                           left = 0.7, bottom = 0.05, 
                                           right = 0.98, top = 0.3)
coho_ARchoro
ggsave(here("output", "figures", "coho_ARchoro.png"), plot=coho_ARchoro, device="png", dpi=300)

# getting weird... iterated choropleths by esu
outline_ids <- unique(sf_outlines$DPS_IDtrunc) 
plot_list <- lapply(1:nrow(sf_outlines), function(i) {
  
  # select the single focus polygon
  focus_polygon <- sf_outlines[i, ]
  
  # extract the title for this specific iteration
  current_title <- focus_polygon$DPStrunc
  
  # "cookie cut" the data to the focus polygon boundary
  # this removes all data outside the outline and clips bordering polygons
  focus_data_clipped <- st_intersection(data, focus_polygon)
  
  # build the map
  p <- ggplot() +
    annotation_map_tile(type = "hotstyle", zoom = 10) +
    # Background: Full muted choropleth
    geom_sf(data = data, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
    # Shroud: Semi-opaque white layer
    geom_sf(data = st_union(data), fill = "white", alpha = 0.7, color = NA) +
    # Highlight: Clipped data only
    geom_sf(data = focus_data_clipped, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
    # Outline: Crisp black border
    geom_sf(data = focus_polygon, fill = NA, color = "black", linewidth = 1.2) +
    bi_scale_fill(pal = "Brown2", dim = 4) +
    bi_theme() +
    labs(title = current_title) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))
  
  
  # combine with legend
  ggdraw() + 
    draw_plot(p, 0, 0, 1, 1) + 
    draw_plot(legend, 0.005, 0.65, 0.25, 0.25)
})

doink <- wrap_plots(plot_list, ncol = 2)
coho_ARchoro_panel <- doink + inset_element(inset_context, 
                                            left = 0.7, bottom = 0.05, 
                                            right = 0.98, top = 0.3)
coho_ARchoro_panel
ggsave(here("output", "figures", "coho_ARchoro_panel.png"), plot=coho_ARchoro_panel, device="png", dpi=300)


###### chinook
ARchin <- ARchin %>%
  filter(!is.na(NWFSC_POP_ID))

sf_fish_combined <- sf_fish %>%
  left_join(ARchin, by = "NWFSC_POP_ID")

sf_chin <- sf_fish_combined %>%
  filter(!is.na(mean_a))

# make sure crs is good
sf_chin_nad83 <- st_transform(sf_chin, crs = 4269)

# # can we make these contiguous?
# contiguity_test <- sf_chin_nad83 %>%
#   group_by(NWFSC_POP_ID) %>%
#   summarize(geometry = st_union(SHAPE)) %>%
#   mutate(
#     # break multipolygons into individual polygons and count them
#     piece_count = lengths(st_cast(geometry, "POLYGON", warn = FALSE)),
#     is_contiguous = piece_count == 1
#   )
# 
# # view groups that are NOT contiguous
# non_contiguous <- filter(contiguity_test, !is_contiguous)
# print(non_contiguous)
#   # it looks as though they are all contiguous..

# can this be collapsed?
sf_chin_nad83col <- sf_chin_nad83 %>%
  group_by(NWFSC_POP_ID, DPS_IDtrunc, DPStrunc) %>%
  summarize(
    mean_lnnosa = mean(mean_lnnosa, na.rm = TRUE),
    mean_a      = mean(mean_a, na.rm = TRUE),
    mean_R      = mean(mean_R, na.rm = TRUE),
    .groups = "drop"
  )

# create ESU outlines
sf_outlines <- sf_chin_nad83col %>%
  group_by(DPS_IDtrunc, DPStrunc) %>%
  summarize(SHAPE = st_union(SHAPE))

# esu outlines?
outline_panels_clipped <- lapply(1:nrow(sf_outlines), function(i) {
  focus_outline <- sf_outlines[i, ]
  esu_data_clipped <- st_intersection(sf_chin_nad83col, focus_outline)
  ggplot() +
    geom_sf(data = esu_data_clipped, aes(fill = mean_a), alpha = 0.7, color = "white", size = 0.1) +
    geom_sf(data = focus_outline, fill = NA, color = "black", linewidth = 1.2) +
    scale_fill_viridis_c(option = "inferno") +
    labs(title = focus_outline$DPStrunc) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 10, face = "bold")
    )
})
esu_panels_clipped <- wrap_plots(outline_panels_clipped, nrow = 3)
esu_panels_clipped
  # I think it's 4 and 104

# preplots
bbox <- st_bbox(sf_chin_nad83col)
region_states <- states(cb = TRUE, resolution = "20m") %>%
  filter(STUSPS %in% c("OR", "WA", "ID")) %>%
  st_transform(4269) # match main map's CRS (NAD83)
# these bounds roughly cover the columbia basin
basin_xlim <- c(-125, -110)
basin_ylim <- c(41.5, 49.5)

# create the basin-centered inset
inset_context <- ggplot() +
  geom_sf(data = region_states, fill = "gray95", color = "gray60", linewidth = 0.3) +
  # red box representing your specific study area
  annotate("rect", 
           xmin = bbox["xmin"], xmax = bbox["xmax"], 
           ymin = bbox["ymin"], ymax = bbox["ymax"], 
           color = "red", fill = NA, linewidth = 0.8) +
  # Crop the map to the Columbia River Basin extent
  coord_sf(xlim = basin_xlim, ylim = basin_ylim, expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    plot.margin = margin(1, 1, 1, 1)
  )

# overlap
sf_base <- sf_chin_nad83col %>% filter(NWFSC_POP_ID != 104)
sf_stripe <- sf_chin_nad83col %>% filter(NWFSC_POP_ID == 104)
  # stripe layer: ONLY population 104
shared_borders <- st_intersection(sf_outlines) %>% 
  filter(n.overlaps > 1) %>% 
  st_cast("MULTILINESTRING")

# plotting
# main_map <- ggplot(data = sf_chin_nad83col) +
#   annotation_map_tile(type = "hotstyle", zoom = 10) + 
#   geom_sf(aes(fill = mean_a), alpha = 0.7) + 
#   geom_sf(data = sf_outlines, fill = NA, color = "black", linewidth = 1.2) + 
#   coord_sf(crs = 4269) + 
#   scale_fill_viridis_c(option = "inferno") + 
#   labs(title = "Map of relative bias in chin populations",
#        fill = "Mean relative bias") +
#   theme_minimal()
# chin_a <- main_map + inset_element(inset_context, 
#                                    left = 0.7, bottom = 0.05, 
#                                    right = 0.98, top = 0.3)
# chin_a
main_map <- ggplot() +
  annotation_map_tile(type = "hotstyle", zoom = 10) +
  geom_sf(data = sf_base, aes(fill = mean_a), alpha = 0.8, color = "white", size = 0.1) +
  geom_sf_pattern(
    data = sf_stripe,
    aes(pattern_fill = mean_a), 
    pattern = 'stripe',
    pattern_color = NA,       # removes the default white border around stripes
    pattern_density = 0.25,    # adjust for stripe thickness
    pattern_spacing = 0.015,
    pattern_angle = 45,
    fill = NA,                # transparent fill so Pop 4's color shows between stripes
    alpha = 1                 # keep stripes opaque to see their specific color clearly
  ) +
  geom_sf(data = shared_borders, color = "black", linetype = "dashed", linewidth = 0.6) + # Shared internal DPS borders (dashed)
  geom_sf(data = sf_outlines, fill = NA, color = "black", linewidth = 1.2) +   # Standard DPS outlines (solid)
  scale_fill_viridis_c(
    option = "inferno", 
    aesthetics = c("fill", "pattern_fill"), # Apply one scale to BOTH fill and pattern_fill
    name = "Bias"
  ) +
  coord_sf(crs = 4269) +
  labs(title = "Average bias - Chinook surveys (1980-2024)",
       caption = "Bias measured relative to 'Dam Counts' method, Solid color = Lower Columbia ESU | Striped color = Upper Willamette ESU") +
  theme_minimal()
chin_a <- main_map + inset_element(inset_context, 
                                   left = 0.7, bottom = 0.05, 
                                   right = 0.98, top = 0.3)
chin_a
ggsave(here("output", "figures", "chin_a.png"), plot=chin_a, device="png", dpi=300)

main_map <- ggplot() +
  annotation_map_tile(type = "hotstyle", zoom = 10) +
  geom_sf(data = sf_base, aes(fill = mean_R), alpha = 0.8, color = "white", size = 0.1) +
  geom_sf_pattern(
    data = sf_stripe,
    aes(pattern_fill = mean_R), 
    pattern = 'stripe',
    pattern_color = NA,       # removes the default white border around stripes
    pattern_density = 0.25,    # adjust for stripe thickness
    pattern_spacing = 0.015,
    pattern_angle = 45,
    fill = NA,                # transparent fill so Pop 4's color shows between stripes
    alpha = 1                 # keep stripes opaque to see their specific color clearly
  ) +
  geom_sf(data = shared_borders, color = "black", linetype = "dashed", linewidth = 0.6) + # Shared internal DPS borders (dashed)
  geom_sf(data = sf_outlines, fill = NA, color = "black", linewidth = 1.2) +   # Standard DPS outlines (solid)
  scale_fill_viridis_c(
    option = "inferno", 
    aesthetics = c("fill", "pattern_fill"), # Apply one scale to BOTH fill and pattern_fill
    name = "Variance"
  ) +
  coord_sf(crs = 4269) +
  labs(title = "Average precision - Chinook surveys (1980-2024)",
       caption = "Solid color = Lower Columbia ESU | Striped color = Upper Willamette ESU") +
  theme_minimal()
chin_r <- main_map + inset_element(inset_context, 
                                   left = 0.7, bottom = 0.05, 
                                   right = 0.98, top = 0.3)
chin_r
ggsave(here("output", "figures", "chin_r.png"), plot=chin_r, device="png", dpi=300)

main_map <- ggplot() +
  annotation_map_tile(type = "hotstyle", zoom = 10) +
  geom_sf(data = sf_base, aes(fill = mean_lnnosa), alpha = 0.8, color = "white", size = 0.1) +
  geom_sf_pattern(
    data = sf_stripe,
    aes(pattern_fill = mean_lnnosa), 
    pattern = 'stripe',
    pattern_color = NA,       # removes the default white border around stripes
    pattern_density = 0.25,    # adjust for stripe thickness
    pattern_spacing = 0.015,
    pattern_angle = 45,
    fill = NA,                # transparent fill so Pop 4's color shows between stripes
    alpha = 1                 # keep stripes opaque to see their specific color clearly
  ) +
  geom_sf(data = shared_borders, color = "black", linetype = "dashed", linewidth = 0.6) + # Shared internal DPS borders (dashed)
  geom_sf(data = sf_outlines, fill = NA, color = "black", linewidth = 1.2) +   # Standard DPS outlines (solid)
  scale_fill_viridis_c(
    option = "inferno", 
    aesthetics = c("fill", "pattern_fill"), # Apply one scale to BOTH fill and pattern_fill
    name = "Pop. size"
  ) +
  coord_sf(crs = 4269) +
  labs(title = "Average population size - Chinook (1980-2024)",
       caption = "Solid color = Lower Columbia ESU | Striped color = Upper Willamette ESU") +
  theme_minimal()
chin_pop <- main_map + inset_element(inset_context, 
                                   left = 0.7, bottom = 0.05, 
                                   right = 0.98, top = 0.3)
chin_pop
ggsave(here("output", "figures", "chin_pop.png"), plot=chin_pop, device="png", dpi=300)

# # choropleth
data <- bi_class(sf_chin_nad83col, x = mean_lnnosa, y = mean_R, style = "equal", dim = 4)
  # bi_class creates a new 'bi_class' column based on quantiles of two variables

# map <- ggplot() +
#   annotation_map_tile(
#     type = "hotstyle",
#     zoom = 10
#   ) + 
#   geom_sf(data, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
#   geom_sf(data = sf_outlines, fill = NA, color = "black", linewidth = 1.2) + 
#   bi_scale_fill(pal = "Brown2", dim = 4) + # Choose a built-in bivariate palette
#   bi_theme()
legend <- bi_legend(pal = "Brown2",
                    dim = 4,
                    xlab = "Population",
                    ylab = "Variance",
                    size = 8) +
  theme(plot.background = element_rect(color = "black", fill = "white", linewidth = 1))
# final_plot <- ggdraw() +
#   draw_plot(map, 0, 0, 1, 1) +
#   draw_plot(legend, 0.5, 0.05, 0.2, 0.2) # Adjust coordinates and size as needed
# chin_ARchoro <- final_plot + inset_element(inset_context, 
#                                            left = 0.7, bottom = 0.05, 
#                                            right = 0.98, top = 0.3)
# chin_ARchoro
# ggsave(here("output", "figures", "chin_ARchoro.png"), plot=chin_ARchoro, device="png", dpi=300)
  # got to apply the stripey thing to the choropleth

# getting weird... iterated choropleths by esu
outline_ids <- unique(sf_outlines$DPS_IDtrunc) 
# plot_list <- lapply(1:nrow(sf_outlines), function(i) {
#   # 1. Select the focus polygon
#   focus_polygon <- sf_outlines[i, ]
#   current_title <- focus_polygon$DPStrunc
#   current_dps_id <- focus_polygon$DPS_IDtrunc # Grab the ID (e.g., "CKLCR")
#   
#   # 2. "Cookie cut" AND Filter by DPS_ID
#   # This ensures CKLCR panel only shows ID 4 and CKUWR only shows ID 104
#   focus_data_clipped <- st_intersection(data, focus_polygon) %>%
#     filter(DPS_IDtrunc == current_dps_id)
#   
#   # 3. Build the map
#   p <- ggplot() + 
#     annotation_map_tile(type = "hotstyle", zoom = 10) +
#     # Background: Full muted choropleth
#     geom_sf(data = data, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
#     # Shroud
#     geom_sf(data = st_union(data), fill = "white", alpha = 0.7, color = NA) +
#     # Highlight: Now specifically filtered to the correct population for this DPS
#     geom_sf(data = focus_data_clipped, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
#     # Outline
#     geom_sf(data = focus_polygon, fill = NA, color = "black", linewidth = 1.2) +
#     bi_scale_fill(pal = "Brown2", dim = 4) +
#     bi_theme() +
#     labs(title = current_title) +
#     theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))
#   
#   # Combine with legend
#   ggdraw() + 
#     draw_plot(p, 0, 0, 1, 1) + 
#     draw_plot(legend, 0.005, 0.65, 0.25, 0.25)
# })
plot_list <- lapply(1:nrow(sf_outlines), function(i) {
  focus_polygon <- sf_outlines[i, ]
  current_title <- focus_polygon$DPStrunc
  focus_data_clipped <- st_intersection(data, focus_polygon)
  p <- ggplot() +
    # annotation_map_tile(type = "hotstyle", zoom = 10) +
    geom_sf(data = data, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
    geom_sf(data = st_union(data), fill = "white", alpha = 0.7, color = NA) +
    geom_sf(data = focus_data_clipped, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
    geom_sf(data = focus_polygon, fill = NA, color = "black", linewidth = 1.2) +
    bi_scale_fill(pal = "Brown2", dim = 4) +
    bi_theme() +
    labs(title = current_title) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))
  ggdraw() + 
    draw_plot(p, 0, 0, 1, 1) + 
    draw_plot(legend, 0.55, 0.05, 0.25, 0.25)
})
doink <- wrap_plots(plot_list, ncol = 2)
chin_ARchoro_panel <- doink + inset_element(inset_context, 
                                            left = 0.7, bottom = 0.05, 
                                            right = 0.98, top = 0.3)
chin_ARchoro_panel
ggsave(here("output", "figures", "chin_ARchoro_panel.png"), plot=chin_ARchoro_panel, device="png", dpi=300)



###### steelhead
ARstel <- ARstel %>%
  filter(!is.na(NWFSC_POP_ID))

sf_fish_combined <- sf_fish %>%
  left_join(ARstel, by = "NWFSC_POP_ID")

sf_stel <- sf_fish_combined %>%
  filter(!is.na(mean_a))

# make sure crs is good
sf_stel_nad83 <- st_transform(sf_stel, crs = 4269)

# # can we make these contiguous?
# contiguity_test <- sf_stel_nad83 %>%
#   group_by(NWFSC_POP_ID) %>%
#   summarize(geometry = st_union(SHAPE)) %>%
#   mutate(
#     # break multipolygons into individual polygons and count them
#     piece_count = lengths(st_cast(geometry, "POLYGON", warn = FALSE)),
#     is_contiguous = piece_count == 1
#   )
# 
# # view groups that are NOT contiguous
# non_contiguous <- filter(contiguity_test, !is_contiguous)
# print(non_contiguous)
#   # it looks as though they are all contiguous..

# can this be collapsed?
sf_stel_nad83col <- sf_stel_nad83 %>%
  group_by(NWFSC_POP_ID, DPS_IDtrunc, DPStrunc) %>%
  summarize(
    mean_lnnosa = mean(mean_lnnosa, na.rm = TRUE),
    mean_a      = mean(mean_a, na.rm = TRUE),
    mean_R      = mean(mean_R, na.rm = TRUE),
    .groups = "drop"
  )

# create ESU outlines
sf_outlines <- sf_stel_nad83col %>%
  group_by(DPS_IDtrunc, DPStrunc) %>%
  summarize(SHAPE = st_union(SHAPE))

# plotting
bbox <- st_bbox(sf_stel_nad83col)
region_states <- states(cb = TRUE, resolution = "20m") %>%
  filter(STUSPS %in% c("OR", "WA", "ID")) %>%
  st_transform(4269) # match main map's CRS (NAD83)
# these bounds roughly cover the columbia basin
basin_xlim <- c(-125, -110)
basin_ylim <- c(41.5, 49.5)

# create the basin-centered inset
inset_context <- ggplot() +
  geom_sf(data = region_states, fill = "gray95", color = "gray60", linewidth = 0.3) +
  # red box representing your specific study area
  annotate("rect", 
           xmin = bbox["xmin"], xmax = bbox["xmax"], 
           ymin = bbox["ymin"], ymax = bbox["ymax"], 
           color = "red", fill = NA, linewidth = 0.8) +
  # Crop the map to the Columbia River Basin extent
  coord_sf(xlim = basin_xlim, ylim = basin_ylim, expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    plot.margin = margin(1, 1, 1, 1)
  )

main_map <- ggplot(data = sf_stel_nad83col) +
  annotation_map_tile(type = "hotstyle", zoom = 10) + 
  geom_sf(aes(fill = mean_a), alpha = 0.7) + 
  geom_sf(data = sf_outlines, fill = NA, color = "black", linewidth = 1.2) + 
  coord_sf(crs = 4269) + 
  scale_fill_viridis_c(option = "inferno") + 
  labs(title = "Average bias - steelhead surveys (1980-2024)",
       caption = "Bias measured relative to 'Dam Counts' method",
       fill = "Bias") +
  theme_minimal()
stel_a <- main_map + inset_element(inset_context, 
                                     left = 0.7, bottom = 0.05, 
                                     right = 0.98, top = 0.3)
stel_a
ggsave(here("output", "figures", "stel_a.png"), plot=stel_a, device="png", dpi=300)

main_map <- ggplot(data = sf_stel_nad83col) +
  annotation_map_tile(type = "hotstyle", zoom = 10) + 
  geom_sf(aes(fill = mean_R), alpha = 0.7) + 
  geom_sf(data = sf_outlines, fill = NA, color = "black", linewidth = 1.2) + 
  coord_sf(crs = 4269) + 
  scale_fill_viridis_c(option = "inferno") + 
  labs(title = "Average precision - steelhead surveys (1980-2024)",
       fill = "Variance") +
  theme_minimal()
stel_r <- main_map + inset_element(inset_context, 
                                     left = 0.7, bottom = 0.05, 
                                     right = 0.98, top = 0.3)
stel_r
ggsave(here("output", "figures", "stel_r.png"), plot=stel_r, device="png", dpi=300)

main_map <- ggplot(data = sf_stel_nad83col) +
  annotation_map_tile(
    type = "cartolight",
    zoom = 10
  ) + 
  geom_sf(aes(fill = mean_lnnosa), alpha = 0.7) + 
  geom_sf(data = sf_outlines, fill = NA, color = "black", linewidth = 1.2) + 
  coord_sf(crs = 4269) + 
  scale_fill_viridis_c(option = "inferno") + 
  labs(title = "Average population size - steelhead (1980-2024)",
       fill = "Pop. size") +
  theme_minimal()
stel_pop <- main_map + inset_element(inset_context, 
                         left = 0.7, bottom = 0.05, 
                         right = 0.98, top = 0.3)
stel_pop
ggsave(here("output", "figures", "stel_pop.png"), plot=stel_pop, device="png", dpi=300)

# choropleth
data <- bi_class(sf_stel_nad83col, x = mean_lnnosa, y = mean_R, style = "equal", dim = 4)
  # bi_class creates a new 'bi_class' column based on quantiles of two variables
map <- ggplot() +
  annotation_map_tile(
    type = "hotstyle",
    zoom = 10
  ) + 
  geom_sf(data, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  geom_sf(data = sf_outlines, fill = NA, color = "black", linewidth = 1.2) + 
  bi_scale_fill(pal = "Brown2", dim = 4) + # Choose a built-in bivariate palette
  labs(title = "Average survey variance and average population size (1980-2024)")
  bi_theme()
legend <- bi_legend(pal = "Brown2",
                    dim = 4,
                    xlab = "Population",
                    ylab = "Variance",
                    size = 8) +
  theme(plot.background = element_rect(color = "black", fill = "white", linewidth = 1))
final_plot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, 0.75, 0.2, 0.2) # Adjust coordinates and size as needed
stel_ARchoro <- final_plot + inset_element(inset_context, 
                                           left = 0.7, bottom = 0.05, 
                                           right = 0.98, top = 0.3)
stel_ARchoro
ggsave(here("output", "figures", "stel_ARchoro.png"), plot=stel_ARchoro, device="png", dpi=300)

# getting weird... iterated choropleths by esu
outline_ids <- unique(sf_outlines$DPS_IDtrunc) 
plot_list <- lapply(1:nrow(sf_outlines), function(i) {
  
  # select the single focus polygon
  focus_polygon <- sf_outlines[i, ]
  
  # extract the title for this specific iteration
  current_title <- focus_polygon$DPStrunc
  
  # "cookie cut" the data to the focus polygon boundary
  # this removes all data outside the outline and clips bordering polygons
  focus_data_clipped <- st_intersection(data, focus_polygon)
  
  # build the map
  p <- ggplot() +
    annotation_map_tile(type = "hotstyle", zoom = 10) +
    # Background: Full muted choropleth
    geom_sf(data = data, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
    # Shroud: Semi-opaque white layer
    geom_sf(data = st_union(data), fill = "white", alpha = 0.7, color = NA) +
    # Highlight: Clipped data only
    geom_sf(data = focus_data_clipped, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
    # Outline: Crisp black border
    geom_sf(data = focus_polygon, fill = NA, color = "black", linewidth = 1.2) +
    bi_scale_fill(pal = "Brown2", dim = 4) +
    bi_theme() +
    labs(title = current_title) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))
  
  
  # combine with legend
  ggdraw() + 
    draw_plot(p, 0, 0, 1, 1) + 
    draw_plot(legend, 0.005, 0.65, 0.25, 0.25)
})

doink <- wrap_plots(plot_list, ncol = 2)
stel_ARchoro_panel <- doink + inset_element(inset_context, 
                                           left = 0.7, bottom = 0.05, 
                                           right = 0.98, top = 0.3)
stel_ARchoro_panel
ggsave(here("output", "figures", "stel_ARchoro_panel.png"), plot=stel_ARchoro_panel, device="png", dpi=300)
