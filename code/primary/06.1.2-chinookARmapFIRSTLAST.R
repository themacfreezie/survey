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

here::i_am("code/primary/06.1.2-chinookARmapFIRSTLAST.R")
options(max.print=2000)

# pull in AR data
ARchin <- readRDS(here("data", "clean", "FLavgAR_chin.rds"))

# pull in spatial layers
gdb_path <- here("data", "raw", "WCR_Salmon_Steelhead_gdb_2015", "WCR_Salmon_Steelhead_gdb_2015.gdb")
st_layers(gdb_path)
# I want 'fish' datalayer
sf_fish <- read_sf(dsn = gdb_path, layer = "fish")
sf_fish$DPS_IDtrunc <- substr(sf_fish$DPS_ID, 1, 5)
# grab DPS_ID
sf_fish$DPStrunc <- str_remove(sf_fish$DPS, " - Outside legal area$")


###### chinook
ARchin <- ARchin %>%
  filter(!is.na(NWFSC_POP_ID))

sf_fish_combined <- sf_fish %>%
  left_join(ARchin, by = "NWFSC_POP_ID")

sf_chin <- sf_fish_combined %>%
  filter(!is.na(first10_mean_a))

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
    first10mean_lnnosa = mean(first10_mean_lnnosa, na.rm = TRUE),
    first10mean_a      = mean(first10_mean_a, na.rm = TRUE),
    first10mean_R      = mean(first10_mean_R, na.rm = TRUE),
    last10mean_lnnosa = mean(last10_mean_lnnosa, na.rm = TRUE),
    last10mean_a      = mean(last10_mean_a, na.rm = TRUE),
    last10mean_R      = mean(last10_mean_R, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate % change from 'first10' to 'last10' variables
sf_chin_nad83col <- sf_chin_nad83col%>%
  mutate(
    # Percentage format (e.g., 15.4 for a 15.4% increase)
    change_a = last10mean_a - first10mean_a,
    change_R = last10mean_R - first10mean_R,
    pctchange_lnnosa = ((last10mean_lnnosa  - first10mean_lnnosa ) / first10mean_lnnosa ) * 100
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
    geom_sf(data = esu_data_clipped, aes(fill = change_a), alpha = 0.7, color = "white", size = 0.1) +
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
main_map <- ggplot() +
  annotation_map_tile(type = "hotstyle", zoom = 10) +
  geom_sf(data = sf_base, aes(fill = change_a), alpha = 0.8, color = "white", size = 0.1) +
  geom_sf_pattern(
    data = sf_stripe,
    aes(pattern_fill = change_a), 
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
  labs(title = "Change in bias - Chinook surveys (1980-2024)",
       caption = "Bias measured relative to 'Dam Counts' method, Solid color = Lower Columbia ESU | Striped color = Upper Willamette ESU") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 28),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y= element_text(size = 18, color = "black"),
  ) 
chin_a <- main_map + inset_element(inset_context, 
                                   left = 0.7, bottom = 0.05, 
                                   right = 0.98, top = 0.3)
chin_a
# ggsave(here("output", "figures", "chin_a.png"), plot=chin_a, device="png", dpi=300)

main_map <- ggplot() +
  annotation_map_tile(type = "hotstyle", zoom = 10) +
  geom_sf(data = sf_base, aes(fill = change_R), alpha = 0.8, color = "white", size = 0.1) +
  geom_sf_pattern(
    data = sf_stripe,
    aes(pattern_fill = change_R), 
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
  labs(title = "Change in varaince - Chinook surveys (1980-2024)",
       caption = "Solid color = Lower Columbia ESU | Striped color = Upper Willamette ESU") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 28),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y= element_text(size = 18, color = "black"),
  ) 
chin_r <- main_map + inset_element(inset_context, 
                                   left = 0.7, bottom = 0.05, 
                                   right = 0.98, top = 0.3)
chin_r
# ggsave(here("output", "figures", "chin_r.png"), plot=chin_r, device="png", dpi=300)

main_map <- ggplot() +
  annotation_map_tile(type = "hotstyle", zoom = 10) +
  geom_sf(data = sf_base, aes(fill = pctchange_lnnosa), alpha = 0.8, color = "white", size = 0.1) +
  geom_sf_pattern(
    data = sf_stripe,
    aes(pattern_fill = pctchange_lnnosa), 
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
  labs(title = "Percent change in population size - Chinook (1980-2024)",
       caption = "Solid color = Lower Columbia ESU | Striped color = Upper Willamette ESU") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 28),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y= element_text(size = 18, color = "black"),
  ) 
chin_pop <- main_map + inset_element(inset_context, 
                                     left = 0.7, bottom = 0.05, 
                                     right = 0.98, top = 0.3)
chin_pop
# ggsave(here("output", "figures", "chin_pop.png"), plot=chin_pop, device="png", dpi=300)