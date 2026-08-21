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

here::i_am("code/primary/06.2-cohoARmap.R")
options(max.print=2000)

# set palette of choice
palette <- "turbo"
bivar_palette <- "Brown2"

# pull in AR data
ARcoho <- readRDS(here("data", "clean", "popavgAR_coho.rds"))

# pull in spatial layers
gdb_path <- here("data", "raw", "WCR_Salmon_Steelhead_gdb_2015", "WCR_Salmon_Steelhead_gdb_2015.gdb")
st_layers(gdb_path)
# I want 'fish' datalayer
sf_fish <- read_sf(dsn = gdb_path, layer = "fish")
sf_fish$DPS_IDtrunc <- substr(sf_fish$DPS_ID, 1, 5)
# grab DPS_ID
sf_fish$DPStrunc <- str_remove(sf_fish$DPS, " - Outside legal area$")


###### coho
ARcoho <- ARcoho %>%
  filter(!is.na(NWFSC_POP_ID))

sf_fish_combined <- sf_fish %>%
  left_join(ARcoho, by = "NWFSC_POP_ID")

sf_coho <- sf_fish_combined %>%
  filter(!is.na(mean_a))

# make sure crs is good
sf_coho_nad83 <- st_transform(sf_coho, crs = 4269)

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

# create ESU outlines
sf_outlines <- sf_coho_nad83col %>%
  group_by(DPS_IDtrunc, DPStrunc) %>%
  summarize(SHAPE = st_union(SHAPE))

# esu outlines?
outline_panels_clipped <- lapply(1:nrow(sf_outlines), function(i) {
  focus_outline <- sf_outlines[i, ]
  esu_data_clipped <- st_intersection(sf_coho_nad83col, focus_outline)
  ggplot() +
    geom_sf(data = esu_data_clipped, aes(fill = mean_a), alpha = 0.7, color = "white", size = 0.1) +
    geom_sf(data = focus_outline, fill = NA, color = "black", linewidth = 1.2) +
    scale_fill_viridis_c(option = palette) +
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
bbox <- st_bbox(sf_coho_nad83col)
region_states <- states(cb = TRUE, resolution = "20m") %>%
  filter(STUSPS %in% c("OR", "WA")) %>%
  st_transform(4269) # match main map's CRS (NAD83)
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

# overlap
sf_base <- sf_coho_nad83col %>% filter(NWFSC_POP_ID != 104)
sf_stripe <- sf_coho_nad83col %>% filter(NWFSC_POP_ID == 104)
  # stripe layer: ONLY population 104
shared_borders <- st_intersection(sf_outlines) %>% 
  filter(n.overlaps > 1) %>% 
  st_cast("MULTILINESTRING")

# plotting
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
    option = palette, 
    aesthetics = c("fill", "pattern_fill"), # Apply one scale to BOTH fill and pattern_fill
    name = "Bias"
  ) +
  coord_sf(crs = 4269) +
  labs(title = "Average bias - Coho surveys (1980-2024)")
  theme_minimal() +
    theme(       
      plot.title = element_text(face = "bold", size = 28),       
      legend.title = element_text(size = 28),       
      legend.text = element_text(size = 20),       
      axis.text.x = element_text(size = 16, color = "black"),       
      axis.text.y= element_text(size = 16, color = "black"),     
    )  
coho_a <- main_map + inset_element(inset_context, 
                                   left = 1.05, bottom = -0.2, 
                                   right = 1.55, top = 0.5)
coho_a
# ggsave(here("output", "figures", "coho_a.png"), plot=coho_a, device="png", dpi=300)

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
    option = palette, 
    aesthetics = c("fill", "pattern_fill"), # Apply one scale to BOTH fill and pattern_fill
    name = "Variance"
  ) +
  coord_sf(crs = 4269) +
  labs(title = "Average variance - Coho surveys (1980-2024)")
  theme_minimal() +
    theme(       
      plot.title = element_text(face = "bold", size = 28),       
      legend.title = element_text(size = 28),       
      legend.text = element_text(size = 20),       
      axis.text.x = element_text(size = 16, color = "black"),       
      axis.text.y= element_text(size = 16, color = "black"),     
    ) 
coho_r <- main_map + inset_element(inset_context, 
                                   left = 1.05, bottom = -0.2, 
                                   right = 1.55, top = 0.5)
coho_r
# ggsave(here("output", "figures", "coho_r.png"), plot=coho_r, device="png", dpi=300)

sf_base$precision <- (1/sf_base$mean_R)
sf_stripe$precision <- (1/sf_stripe$mean_R)

main_map <- ggplot() +
  annotation_map_tile(type = "hotstyle", zoom = 10) +
  geom_sf(data = sf_base, aes(fill = precision), alpha = 0.8, color = "white", size = 0.1) +
  geom_sf_pattern(
    data = sf_stripe,
    aes(pattern_fill = precision), 
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
    option = palette, 
    aesthetics = c("fill", "pattern_fill"), # Apply one scale to BOTH fill and pattern_fill
    name = "Precision"
  ) +
  coord_sf(crs = 4269) +
  labs(title = "Average precision\n Coho surveys (1980-2024)") +
  theme_minimal() +
  theme(       
    plot.title = element_text(face = "bold", size = 28),       
    legend.title = element_text(size = 28),       
    legend.text = element_text(size = 20),       
    axis.text.x = element_text(size = 16, color = "black"),       
    axis.text.y= element_text(size = 16, color = "black"),     
  ) 
coho_pre <- main_map + inset_element(inset_context, 
                                     left = 0.7, bottom = 0.05, 
                                     right = 0.98, top = 0.3)
coho_pre

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
    option = palette, 
    aesthetics = c("fill", "pattern_fill"), # Apply one scale to BOTH fill and pattern_fill
    name = "Pop. size"
  ) +
  coord_sf(crs = 4269) +
  labs(title = "Average population size - Coho (1980-2024)")
  theme_minimal() +
    theme(       
      plot.title = element_text(face = "bold", size = 28),       
      legend.title = element_text(size = 28),       
      legend.text = element_text(size = 20),       
      axis.text.x = element_text(size = 16, color = "black"),       
      axis.text.y= element_text(size = 16, color = "black"),     
    ) 
coho_pop <- main_map + inset_element(inset_context, 
                                     left = 1.05, bottom = -0.2, 
                                     right = 1.55, top = 0.5)
coho_pop
# ggsave(here("output", "figures", "coho_pop.png"), plot=coho_pop, device="png", dpi=300)

# # choropleths
data <- bi_class(sf_coho_nad83col, x = mean_lnnosa, y = mean_R, style = "equal", dim = 4)

legend <- bi_legend(pal = bivar_palette,
                    dim = 4,
                    xlab = "Population",
                    ylab = "Variance",
                    size = 18) +
  theme(plot.background = element_rect(color = "black", fill = "white", linewidth = 1))

outline_ids <- unique(sf_outlines$DPS_IDtrunc) 

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
    bi_scale_fill(pal = bivar_palette, dim = 4) +
    bi_theme() +
    labs(title = current_title) +
    theme(       
      plot.title = element_text(face = "bold", size = 28),       
      legend.title = element_text(size = 28),       
      legend.text = element_text(size = 20),       
      axis.text.x = element_text(size = 16, color = "black"),       
      axis.text.y= element_text(size = 16, color = "black"),     
    ) 
  ggdraw() + 
    draw_plot(p, 0, 0, 1, 1) + 
    draw_plot(legend, 0.7, 0.3, 0.25, 0.25)
})

doink <- wrap_plots(plot_list, ncol = 2)
coho_Rpop_choro <- doink + inset_element(inset_context, 
                                            left = 0.7, bottom = 0.05, 
                                            right = 0.98, top = 0.3)
coho_Rpop_choro
# ggsave(here("output", "figures", "coho_ARchoro_panel.png"), plot=coho_ARchoro_panel, device="png", dpi=300)

data <- bi_class(sf_coho_nad83col, x = mean_lnnosa, y = mean_a, style = "equal", dim = 4)

legend <- bi_legend(pal = bivar_palette,
                    dim = 4,
                    xlab = "Population",
                    ylab = "Bias",
                    size = 18) +
  theme(plot.background = element_rect(color = "black", fill = "white", linewidth = 1))

outline_ids <- unique(sf_outlines$DPS_IDtrunc) 

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
    bi_scale_fill(pal = bivar_palette, dim = 4) +
    bi_theme() +
    labs(title = current_title) +
    theme(       
      plot.title = element_text(face = "bold", size = 28),       
      legend.title = element_text(size = 28),       
      legend.text = element_text(size = 20),       
      axis.text.x = element_text(size = 16, color = "black"),       
      axis.text.y= element_text(size = 16, color = "black"),     
    ) 
  ggdraw() + 
    draw_plot(p, 0, 0, 1, 1) + 
    draw_plot(legend, 0.7, 0.3, 0.25, 0.25)
})

doink <- wrap_plots(plot_list, ncol = 2)
coho_Apop_choro <- doink + inset_element(inset_context, 
                                         left = 0.7, bottom = 0.05, 
                                         right = 0.98, top = 0.3)
coho_Apop_choro
# ggsave(here("output", "figures", "coho_ARchoro_panel.png"), plot=coho_ARchoro_panel, device="png", dpi=300)

data <- bi_class(sf_coho_nad83col, x = mean_a, y = mean_R, style = "equal", dim = 4)

legend <- bi_legend(pal = bivar_palette,
                    dim = 4,
                    xlab = "Bias",
                    ylab = "Variance",
                    size = 18) +
  theme(plot.background = element_rect(color = "black", fill = "white", linewidth = 1))

outline_ids <- unique(sf_outlines$DPS_IDtrunc) 

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
    bi_scale_fill(pal = bivar_palette, dim = 4) +
    bi_theme() +
    labs(title = current_title) +
    theme(       
      plot.title = element_text(face = "bold", size = 28),       
      legend.title = element_text(size = 28),       
      legend.text = element_text(size = 20),       
      axis.text.x = element_text(size = 16, color = "black"),       
      axis.text.y= element_text(size = 16, color = "black"),     
    ) 
  ggdraw() + 
    draw_plot(p, 0, 0, 1, 1) + 
    draw_plot(legend, 0.7, 0.3, 0.25, 0.25)
})

doink <- wrap_plots(plot_list, ncol = 2)
coho_AR_choro <- doink + inset_element(inset_context, 
                                         left = 0.7, bottom = 0.05, 
                                         right = 0.98, top = 0.3)
coho_AR_choro
ggsave(here("output", "figures", "coho_ARchoro_panel.png"), plot=coho_ARchoro_panel, device="png", dpi=300)

## iterated bias and precision by esu
# bias
outline_ids <- unique(sf_outlines$DPS_IDtrunc) 
plot_list <- lapply(1:nrow(sf_outlines), function(i) {
  
  # select the single focus polygon
  focus_polygon <- sf_outlines[i, ]
  
  # extract the title for this specific iteration
  current_title <- focus_polygon$DPStrunc
  
  # "cookie cut" the data to the focus polygon boundary
  # this removes all data outside the outline and clips bordering polygons
  focus_data_clipped <- st_intersection(sf_coho_nad83col, focus_polygon)
  
  # build the map
  p <- ggplot() +
    annotation_map_tile(type = "hotstyle", zoom = 10) +
    # Background: Full muted choropleth
    geom_sf(data = sf_coho_nad83col, mapping = aes(fill = mean_a), color = "white", size = 0.1, show.legend = FALSE) +
    # Shroud: Semi-opaque white layer
    geom_sf(data = st_union(sf_coho_nad83col), fill = "white", alpha = 0.7, color = NA) +
    # Highlight: Clipped data only
    geom_sf(data = focus_data_clipped, mapping = aes(fill = mean_a), color = "white", size = 0.1, show.legend = TRUE) +
    # Outline: Crisp black border
    geom_sf(data = focus_polygon, fill = NA, color = "black", linewidth = 1.2) +
    coord_sf(crs = 4269) + 
    scale_fill_viridis_c(option = palette, name = "Bias") + 
    labs(title = current_title) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))
})

doink <- wrap_plots(plot_list, ncol = 2) + plot_layout(guides = "collect") +
  plot_annotation(title = "Average bias - coho surveys (1980 - 2024)",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)))
cohoBias_panel <- doink + inset_element(inset_context, 
                                        left = 0.7, bottom = 0.05, 
                                        right = 0.98, top = 0.3)
cohoBias_panel

# Precision
outline_ids <- unique(sf_outlines$DPS_IDtrunc) 
plot_list <- lapply(1:nrow(sf_outlines), function(i) {
  
  # select the single focus polygon
  focus_polygon <- sf_outlines[i, ]
  
  # extract the title for this specific iteration
  current_title <- focus_polygon$DPStrunc
  
  # "cookie cut" the data to the focus polygon boundary
  # this removes all data outside the outline and clips bordering polygons
  focus_data_clipped <- st_intersection(sf_coho_nad83col, focus_polygon)
  
  # build the map
  p <- ggplot() +
    annotation_map_tile(type = "hotstyle", zoom = 10) +
    # Background: Full muted choropleth
    geom_sf(data = sf_coho_nad83col, mapping = aes(fill = precision), color = "white", size = 0.1, show.legend = FALSE) +
    # Shroud: Semi-opaque white layer
    geom_sf(data = st_union(sf_coho_nad83col), fill = "white", alpha = 0.7, color = NA) +
    # Highlight: Clipped data only
    geom_sf(data = focus_data_clipped, mapping = aes(fill = precision), color = "white", size = 0.1, show.legend = TRUE) +
    # Outline: Crisp black border
    geom_sf(data = focus_polygon, fill = NA, color = "black", linewidth = 1.2) +
    coord_sf(crs = 4269) + 
    scale_fill_viridis_c(option = palette, name = "Precision") + 
    labs(title = current_title) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))
})

doink <- wrap_plots(plot_list, ncol = 2) + plot_layout(guides = "collect") +
  plot_annotation(title = "Average precision - coho surveys (1980 - 2024)",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)))
cohoPre_panel <- doink + inset_element(inset_context, 
                                       left = 0.7, bottom = 0.05, 
                                       right = 0.98, top = 0.3)
cohoPre_panel
