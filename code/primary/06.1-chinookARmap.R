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

here::i_am("code/primary/06.1-chinookARmap.R")
options(max.print=2000)

# pull in AR data
ARchin <- readRDS(here("data", "clean", "popavgAR_chin.rds"))

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
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 24),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 14),
    axis.text.x = element_text(size = 14, color = "black"),
    axis.text.y= element_text(size = 14, color = "black"),
  )
chin_a <- main_map + inset_element(inset_context, 
                                   left = 0.7, bottom = 0.05, 
                                   right = 0.98, top = 0.3)
chin_a
# ggsave(here("output", "figures", "chin_a.png"), plot=chin_a, device="png", dpi=300)

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
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 24),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 14),
    axis.text.x = element_text(size = 14, color = "black"),
    axis.text.y= element_text(size = 14, color = "black"),
  )
chin_r <- main_map + inset_element(inset_context, 
                                   left = 0.7, bottom = 0.05, 
                                   right = 0.98, top = 0.3)
chin_r
# ggsave(here("output", "figures", "chin_r.png"), plot=chin_r, device="png", dpi=300)

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
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 24),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 14),
    axis.text.x = element_text(size = 14, color = "black"),
    axis.text.y= element_text(size = 14, color = "black"),
  )
chin_pop <- main_map + inset_element(inset_context, 
                                     left = 0.7, bottom = 0.05, 
                                     right = 0.98, top = 0.3)
chin_pop
# ggsave(here("output", "figures", "chin_pop.png"), plot=chin_pop, device="png", dpi=300)

# # choropleth
data <- bi_class(sf_chin_nad83col, x = mean_lnnosa, y = mean_R, style = "equal", dim = 4)

legend <- bi_legend(pal = "Brown2",
                    dim = 4,
                    xlab = "Population",
                    ylab = "Variance",
                    size = 8) +
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
# ggsave(here("output", "figures", "chin_ARchoro_panel.png"), plot=chin_ARchoro_panel, device="png", dpi=300)

# facet wraps
# 1. Prepare the data for faceting
# We want all populations in one object, ensuring 4 and 104 are distinct
sf_facet_data <- sf_chin_nad83col %>%
  mutate(is_overlap = NWFSC_POP_ID %in% c(4, 104))

# 2. Function to generate the faceted plots
create_facet_plot <- function(data, fill_var, title_text, legend_name) {
  ggplot() +
    # Background: Show the full basin outline in light gray for context in every facet
    geom_sf(data = sf_outlines, fill = "gray95", color = "gray80", size = 0.1) +
    
    # Foreground: Plot only the populations belonging to the facet's DPS
    geom_sf(data = data, aes(fill = !!sym(fill_var)), color = "white", size = 0.05) +
    
    # Highlight the specific DPS boundary for that facet
    geom_sf(data = sf_outlines, fill = NA, color = "black", linewidth = 0.6) +
    
    scale_fill_viridis_c(option = "inferno", name = legend_name) +
    
    # Facet by DPS - this ensures Pop 4 shows up in its DPS and 104 in its own
    facet_wrap(~DPStrunc, ncol = 2) +
    
    labs(title = title_text,
         subtitle = "Populations 4 and 104 correctly separated by DPS assignment",
         caption = "Data: 1980-2024 Chinook Surveys") +
    theme_minimal() +
    theme(
      strip.text = element_text(face = "bold", size = 9),
      legend.position = "right",
      panel.grid = element_blank()
    )
}

# 3. Generate the Bias Facet Plot
chin_a_facet <- create_facet_plot(
  sf_facet_data, 
  "mean_a", 
  "Chinook Survey Bias by DPS", 
  "Bias"
)

# 4. Generate the Precision Facet Plot
chin_r_facet <- create_facet_plot(
  sf_facet_data, 
  "mean_R", 
  "Chinook Survey Precision by DPS", 
  "Variance"
)

# # Display the plots
# print(chin_a_facet)
# print(chin_r_facet)

# individual panels
# 1. Ensure the list of DPS is derived from the population data
# This ensures we iterate through each ESU identity
dps_list <- unique(sf_facet_data$DPStrunc)

# 2. Corrected function to isolate overlapping populations
create_single_dps_plot <- function(current_dps_name, data, fill_var, title_prefix, legend_name) {
  
  # CRITICAL STEP: Filter the population data FIRST
  # This ensures Pop 4 is in the LC plot and 104 is in the UW plot
  dps_data <- data %>% 
    filter(DPStrunc == current_dps_name)
  
  # Filter the outline to ONLY the boundary of the current ESU
  dps_outline <- sf_outlines %>% 
    filter(DPStrunc == current_dps_name)
  
  ggplot() +
    annotation_map_tile(type = "hotstyle", zoom = 10) +
    # Layer 1: Global Context
    # We show all ESU outlines in very faint gray as a base map
    geom_sf(data = sf_outlines, fill = "gray98", color = "gray90", size = 0.1) +
    
    # Layer 2: The Data
    # Only the populations belonging to THIS dps_name are drawn here.
    # Because Pop 4 and 104 have different DPStrunc values, they will never 
    # appear in the same plot object.
    geom_sf(data = dps_data, 
            aes(fill = !!sym(fill_var)), 
            color = "white", 
            size = 0.1) +
    
    # Layer 3: The Specific Highlight
    # Draw the heavy black outline ONLY for the current ESU
    geom_sf(data = dps_outline, 
            fill = NA, 
            color = "black", 
            linewidth = 0.8) +
    
    scale_fill_viridis_c(option = "inferno", 
                         name = legend_name,
                         # Ensure the scale limits are consistent across all plots
                         limits = range(data[[fill_var]], na.rm = TRUE)) +
    
    labs(
      title = current_dps_name,
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 24),
      legend.title = element_text(size = 18),
      legend.text = element_text(size = 14),
      axis.text.x = element_text(size = 14, color = "black"),
      axis.text.y= element_text(size = 14, color = "black"),
      panel.grid = element_blank(),
      legend.position = "right"
    ) +
    inset_element(
      inset_context,
      left = 0.7, bottom = 0.05,
      right = 0.95, top = 0.35
    )
}

# 3. Generate the individual plot lists
bias_plots_list <- lapply(dps_list, function(d) {
  create_single_dps_plot(d, sf_facet_data, "mean_a", "Bias", "Bias")
})

precision_plots_list <- lapply(dps_list, function(d) {
  create_single_dps_plot(d, sf_facet_data, "mean_R", "Precision", "Variance")
})

# To verify, you can call them specifically:
# Lower Columbia (contains Pop 4)
# Upper Willamette (contains Pop 104)

# panel bivariate choropleth
bi_data <- bi_class(
  sf_chin_nad83col,
  x = mean_a,
  y = mean_R,
  style = "equal",
  dim = 4
)

bi_legend_shared <- bi_legend(
  pal = "Brown2",
  dim = 4,
  xlab = "Bias",
  ylab = "Variance",
  size = 8
) +
  theme(
    plot.background = element_rect(color = "black", fill = "white", linewidth = 1),
    axis.title.x = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(size = 18, face = "bold")
  )

create_bivariate_dps_plot <- function(current_dps_name, data, legend) {
  
  # Filter data for this DPS only
  dps_data <- data %>%
    filter(DPStrunc == current_dps_name)
  
  # Get corresponding outline
  dps_outline <- sf_outlines %>%
    filter(DPStrunc == current_dps_name)
  
  # Base plot
  p <- ggplot() +
    annotation_map_tile(type = "hotstyle", zoom = 10) +
    
    # Light background of all ESUs for context
    geom_sf(data = sf_outlines, fill = "gray98", color = "gray90", size = 0.1) +
    
    # Bivariate fill ONLY for this DPS
    geom_sf(
      data = dps_data,
      aes(fill = bi_class),
      color = "white",
      size = 0.1
    ) +
    
    # Highlight this DPS boundary
    geom_sf(
      data = dps_outline,
      fill = NA,
      color = "black",
      linewidth = 0.8
    ) +
    
    bi_scale_fill(pal = "Brown2", dim = 4) +
    
    labs(title = current_dps_name) +
    
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 24),
      legend.title = element_text(size = 24),
      legend.text = element_text(size = 18),
      axis.text.x = element_text(size = 14, color = "black"),
      axis.text.y= element_text(size = 14, color = "black"),
      panel.grid = element_blank(),
      legend.position = "none"  # we add legend manually
    )
  
  # Combine main plot + legend
  combined <- ggdraw() +
    draw_plot(p, 0, 0, 1, 1) +
    draw_plot(legend, 0.7, 0.05, 0.35, 0.35)
  
  # ✅ Add inset here
  combined +
    inset_element(
      inset_context,
      left = 0.55, bottom = 0.05,
      right = 0.75, top = 0.35
    )
}

dps_list <- unique(bi_data$DPStrunc)

bivar_dps_plots <- lapply(dps_list, function(d) {
  create_bivariate_dps_plot(d, bi_data, bi_legend_shared)
})

bias_plots_list[[1]]
bias_plots_list[[2]]
bias_plots_list[[3]]

precision_plots_list[[1]]
precision_plots_list[[2]]
precision_plots_list[[3]]

bivar_dps_plots[[1]]
bivar_dps_plots[[2]]
bivar_dps_plots[[3]]