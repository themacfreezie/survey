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
palette <- "turbo"

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
  scale_fill_viridis_c(
    option = palette, 
    name = "Pct Change\nPop Size\n(1980-2024)",
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
multipanel_layout <- (p1 / p3) |  p2 + 
  plot_layout(guides = "keep", widths = c(2.5, 1.5)) 

# Append unified title, captioning, and the shared legend block
final_output <- cowplot::plot_grid(
  multipanel_layout, 
  shared_legend, 
  rel_widths = c(10, 1.5), 
  nrow = 1
)

# Render complete layout
final_output