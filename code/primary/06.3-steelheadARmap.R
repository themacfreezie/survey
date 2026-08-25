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

here::i_am("code/primary/06.3-steelheadARmap.R")
options(max.print=2000)

# set palette of choice
palette <- "turbo"
bivar_palette <- "Brown2"

# set crs of choice
crsSET <- 4326

# pull in AR data
ARstel <- readRDS(here("data", "clean", "popavgAR_stel.rds"))

# pull in spatial layers
gdb_path <- here("data", "raw", "WCR_Salmon_Steelhead_gdb_2015", "WCR_Salmon_Steelhead_gdb_2015.gdb")
st_layers(gdb_path)
# I want 'fish' datalayer
sf_fish <- read_sf(dsn = gdb_path, layer = "fish")
sf_fish$DPS_IDtrunc <- substr(sf_fish$DPS_ID, 1, 5)
# grab DPS_ID
sf_fish$DPStrunc <- str_remove(sf_fish$DPS, " - Outside legal area$")

###### steelhead
ARstel <- ARstel %>%
  filter(!is.na(NWFSC_POP_ID))

sf_fish_combined <- sf_fish %>%
  left_join(ARstel, by = "NWFSC_POP_ID")

sf_stel <- sf_fish_combined %>%
  filter(!is.na(mean_a))

# make sure crs is good
sf_stel_nad83 <- st_transform(sf_stel, crs = crsSET)

# can we make these contiguous?
contiguity_test <- sf_stel_nad83 %>%
  group_by(NWFSC_POP_ID) %>%
  summarize(geometry = st_union(SHAPE)) %>%
  mutate(
    # break multipolygons into individual polygons and count them
    piece_count = lengths(st_cast(geometry, "POLYGON", warn = FALSE)),
    is_contiguous = piece_count == 1
  )

# view groups that are NOT contiguous
non_contiguous <- filter(contiguity_test, !is_contiguous)
print(non_contiguous)
  # it looks as though they are all contiguous..

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
  st_transform(crsSET) # match main map's CRS (NAD83)
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
  geom_sf(aes(fill = mean_a), alpha = 0.8, color = "white", size = 0.1) + 
  geom_sf(data = sf_outlines, fill = NA, color = "black", linewidth = 1.2) + 
  coord_sf(crs = crsSET) + 
  scale_fill_viridis_c(option = palette) + 
  labs(title = "Average bias - steelhead surveys (1980-2024)",
       caption = "Bias measured relative to 'Dam Counts' method",
       fill = "Bias") +
  theme_minimal() +
  theme(       
    plot.title = element_text(face = "bold", size = 28),       
    legend.title = element_text(size = 28),       
    legend.text = element_text(size = 20),       
    axis.text.x = element_text(size = 16, color = "black"),       
    axis.text.y= element_text(size = 16, color = "black"),     
  ) 
stel_a <- main_map + inset_element(inset_context, 
                                   left = 0.7, bottom = 0.05, 
                                   right = 0.98, top = 0.3)
# stel_a
ggsave(here("output", "figures", "stel_a.png"), plot=stel_a, device="png", dpi=300)

main_map <- ggplot(data = sf_stel_nad83col) +
  annotation_map_tile(type = "hotstyle", zoom = 10) + 
  geom_sf(aes(fill = mean_R), alpha = 0.8, color = "white", size = 0.1) +
  geom_sf(data = sf_outlines, fill = NA, color = "black", linewidth = 1.2) + 
  coord_sf(crs = crsSET) + 
  scale_fill_viridis_c(option = palette) + 
  labs(title = "Average variance - steelhead surveys (1980-2024)",
       fill = "Variance") +
  theme_minimal() +
  theme(       
    plot.title = element_text(face = "bold", size = 28),       
    legend.title = element_text(size = 28),       
    legend.text = element_text(size = 20),       
    axis.text.x = element_text(size = 16, color = "black"),       
    axis.text.y= element_text(size = 16, color = "black"),     
  ) 
stel_r <- main_map + inset_element(inset_context, 
                                   left = 0.7, bottom = 0.05, 
                                   right = 0.98, top = 0.3)
# stel_r
ggsave(here("output", "figures", "stel_r.png"), plot=stel_r, device="png", dpi=300)

sf_stel_nad83col$precision <- (1/sf_stel_nad83col$mean_R)

main_map <- ggplot(data = sf_stel_nad83col) +
  annotation_map_tile(type = "hotstyle", zoom = 10) + 
  geom_sf(aes(fill = precision), alpha = 0.8, color = "white", size = 0.1) + 
  geom_sf(data = sf_outlines, fill = NA, color = "black", linewidth = 1.2) + 
  coord_sf(crs = crsSET) + 
  scale_fill_viridis_c(option = palette) + 
  labs(title = "Average precision - steelhead surveys (1980-2024)",
       fill = "Precision") +
  theme_minimal() +
  theme(       
    plot.title = element_text(face = "bold", size = 28),       
    legend.title = element_text(size = 28),       
    legend.text = element_text(size = 20),       
    axis.text.x = element_text(size = 16, color = "black"),       
    axis.text.y= element_text(size = 16, color = "black"),     
  ) 
stel_pre <- main_map + inset_element(inset_context, 
                                   left = 0.7, bottom = 0.05, 
                                   right = 0.98, top = 0.3)
# stel_pre
ggsave(here("output", "figures", "stel_pre.png"), plot=stel_pre, device="png", dpi=300)

main_map <- ggplot(data = sf_stel_nad83col) +
  annotation_map_tile(
    type = "cartolight",
    zoom = 10
  ) + 
  geom_sf(aes(fill = mean_lnnosa), alpha = 0.7) + 
  geom_sf(data = sf_outlines, fill = NA, color = "black", linewidth = 1.2) + 
  coord_sf(crs = crsSET) + 
  scale_fill_viridis_c(option = palette) + 
  labs(title = "Average population size - steelhead (1980-2024)",
       fill = "Pop. size") +
  theme_minimal() +
  theme(       
    plot.title = element_text(face = "bold", size = 28),       
    legend.title = element_text(size = 28),       
    legend.text = element_text(size = 20),       
    axis.text.x = element_text(size = 16, color = "black"),       
    axis.text.y= element_text(size = 16, color = "black"),     
  ) 
stel_pop <- main_map + inset_element(inset_context, 
                                     left = 0.7, bottom = 0.05, 
                                     right = 0.98, top = 0.3)
# stel_pop
ggsave(here("output", "figures", "stel_pop.png"), plot=stel_pop, device="png", dpi=300)

# choropleths
data <- bi_class(sf_stel_nad83col, x = mean_lnnosa, y = mean_R, style = "equal", dim = 4)
# bi_class creates a new 'bi_class' column based on quantiles of two variables
map <- ggplot() +
  annotation_map_tile(
    type = "hotstyle",
    zoom = 10
  ) + 
  geom_sf(data, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  geom_sf(data = sf_outlines, fill = NA, color = "black", linewidth = 1.2) + 
  bi_scale_fill(pal = bivar_palette, dim = 4) + # Choose a built-in bivariate palette
  theme(       
    plot.title = element_text(face = "bold", size = 28),       
    legend.title = element_text(size = 28),       
    legend.text = element_text(size = 20),       
    axis.text.x = element_text(size = 16, color = "black"),       
    axis.text.y= element_text(size = 16, color = "black"),     
  ) 
bi_theme()
legend <- bi_legend(pal = bivar_palette,
                    dim = 4,
                    xlab = "Population",
                    ylab = "Variance",
                    size = 16) +
  theme(plot.background = element_rect(color = "black", fill = "white", linewidth = 1))
final_plot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, 0.75, 0.2, 0.2) # Adjust coordinates and size as needed
stel_Rpop_choro <- final_plot + inset_element(inset_context, 
                                           left = 0.7, bottom = 0.05, 
                                           right = 0.98, top = 0.3)
# stel_Rpop_choro

data <- bi_class(sf_stel_nad83col, x = mean_lnnosa, y = mean_a, style = "equal", dim = 4)
# bi_class creates a new 'bi_class' column based on quantiles of two variables
map <- ggplot() +
  annotation_map_tile(
    type = "hotstyle",
    zoom = 10
  ) + 
  geom_sf(data, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  geom_sf(data = sf_outlines, fill = NA, color = "black", linewidth = 1.2) + 
  bi_scale_fill(pal = bivar_palette, dim = 4) + # Choose a built-in bivariate palette
  theme(       
    plot.title = element_text(face = "bold", size = 28),       
    legend.title = element_text(size = 28),       
    legend.text = element_text(size = 20),       
    axis.text.x = element_text(size = 16, color = "black"),       
    axis.text.y= element_text(size = 16, color = "black"),     
  ) 
bi_theme()
legend <- bi_legend(pal = bivar_palette,
                    dim = 4,
                    xlab = "Population",
                    ylab = "Bias",
                    size = 16) +
  theme(plot.background = element_rect(color = "black", fill = "white", linewidth = 1))
final_plot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, 0.75, 0.2, 0.2) # Adjust coordinates and size as needed
stel_Apop_choro <- final_plot + inset_element(inset_context, 
                                           left = 0.7, bottom = 0.05, 
                                           right = 0.98, top = 0.3)
# stel_Apop_choro


data <- bi_class(sf_stel_nad83col, x = mean_a, y = mean_R, style = "equal", dim = 4)
# bi_class creates a new 'bi_class' column based on quantiles of two variables
map <- ggplot() +
  annotation_map_tile(
    type = "hotstyle",
    zoom = 10
  ) + 
  geom_sf(data, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  geom_sf(data = sf_outlines, fill = NA, color = "black", linewidth = 1.2) + 
  bi_scale_fill(pal = bivar_palette, dim = 4) + # Choose a built-in bivariate palette
  theme(       
    plot.title = element_text(face = "bold", size = 28),       
    legend.title = element_text(size = 28),       
    legend.text = element_text(size = 20),       
    axis.text.x = element_text(size = 16, color = "black"),       
    axis.text.y= element_text(size = 16, color = "black"),     
  ) 
bi_theme()
legend <- bi_legend(pal = bivar_palette,
                    dim = 4,
                    xlab = "Bias",
                    ylab = "Variance",
                    size = 16) +
  theme(plot.background = element_rect(color = "black", fill = "white", linewidth = 1))
final_plot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, 0.75, 0.2, 0.2) # Adjust coordinates and size as needed
stel_AR_choro <- final_plot + inset_element(inset_context, 
                                           left = 0.7, bottom = 0.05, 
                                           right = 0.98, top = 0.3)
# stel_AR_choro

# iterated bias and precision by esu
# bias
outline_ids <- unique(sf_outlines$DPS_IDtrunc) 
plot_list <- lapply(1:nrow(sf_outlines), function(i) {
  
  # select the single focus polygon
  focus_polygon <- sf_outlines[i, ]
  
  # extract the title for this specific iteration
  current_title <- focus_polygon$DPStrunc
  
  # "cookie cut" the data to the focus polygon boundary
  # this removes all data outside the outline and clips bordering polygons
  focus_data_clipped <- st_intersection(sf_stel_nad83col, focus_polygon)
  
  # build the map
  p <- ggplot() +
    annotation_map_tile(type = "hotstyle", zoom = 10) +
    # Background: Full muted choropleth
    geom_sf(data = sf_stel_nad83col, mapping = aes(fill = mean_a), color = "white", size = 0.1, show.legend = FALSE) +
    # Shroud: Semi-opaque white layer
    geom_sf(data = st_union(sf_stel_nad83col), fill = "white", alpha = 0.7, color = NA) +
    # Highlight: Clipped data only
    geom_sf(data = focus_data_clipped, mapping = aes(fill = mean_a), color = "white", size = 0.1, show.legend = TRUE) +
    # Outline: Crisp black border
    geom_sf(data = focus_polygon, fill = NA, color = "black", linewidth = 1.2) +
    coord_sf(crs = crsSET) + 
    scale_fill_viridis_c(option = palette, name = "Bias") + 
    labs(title = current_title) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))
})

doink <- wrap_plots(plot_list, ncol = 2) + plot_layout(guides = "collect") +
  plot_annotation(title = "Average bias - steelhead surveys (1980 - 2024)",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)))
stelBias_panel <- doink + inset_element(inset_context, 
                                         left = 0.7, bottom = 0.05, 
                                         right = 1.1, top = 0.35)
stelBias_panel

# Precision
outline_ids <- unique(sf_outlines$DPS_IDtrunc) 
plot_list <- lapply(1:nrow(sf_outlines), function(i) {
  
  # select the single focus polygon
  focus_polygon <- sf_outlines[i, ]
  
  # extract the title for this specific iteration
  current_title <- focus_polygon$DPStrunc
  
  # "cookie cut" the data to the focus polygon boundary
  # this removes all data outside the outline and clips bordering polygons
  focus_data_clipped <- st_intersection(sf_stel_nad83col, focus_polygon)
  
  # build the map
  p <- ggplot() +
    annotation_map_tile(type = "hotstyle", zoom = 10) +
    # Background: Full muted choropleth
    geom_sf(data = sf_stel_nad83col, mapping = aes(fill = precision), color = "white", size = 0.1, show.legend = FALSE) +
    # Shroud: Semi-opaque white layer
    geom_sf(data = st_union(sf_stel_nad83col), fill = "white", alpha = 0.7, color = NA) +
    # Highlight: Clipped data only
    geom_sf(data = focus_data_clipped, mapping = aes(fill = precision), color = "white", size = 0.1, show.legend = TRUE) +
    # Outline: Crisp black border
    geom_sf(data = focus_polygon, fill = NA, color = "black", linewidth = 1.2) +
    coord_sf(crs = crsSET) + 
    scale_fill_viridis_c(option = palette, name = "Precision") + 
    labs(title = current_title) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))
})

doink <- wrap_plots(plot_list, ncol = 2) + plot_layout(guides = "collect") +
  plot_annotation(title = "Average precision - steelhead surveys (1980 - 2024)",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)))
stelPre_panel <- doink + inset_element(inset_context, 
                                            left = 0.7, bottom = 0.05, 
                                            right = 1.1, top = 0.35)
stelPre_panel
