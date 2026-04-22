## SET WORKING DIR & PACKAGES
library(ggspatial)
library(here)
library(prettymapr)
library(sf)
library(tidyverse)

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

# coho test case - as no NWFSC ids are missing
sf_fish_combined <- sf_fish %>%
  left_join(ARcoho, by = "NWFSC_POP_ID")

sf_coho <- sf_fish_combined %>%
  filter(!is.na(mean_a))

# test plot
sf_coho_nad83 <- st_transform(sf_coho, crs = 4269)

# ggplot(data = sf_coho_nad83) +
#   annotation_map_tile(type = "osm", zoom = 10) + 
#   geom_sf(aes(fill = mean_a), alpha = 0.7) + 
#   coord_sf(crs = 4269) + 
#   scale_fill_viridis_c(option = "plasma") + # Provides a high-contrast, accessible gradient
#   labs(title = "Map of bias in coho populations",
#        fill = "Mean bias") +
#   theme_minimal()
# 
# ggplot(data = sf_coho_nad83) +
#   annotation_map_tile(type = "osm", zoom = 10) + 
#   geom_sf(aes(fill = mean_R), alpha = 0.7) + 
#   coord_sf(crs = 4269) + 
#   scale_fill_viridis_c(option = "plasma") + # Provides a high-contrast, accessible gradient
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
  group_by(NWFSC_POP_ID) %>%
  summarize(
    mean_lnnosa = mean(mean_lnnosa, na.rm = TRUE),
    mean_a      = mean(mean_a, na.rm = TRUE),
    mean_R      = mean(mean_R, na.rm = TRUE),
    .groups = "drop"
  )

# replotting
ggplot(data = sf_coho_nad83col) +
  annotation_map_tile(type = "osm", zoom = 10) + 
  geom_sf(aes(fill = mean_a), alpha = 0.7) + 
  coord_sf(crs = 4269) + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "Map of relative bias in coho populations",
       fill = "Mean relative bias") +
  theme_minimal()

ggplot(data = sf_coho_nad83col) +
  annotation_map_tile(type = "osm", zoom = 10) + 
  geom_sf(aes(fill = mean_R), alpha = 0.7) + 
  coord_sf(crs = 4269) + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "Map of precision in coho populations",
       fill = "Mean variance") +
  theme_minimal()

ggplot(data = sf_coho_nad83col) +
  annotation_map_tile(type = "osm", zoom = 10) + 
  geom_sf(aes(fill = mean_lnnosa), alpha = 0.7) + 
  coord_sf(crs = 4269) + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "Map of population size in coho populations",
       fill = "Average pop size (1980-2024)") +
  theme_minimal()


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
  group_by(NWFSC_POP_ID) %>%
  summarize(
    mean_lnnosa = mean(mean_lnnosa, na.rm = TRUE),
    mean_a      = mean(mean_a, na.rm = TRUE),
    mean_R      = mean(mean_R, na.rm = TRUE),
    .groups = "drop"
  )

# replotting
ggplot(data = sf_chin_nad83col) +
  annotation_map_tile(type = "osm", zoom = 10) + 
  geom_sf(aes(fill = mean_a), alpha = 0.7) + 
  coord_sf(crs = 4269) + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "Map of relative bias in chin populations",
       fill = "Mean relative bias") +
  theme_minimal()

ggplot(data = sf_chin_nad83col) +
  annotation_map_tile(type = "osm", zoom = 10) + 
  geom_sf(aes(fill = mean_R), alpha = 0.7) + 
  coord_sf(crs = 4269) + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "Map of precision in chinook populations",
       fill = "Mean variance") +
  theme_minimal()

ggplot(data = sf_chin_nad83col) +
  annotation_map_tile(type = "osm", zoom = 10) + 
  geom_sf(aes(fill = mean_lnnosa), alpha = 0.7) + 
  coord_sf(crs = 4269) + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "Map of population size in chinook populations",
       fill = "Average pop size (1980-2024)") +
  theme_minimal()


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
  group_by(NWFSC_POP_ID) %>%
  summarize(
    mean_lnnosa = mean(mean_lnnosa, na.rm = TRUE),
    mean_a      = mean(mean_a, na.rm = TRUE),
    mean_R      = mean(mean_R, na.rm = TRUE),
    .groups = "drop"
  )

# replotting
ggplot(data = sf_stel_nad83col) +
  annotation_map_tile(type = "osm", zoom = 10) + 
  geom_sf(aes(fill = mean_a), alpha = 0.7) + 
  coord_sf(crs = 4269) + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "Map of relative bias in steelhead populations",
       fill = "Mean relative bias") +
  theme_minimal()

ggplot(data = sf_stel_nad83col) +
  annotation_map_tile(type = "osm", zoom = 10) + 
  geom_sf(aes(fill = mean_R), alpha = 0.7) + 
  coord_sf(crs = 4269) + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "Map of precision in steelhead populations",
       fill = "Mean variance") +
  theme_minimal()

ggplot(data = sf_stel_nad83col) +
  annotation_map_tile(type = "osm", zoom = 10) + 
  geom_sf(aes(fill = mean_lnnosa), alpha = 0.7) + 
  coord_sf(crs = 4269) + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "Map of population size in steelhead populations",
       fill = "Average pop size (1980-2024)") +
  theme_minimal()