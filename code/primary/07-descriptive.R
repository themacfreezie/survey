## SET WORKING DIR & PACKAGES
# library(gganimate)
library(ggspatial)
library(here)
library(RColorBrewer)
library(readxl)
library(sf)
library(tidyverse)
library(viridis)

here::i_am("code/primary/03-model_build.R")
options(max.print=2000)

# pull in data
nmfs_popid <- read_excel(here("data", "raw", "cap-hli.xls"), sheet = "NOSA")
load(here("data", "clean", "nosa_codes.Rda"))
nosa <- merge

# natural log of counts
nosa$lnnosa <- log(nosa$NOSA + 1)

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
nosa <- left_join(nosa, lookup_nmfs, by = "PopID")
nosa <- nosa[-c(1, 3, 5, 6, 8, 9)]

# different species
nosa_chin <- nosa %>% filter(CommonName=="Chinook Salmon")
nosa_coho <- nosa %>% filter(CommonName=="Coho Salmon")
nosa_stel <- nosa %>% filter(CommonName=="Steelhead")

# 1. Combine and clean data
all_data <- bind_rows(
  mutate(nosa_chin, Species = "Chinook"),
  mutate(nosa_coho, Species = "Coho"),
  mutate(nosa_stel, Species = "Steelhead")
) %>%
  filter(!is.na(NWFSC_POP_ID), NWFSC_POP_ID != 1)
    # this is sketchy but its being weird

# 2. Create the shuffled global color palette
set.seed(123) # Keeps your 'random' colors consistent across runs
n_pops <- length(unique(all_data$NWFSC_POP_ID))
rand_colors <- sample(colorRampPalette(brewer.pal(12, "Paired"))(n_pops))

# 3. Plot with heavier line weights
ggplot(all_data, aes(x = Year, y = lnnosa, color = factor(NWFSC_POP_ID), group = NWFSC_POP_ID)) +
  # Increased linewidth for better visibility
  geom_line(linewidth = .75, alpha = 0.85) + 
  facet_wrap(~ Species, ncol = 1, scales = "free_y") +
  scale_color_manual(values = rand_colors) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.title = element_text(size = 24, face = "bold", margin = margin(b = 10)),
    strip.text = element_text(size = 18, face = "bold", hjust = 0),
    axis.line = element_line(color = "black"),
    # Make Y-axis label larger
    axis.title.y = element_text(size = 18),
    # Remove X-axis label (title) while keeping the years (text)
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 14, color = "black"),
    axis.text.y = element_text(size = 14, color = "black")
  ) +
  labs(
    title = "Oregon salmonid populations",
    y = "ln(Natural-origin abundance)",
    x = "Year"
  )

# # bring in spatial data
# # pull in spatial layers
# gdb_path <- here("data", "raw", "WCR_Salmon_Steelhead_gdb_2015", "WCR_Salmon_Steelhead_gdb_2015.gdb")
# st_layers(gdb_path)
#   # I want 'fish' datalayer
# sf_fish <- read_sf(dsn = gdb_path, layer = "fish")
# sf_fish$DPS_IDtrunc <- substr(sf_fish$DPS_ID, 1, 5)
#   # grab DPS_ID
# sf_fish$DPStrunc <- str_remove(sf_fish$DPS, " - Outside legal area$")
# 
# # collapse to level of NWFSC_POP_ID
# sf_fishLOOKUP <- sf_fish %>%
#   st_make_valid() %>% 
#   group_by(NWFSC_POP_ID, POPULATION, DPS_IDtrunc, DPStrunc) %>%
#   summarize(SHAPE = st_union(SHAPE))
# 
# # combine
# sf_fish_combined <- nosa_coho %>%
#   left_join(sf_fishLOOKUP, by = "NWFSC_POP_ID")
# 
# sf_coho <- sf_fish_combined %>%
#   filter(!is.na(NWFSC_POP_ID))
# sf_coho <- st_as_sf(sf_coho, sf_column_name = "SHAPE")
# sf_coho <- st_transform(sf_coho, crs = 4269)
# 
# sf_fish_combined <- nosa_chin %>%
#   left_join(sf_fishLOOKUP, by = "NWFSC_POP_ID")
# 
# sf_chin <- sf_fish_combined %>%
#   filter(!is.na(NWFSC_POP_ID))
# sf_chin <- st_as_sf(sf_chin, sf_column_name = "SHAPE")
# sf_chin <- st_transform(sf_chin, crs = 4269)
# 
# sf_fish_combined <- nosa_stel %>%
#   left_join(sf_fishLOOKUP, by = "NWFSC_POP_ID")
# 
# sf_stel <- sf_fish_combined %>%
#   filter(!is.na(NWFSC_POP_ID))
# sf_stel <- st_as_sf(sf_stel, sf_column_name = "SHAPE")
# sf_stel <- st_transform(sf_stel, crs = 4269)
# 
# # time lapse
# anim_map <- ggplot(data = sf_coho) +
#   annotation_map_tile(type = "osm", zoom = 8) + 
#   geom_sf(aes(fill = NOSA, geometry = SHAPE), alpha = 0.7) +
#   scale_fill_viridis_c(option = "plasma") +
#   transition_time(Year) +
#   labs(title = "Coho Salmon Abundance",
#        subtitle = "Year: {as.integer(frame_time)}",
#        caption = "Data: 1980-2024")
# animate(anim_map, 
#         nframes = 200, 
#         fps = 10, 
#         renderer = gifski_renderer())
