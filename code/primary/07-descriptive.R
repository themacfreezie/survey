## SET WORKING DIR & PACKAGES
# library(gganimate)
library(DT)
library(ggspatial)
library(gt)
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

# methods by ESU
load(here("data", "clean", "chin_modeldat.Rda"))
load(here("data", "clean", "coho_modeldat.Rda"))
load(here("data", "clean", "stel_modeldat.Rda"))

# pop list
load(here("data", "clean", "populations_list.Rda"))
pop_list <- pop_list |> 
  filter(CommonPopName != "Lostine River Spring Chinook")
pop_list <- pop_list |> 
  filter(WATERBODY != "Hood River traps: WF Moving Falls (45.571/-121.658), East Fork (45.502/-121.562), MF Parkdale Hatchery (45.524/-121.621)")
pop_list <- pop_list |> 
  filter(WATERBODY != "Upper Gorge Tributaries and Hood River")
pop_list <- pop_list |> 
  filter(WATERBODY != "North Fork Scappoose Creek, South Fork Scappoose Creek, and tributaries")

# join
chin_pops <- left_join(nosa_chin, pop_list, by = "PopID")
chin_pops$ESAPOPNAME <- sub(".*\\((.*?)\\).*", "\\1", chin_pops$ESAPOPNAME)
chin_pops <- chin_pops[chin_pops$ESAPOPNAME != "N/A", ]

coho_pops <- left_join(nosa_coho, pop_list, by = "PopID")
coho_pops$ESAPOPNAME <- sub(".*\\((.*?)\\).*", "\\1", coho_pops$ESAPOPNAME)
coho_pops <- coho_pops[coho_pops$ESAPOPNAME != "N/A", ]

stel_pops <- left_join(nosa_stel, pop_list, by = "PopID")
stel_pops$ESAPOPNAME <- sub(".*\\((.*?)\\).*", "\\1", stel_pops$ESAPOPNAME)
stel_pops <- stel_pops[stel_pops$ESAPOPNAME != "N/A", ]

# table
ESUmethods_chin <- table(chin_pops$MethodName, chin_pops$ESAPOPNAME)
print(ESUmethods_chin)
ESUmethods_chin <- as.data.frame.matrix(ESUmethods_chin)

ESUmethods_coho <- table(coho_pops$MethodName, coho_pops$ESAPOPNAME)
print(ESUmethods_coho)
ESUmethods_coho <- as.data.frame.matrix(ESUmethods_coho)

ESUmethods_stel <- table(stel_pops$MethodName, stel_pops$ESAPOPNAME)
print(ESUmethods_stel)
ESUmethods_stel <- as.data.frame.matrix(ESUmethods_stel)

ESUmethods_chin_table <- ESUmethods_chin %>%
  gt(rownames_to_stub = TRUE) %>% 
  tab_header(
    title = "Survey methods by ESU - Chinook salmon",
  ) %>%
  tab_options(
    table.width = pct(100),
    data_row.padding = px(5)    
  )
ESUmethods_chin_table

ESUmethods_coho_table <- ESUmethods_coho %>%
  gt(rownames_to_stub = TRUE) %>% 
  tab_header(
    title = "Survey methods by ESU - coho salmon",
  ) %>%
  tab_options(
    table.width = pct(100),
    data_row.padding = px(5)    
  )
ESUmethods_coho_table

ESUmethods_stel_table <- ESUmethods_stel %>%
  gt(rownames_to_stub = TRUE) %>% 
  tab_header(
    title = "Survey methods by DPS - steelhead trout",
  ) %>%
  tab_options(
    table.width = pct(100),
    data_row.padding = px(5)    
  )
ESUmethods_stel_table
