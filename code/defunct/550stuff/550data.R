## SET WORKING DIR & PACKAGES
library(here)
library(panelr)
library(readxl)
library(stringr)
library(tidyverse)

# here::i_am("code/550models.R")
options(max.print=2000)

# import data
nosa <- read_excel(here("550", "data", "bills_nosa_data.xlsx"))

# some exploratory tables
table(nosa$species, nosa$popid)
table(nosa$species, nosa$method)
methodsTable <- nosa %>% 
  pivot_wider(names_from = "calyear", values_from = "method", id_cols = "popid")
print(methodsTable)

# different species
nosa_chin <- nosa %>% filter(species=="chin")
nosa_coho <- nosa %>% filter(species=="coho")
nosa_stel <- nosa %>% filter(species=="steel")

methodsTable_chin <- nosa_chin %>% 
  pivot_wider(names_from = "calyear", values_from = "method", id_cols = "popid")
print(methodsTable_chin)
methodsTable_coho <- nosa_coho %>% 
  pivot_wider(names_from = "calyear", values_from = "method", id_cols = "popid")
print(methodsTable_coho)
methodsTable_stel <- nosa_stel %>% 
  pivot_wider(names_from = "calyear", values_from = "method", id_cols = "popid")
print(methodsTable_stel)

## per species
# coho
unique(nosa_coho$popid)
# 27 populations
unique(nosa_coho$method)
# 15 methods including 0

# steelhead
unique(nosa_stel$popid)
# 23 populations
unique(nosa_stel$method)
# 18 methods including 0

# chinook
unique(nosa_chin$popid)
# 18 populations
unique(nosa_chin$method)
# 11 methods including 0

# throw away junk
nosa_chin <- nosa_chin[-c(2, 4, 6:10, 12, 14:48)]
nosa_coho <- nosa_coho[-c(2, 4, 6:10, 12, 14:48)]
nosa_stel <- nosa_stel[-c(2, 4, 6:10, 12, 14:48)]

# may need to clean to assure nosa = NA -> method = 0 and vice versa

# new popid/method var
nosa_chin$popmethod <- paste0(as.character(nosa_chin$popid),"_", as.character(nosa_chin$method))
nosa_coho$popmethod <- paste0(as.character(nosa_coho$popid),"_", as.character(nosa_coho$method))
nosa_stel$popmethod <- paste0(as.character(nosa_stel$popid),"_", as.character(nosa_stel$method))
# throw away junk
nosa_chin <- nosa_chin[-c(1, 3, 5)]
nosa_coho <- nosa_coho[-c(1, 3, 5)]
nosa_stel <- nosa_stel[-c(1, 3, 5)]

# set data wide (rows = popid/method, columns = year)
nosa_chin <- panel_data(nosa_chin, id = popmethod, wave = calyear)
nosa_chin <- widen_panel(nosa_chin, separator = "_")
nosa_coho <- panel_data(nosa_coho, id = popmethod, wave = calyear)
nosa_coho <- widen_panel(nosa_coho, separator = "_")
nosa_stel <- panel_data(nosa_stel, id = popmethod, wave = calyear)
nosa_stel <- widen_panel(nosa_stel, separator = "_")

# some resorting and cleaning
nosa_chin <- nosa_chin[,order(colnames(nosa_chin))]
nosa_chin_rows <- as.data.frame(stringr::str_split_fixed(nosa_chin$popmethod, "_", 2))
colnames(nosa_chin_rows) <- c("popid", "method")
nosa_chin <- nosa_chin[-c(44)]
colnames(nosa_chin) <- substr(colnames(nosa_chin), 8, 11)
years <- colnames(nosa_chin)
nosa_chin <- as.matrix(nosa_chin)
save(nosa_chin, file=here("550", "data", "nosa_chin.rda"))

nosa_coho <- nosa_coho[,order(colnames(nosa_coho))]
nosa_coho_rows <- as.data.frame(stringr::str_split_fixed(nosa_coho$popmethod, "_", 2))
colnames(nosa_coho_rows) <- c("popid", "method")
nosa_coho <- nosa_coho[-c(44)]
colnames(nosa_coho) <- substr(colnames(nosa_coho), 8, 11)
years <- colnames(nosa_coho)
nosa_coho <- as.matrix(nosa_coho)
save(nosa_coho, file=here("550", "data", "nosa_coho.rda"))

nosa_stel <- nosa_stel[,order(colnames(nosa_stel))]
nosa_stel_rows <- as.data.frame(stringr::str_split_fixed(nosa_stel$popmethod, "_", 2))
colnames(nosa_stel_rows) <- c("popid", "method")
nosa_stel <- nosa_stel[-c(44)]
colnames(nosa_stel) <- substr(colnames(nosa_stel), 8, 11)
years <- colnames(nosa_stel)
nosa_stel <- as.matrix(nosa_stel)
save(nosa_stel, file=here("550", "data", "nosa_stel.rda"))