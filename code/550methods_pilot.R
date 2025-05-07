## SET WORKING DIR & PACKAGES

library(here)
library(MARSS)
library(marssTMB)
library(panelr)
library(patchwork)
library(readxl)
library(showtext)
library(tidyverse)

here::i_am("code/survey_sandbox.R")
options(max.print=2000)

showtext_auto()
showtext_opts(dpi=300)

# import data
nosa <- read_excel(here("data", "bills_nosa_data.xlsx"))
methods <- read_excel(here("data", "METHODS_ODFW_Recompiled2_and_figs.xlsx"), sheet = "data-wide")

# we'll look at coho
nosa_chin <- nosa %>% filter(species=="chin")
unique(nosa_chin$popid)
  # 27 populations
unique(nosa_chin$method)
  # 15 methods including 0

# selecting two populations with some overlapping methods
table(nosa_chin$popid, nosa_chin$method)
pilot <- nosa_chin %>% filter(popid==8|popid==11)

# throw away junk
pilot <- pilot[-c(2, 4, 6:10, 12, 14:48)]

# may need to clean to assure nosa = NA -> method = 0 and vice versa

# new popid/method var
pilot$popmethod <- paste0(as.character(pilot$popid),"_", as.character(pilot$method))
# throw away junk
data <- pilot[-c(1, 3, 5)]

# set data wide (rows = popid/method, columns = year)
data <- panel_data(data, id = popmethod, wave = calyear)
data <- widen_panel(data, separator = "_")

# some resorting and cleaning
data <- data[,order(colnames(data))]
data_rows <- data$popmethod
data <- data[-c(44)]
colnames(data) <- substr(colnames(data), 8, 11)
years <- colnames(data)
