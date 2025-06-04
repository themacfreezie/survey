## SET WORKING DIR & PACKAGES

library(here)
library(MARSS)
library(marssTMB)
library(panelr)
library(readxl)
library(stringr)
library(tidyverse)

here::i_am("code/550methods_pilot.R")
options(max.print=2000)

# import data
nosa <- read_excel(here("data", "bills_nosa_data.xlsx"))

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

# we'll look at chinook
unique(nosa_chin$popid)
  # 27 populations
unique(nosa_chin$method)
  # 15 methods including 0

# selecting two populations with some overlapping methods
table(nosa_chin$popid, nosa_chin$method)
data <- nosa_chin %>% filter(popid==8|popid==11)
# data <- nosa_chin

# throw away junk
data <- data[-c(2, 4, 6:10, 12, 14:48)]

# may need to clean to assure nosa = NA -> method = 0 and vice versa

# new popid/method var
data$popmethod <- paste0(as.character(data$popid),"_", as.character(data$method))
# throw away junk
data <- data[-c(1, 3, 5)]

# set data wide (rows = popid/method, columns = year)
data <- panel_data(data, id = popmethod, wave = calyear)
data <- widen_panel(data, separator = "_")

# some resorting and cleaning
data <- data[,order(colnames(data))]
data_rows <- as.data.frame(stringr::str_split_fixed(data$popmethod, "_", 2))
colnames(data_rows) <- c("popid", "method")
data <- data[-c(44)]
colnames(data) <- substr(colnames(data), 8, 11)
years <- colnames(data)
data <- as.matrix(data)

# constructing R and a and Z
# R
n <- nrow(data)
R.model <- matrix(list(0), n, n)
diag(R.model) <- paste0("r", data_rows$method)

# a
scale <- "21"
  # sets relative value against which other survey methods will be scaled
a.model <- matrix(list(0), n, 1)
for(i in 1:length(a.model)){
  if(data_rows$method[i] != scale){
    a.model[i] <- paste0("a", data_rows$method[i])
  }
}

# Z
pops <- c(unique(data_rows$popid))
Z.model <- matrix(0, nrow=nrow(data), ncol=length(unique(data_rows$popid)))
for(i in seq(length(pops))){
  Z.model[data_rows$popid == pops[i], i] <- 1
}

# model list
mod.list <- list(
  B = "identity",
  U = "zero",
  Q = "diagonal and unequal",
  Z = Z.model,
  A = a.model,
  R = R.model,
  x0 = "equal",
  V0 = "zero",
  tinitx = 0
)

# controls
con.list <- list(maxit = 3000, allow.degen = TRUE)

# run MARSS model
ssm <- MARSS(data, model = mod.list, method = "kem", control = con.list)
ssm$AICc
