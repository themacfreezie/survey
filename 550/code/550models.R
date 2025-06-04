## SET WORKING DIR & PACKAGES
library(here)
library(MARSS)
library(marssTMB)
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

nosa_coho <- nosa_coho[,order(colnames(nosa_coho))]
nosa_coho_rows <- as.data.frame(stringr::str_split_fixed(nosa_coho$popmethod, "_", 2))
colnames(nosa_coho_rows) <- c("popid", "method")
nosa_coho <- nosa_coho[-c(44)]
colnames(nosa_coho) <- substr(colnames(nosa_coho), 8, 11)
years <- colnames(nosa_coho)
nosa_coho <- as.matrix(nosa_coho)

nosa_stel <- nosa_stel[,order(colnames(nosa_stel))]
nosa_stel_rows <- as.data.frame(stringr::str_split_fixed(nosa_stel$popmethod, "_", 2))
colnames(nosa_stel_rows) <- c("popid", "method")
nosa_stel <- nosa_stel[-c(44)]
colnames(nosa_stel) <- substr(colnames(nosa_stel), 8, 11)
years <- colnames(nosa_stel)
nosa_stel <- as.matrix(nosa_stel)

# set controls
con.list <- list(maxit = 5000, allow.degen = TRUE)

## model chinook
# constructing R and a and Z
# R
n_chin <- nrow(nosa_chin)
R_chin.model <- matrix(list(0), n_chin, n_chin)
diag(R_chin.model) <- paste0("r", nosa_chin_rows$method)

# a
scale <- "11"
  # sets relative value against which other survey methods will be scaled
a_chin.model <- matrix(list(0), n_chin, 1)
for(i in 1:length(a_chin.model)){
  if(nosa_chin_rows$method[i] != scale){
    a_chin.model[i] <- paste0("a", nosa_chin_rows$method[i])
  }
}

# Z
pops_chin <- c(unique(nosa_chin_rows$popid))
Z_chin.model <- matrix(0, nrow=nrow(nosa_chin), ncol=length(unique(nosa_chin_rows$popid)))
for(i in seq(length(pops_chin))){
  Z_chin.model[nosa_chin_rows$popid == pops_chin[i], i] <- 1
}

# model list
mod_chin.list <- list(
  B = "identity",
  U = "zero",
  Q = "diagonal and unequal",
  Z = Z_chin.model,
  A = a_chin.model,
  R = R_chin.model,
  x0 = "equal",
  V0 = "zero",
  tinitx = 0
)

# run MARSS model
if(!file.exists(here::here("550", "data", "ssm_chin.rds"))){
  ssm_chin <- MARSS(nosa_chin, model = mod_chin.list, method = "kem", control = con.list)
  saveRDS(ssm_chin, file=here::here("550", "data", "ssm_chin.rds"))
}
# load in ssm_chin
ssm_chin <- readRDS(file=here::here("550", "data", "ssm_chin.rds"))

## model coho
# constructing R and a and Z
# R
n_coho <- nrow(nosa_coho)
R_coho.model <- matrix(list(0), n_coho, n_coho)
diag(R_coho.model) <- paste0("r", nosa_coho_rows$method)

# a
scale <- "11"
  # sets relative value against which other survey methods will be scaled
a_coho.model <- matrix(list(0), n_coho, 1)
for(i in 1:length(a_coho.model)){
  if(nosa_coho_rows$method[i] != scale){
    a_coho.model[i] <- paste0("a", nosa_coho_rows$method[i])
  }
}

# Z
pops_coho <- c(unique(nosa_coho_rows$popid))
Z_coho.model <- matrix(0, nrow=nrow(nosa_coho), ncol=length(unique(nosa_coho_rows$popid)))
for(i in seq(length(pops_coho))){
  Z_coho.model[nosa_coho_rows$popid == pops_coho[i], i] <- 1
}

# model list
mod_coho.list <- list(
  B = "identity",
  U = "zero",
  Q = "diagonal and unequal",
  Z = Z_coho.model,
  A = a_coho.model,
  R = R_coho.model,
  x0 = "equal",
  V0 = "zero",
  tinitx = 0
)

# run MARSS model
if(!file.exists(here::here("550", "data", "ssm_coho.rds"))){
  ssm_coho <- MARSS(nosa_coho, model = mod_coho.list, method = "kem", control = con.list)
  saveRDS(ssm_coho, file=here::here("550", "data", "ssm_coho.rds"))
}
# load in ssm_coho
ssm_coho <- readRDS(file=here::here("550", "data", "ssm_coho.rds"))

## model steelhead
# constructing R and a and Z
# R
n_stel <- nrow(nosa_stel)
R_stel.model <- matrix(list(0), n_stel, n_stel)
diag(R_stel.model) <- paste0("r", nosa_stel_rows$method)

# a
scale <- "11"
# sets relative value against which other survey methods will be scaled
a_stel.model <- matrix(list(0), n_stel, 1)
for(i in 1:length(a_stel.model)){
  if(nosa_stel_rows$method[i] != scale){
    a_stel.model[i] <- paste0("a", nosa_stel_rows$method[i])
  }
}

# Z
pops_stel <- c(unique(nosa_stel_rows$popid))
Z_stel.model <- matrix(0, nrow=nrow(nosa_stel), ncol=length(unique(nosa_stel_rows$popid)))
for(i in seq(length(pops_stel))){
  Z_stel.model[nosa_stel_rows$popid == pops_stel[i], i] <- 1
}

# model list
mod_stel.list <- list(
  B = "identity",
  U = "zero",
  Q = "diagonal and unequal",
  Z = Z_stel.model,
  A = a_stel.model,
  R = R_stel.model,
  x0 = "equal",
  V0 = "zero",
  tinitx = 0
)

# run MARSS model
if(!file.exists(here::here("550", "data", "ssm_stel.rds"))){
  ssm_stel <- MARSS(nosa_stel, model = mod_stel.list, method = "kem", control = con.list)
  saveRDS(ssm_stel, file=here::here("550", "data", "ssm_stel.rds"))
}
# load in ssm_stel
ssm_stel <- readRDS(file=here::here("550", "data", "ssm_stel.rds"))
