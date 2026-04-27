## SET WORKING DIR & PACKAGES
library(here)
library(MARSS)
library(panelr)
library(tidyverse)

here::i_am("code/primary/03-model_build.R")
options(max.print=2000)

# pull in data
load(here("data", "clean", "nosa_codes.Rda"))
nosa <- merge

# how often are particular survey methods used
counts <- table(nosa$MethodNameID)
counts

# will drop those methods for which fewer than 30 observations exist
low_count_ids <- names(counts[counts < 10])
low_counts <- as.numeric(low_count_ids)

nosa <- nosa %>%
  filter(!MethodNameID %in% low_counts)
table(nosa$MethodNameID)

# new popid/method var
nosa$popmethod <- paste0(as.character(nosa$PopID),"_", as.character(nosa$MethodNameID))

# natural log of counts
nosa$lnnosa <- log(nosa$NOSA + 1)

# different species
nosa_chin <- nosa %>% filter(CommonName=="Chinook Salmon")
nosa_coho <- nosa %>% filter(CommonName=="Coho Salmon")
nosa_stel <- nosa %>% filter(CommonName=="Steelhead")

## vital information
methodsTable_chin <- nosa_chin %>% 
  pivot_wider(names_from = "Year", values_from = "MethodNameID", id_cols = "PopID")
print(methodsTable_chin)
methodsTable_coho <- nosa_coho %>% 
  pivot_wider(names_from = "Year", values_from = "MethodNameID", id_cols = "PopID")
print(methodsTable_coho)
methodsTable_stel <- nosa_stel %>% 
  pivot_wider(names_from = "Year", values_from = "MethodNameID", id_cols = "PopID")
print(methodsTable_stel)

## per species
# coho
length(unique(nosa_coho$PopID))
  # 29 populations
unique(nosa_coho$MethodNameID)
  # 8 methods
table(nosa_coho$MethodNameID)
  # method 9 only appears 26 times
  # method 11 only appears 22 times
# nosa_coho <- nosa_coho[nosa_coho$MethodNameID != 9, ]
# nosa_coho <- nosa_coho[nosa_coho$MethodNameID != 11, ]

# steelhead
length(unique(nosa_stel$PopID))
  # 23 populations
unique(nosa_stel$MethodNameID)
  # 5 methods
table(nosa_stel$MethodNameID)

# chinook
length(unique(nosa_chin$PopID))
  # 21 populations
unique(nosa_chin$MethodNameID)
  # 9 methods including 0
table(nosa_chin$MethodNameID)

# throw away junk
nosa_chinFULL <- nosa_chin
nosa_cohoFULL <- nosa_coho
nosa_stelFULL <- nosa_stel

nosa_chin <- nosa_chin[-c(1, 3, 4:9)]
nosa_coho <- nosa_coho[-c(1, 3, 4:9)]
nosa_stel <- nosa_stel[-c(1, 3, 4:9)]

# set data wide (rows = popid/method, columns = year)
nosa_chin <- panel_data(nosa_chin, id = popmethod, wave = Year)
nosa_chin <- widen_panel(nosa_chin, separator = "_")
nosa_coho <- panel_data(nosa_coho, id = popmethod, wave = Year)
nosa_coho <- widen_panel(nosa_coho, separator = "_")
nosa_stel <- panel_data(nosa_stel, id = popmethod, wave = Year)
nosa_stel <- widen_panel(nosa_stel, separator = "_")

# some resorting and cleaning
nosa_chin <- nosa_chin[,order(colnames(nosa_chin))]
nosa_chin_rows <- as.data.frame(stringr::str_split_fixed(nosa_chin$popmethod, "_", 2))
colnames(nosa_chin_rows) <- c("popid", "method")
nosa_chin <- nosa_chin[-c(46)]
colnames(nosa_chin) <- substr(colnames(nosa_chin), 8, 11)
years <- colnames(nosa_chin)
nosa_chin <- as.matrix(nosa_chin)

nosa_coho <- nosa_coho[,order(colnames(nosa_coho))]
nosa_coho_rows <- as.data.frame(stringr::str_split_fixed(nosa_coho$popmethod, "_", 2))
colnames(nosa_coho_rows) <- c("popid", "method")
nosa_coho <- nosa_coho[-c(46)]
colnames(nosa_coho) <- substr(colnames(nosa_coho), 8, 11)
years <- colnames(nosa_coho)
nosa_coho <- as.matrix(nosa_coho)

nosa_stel <- nosa_stel[,order(colnames(nosa_stel))]
nosa_stel_rows <- as.data.frame(stringr::str_split_fixed(nosa_stel$popmethod, "_", 2))
colnames(nosa_stel_rows) <- c("popid", "method")
nosa_stel <- nosa_stel[-c(46)]
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
scale <- "9"
  # sets relative value against which other survey methods will be scaled
  # 9 -> dam counts - accurate (according to parsons and Skalski)
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
if(!file.exists(here::here("data", "clean", paste("ssm_chinM", scale, ".rds", sep="")))){
  ptm <- proc.time()
  ssm_chin <- MARSS(nosa_chin, model = mod_chin.list, method = "kem", control = con.list)
  saveRDS(ssm_chin, file=here::here("data", "clean", paste("ssm_chinM", scale, ".rds", sep="")))
  chin_time <- proc.time()[3] - ptm
  chin_time
}
# load in ssm_chin
ssm_chin <- readRDS(file=here::here("data", "clean", paste("ssm_chinM", scale, ".rds", sep="")))

## model coho
# constructing R and a and Z
# R
n_coho <- nrow(nosa_coho)
R_coho.model <- matrix(list(0), n_coho, n_coho)
diag(R_coho.model) <- paste0("r", nosa_coho_rows$method)

# a
scale <- "9"
  # 9 -> dam counts - accurate (according to parsons and Skalski)
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
if(!file.exists(here::here("data", "clean", paste("ssm_cohoM", scale, ".rds", sep="")))){
  ptm <- proc.time()
  ssm_coho <- MARSS(nosa_coho, model = mod_coho.list, method = "kem", control = con.list)
  saveRDS(ssm_coho, file=here::here("data", "clean", paste("ssm_cohoM", scale, ".rds", sep="")))
  coho_time <- proc.time()[3] - ptm
  coho_time
}
# load in ssm_coho
ssm_coho <- readRDS(file=here::here("data", "clean", paste("ssm_cohoM", scale, ".rds", sep="")))

## model steelhead
# constructing R and a and Z
# R
n_stel <- nrow(nosa_stel)
R_stel.model <- matrix(list(0), n_stel, n_stel)
diag(R_stel.model) <- paste0("r", nosa_stel_rows$method)

# a
scale <- "9"
  # 9 -> dam counts - accurate (according to parsons and Skalski)
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
if(!file.exists(here::here("data", "clean", paste("ssm_stelM", scale, ".rds", sep="")))){
  ptm <- proc.time()
  ssm_stel <- MARSS(nosa_stel, model = mod_stel.list, method = "kem", control = con.list)
  saveRDS(ssm_stel, file=here::here("data", "clean", paste("ssm_stelM", scale, ".rds", sep="")))
  stel_time <- proc.time()[3] - ptm
  stel_time
}
# load in ssm_stel
ssm_stel <- readRDS(file=here::here("data", "clean", paste("ssm_stelM", scale, ".rds", sep="")))