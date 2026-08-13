## SET WORKING DIR & PACKAGES
library(here)
library(MARSS)
library(panelr)
library(tidyverse)

here::i_am("code/primary/03.2-ESUmodel_build.R")
options(max.print=2000)

# pull in data - nosa
load(here("data", "clean", "nosa_codes.Rda"))
nosa <- merge

# pull in data - pop list
load(here("data", "clean", "populations_list.Rda"))
pop_list <- pop_list |> 
  filter(CommonPopName != "Lostine River Spring Chinook")
pop_list <- pop_list |> 
  filter(WATERBODY != "Hood River traps: WF Moving Falls (45.571/-121.658), East Fork (45.502/-121.562), MF Parkdale Hatchery (45.524/-121.621)")
pop_list <- pop_list |> 
  filter(WATERBODY != "Upper Gorge Tributaries and Hood River")
pop_list <- pop_list |> 
  filter(WATERBODY != "North Fork Scappoose Creek, South Fork Scappoose Creek, and tributaries")
pop_list <- pop_list[-c(7, 8)]

# join
nosa <- left_join(nosa, pop_list, by = "PopID")
nosa <- nosa[nosa$ESAPOPNAME != "N/A", ]

# new popid/method var
nosa$popmethod <- paste0(as.character(nosa$PopID),"_", as.character(nosa$MethodNameID))

# natural log of counts
nosa$lnnosa <- log(nosa$NOSA + 1)

# different species
nosa_chin <- nosa %>% filter(CommonName=="Chinook Salmon")
nosa_coho <- nosa %>% filter(CommonName=="Coho Salmon")
nosa_stel <- nosa %>% filter(CommonName=="Steelhead")

# will drop those methods for which fewer than 10 observations exist
counts_chin <- table(nosa_chin$MethodNameID)
low_count_ids <- names(counts_chin[counts_chin < 10])
low_counts_chin <- as.numeric(low_count_ids)
nosa_chin <- nosa_chin %>%
  filter(!MethodNameID %in% low_counts_chin)
table(nosa_chin$MethodNameID)

counts_coho <- table(nosa_coho$MethodNameID)
low_count_ids <- names(counts_coho[counts_coho < 10])
low_counts_coho <- as.numeric(low_count_ids)
nosa_coho <- nosa_coho %>%
  filter(!MethodNameID %in% low_counts_coho)
table(nosa_coho$MethodNameID)

counts_stel <- table(nosa_stel$MethodNameID)
low_count_ids <- names(counts_stel[counts_stel < 10])
low_counts_stel <- as.numeric(low_count_ids)
nosa_stel <- nosa_stel %>%
  filter(!MethodNameID %in% low_counts_stel)
table(nosa_stel$MethodNameID)


# set up popID data for MARSS models
chin_dat <- nosa_chin[-c(1, 3:16)]
coho_dat <- nosa_coho[-c(1, 3:16)]
stel_dat <- nosa_stel[-c(1, 3:16)]

# set data wide (rows = popid/method, columns = year)
chin_dat <- panel_data(chin_dat, id = popmethod, wave = Year)
chin_dat <- widen_panel(chin_dat, separator = "_")
coho_dat <- panel_data(coho_dat, id = popmethod, wave = Year)
coho_dat <- widen_panel(coho_dat, separator = "_")
stel_dat <- panel_data(stel_dat, id = popmethod, wave = Year)
stel_dat <- widen_panel(stel_dat, separator = "_")

# some resorting and cleaning
chin_dat <- chin_dat[,order(colnames(chin_dat))]
chin_dat_rows <- as.data.frame(stringr::str_split_fixed(chin_dat$popmethod, "_", 2))
colnames(chin_dat_rows) <- c("PopID", "method")
chin_dat_rows$PopID <- as.numeric(chin_dat_rows$PopID)
chin_dat_rows$method <- as.numeric(chin_dat_rows$method)
chin_dat_rows <- left_join(chin_dat_rows, pop_list, by = "PopID")
chin_dat_rows$ESANAME <- sub(".*\\((.*?)\\).*", "\\1", chin_dat_rows$ESAPOPNAME)
# chin_dat_rows <- chin_dat_rows[chin_dat_rows$ESAPOPNAME != "N/A", ]
chin_dat_rows <- chin_dat_rows[-c(3:9)]
chin_dat_rows$ESAcode <- as.numeric(as.factor(chin_dat_rows$ESANAME))
chin_dat <- chin_dat[-c(46)]
colnames(chin_dat) <- substr(colnames(chin_dat), 8, 11)
years <- colnames(chin_dat)
chin_dat <- as.matrix(chin_dat)

coho_dat <- coho_dat[,order(colnames(coho_dat))]
coho_dat_rows <- as.data.frame(stringr::str_split_fixed(coho_dat$popmethod, "_", 2))
colnames(coho_dat_rows) <- c("PopID", "method")
coho_dat_rows$PopID <- as.numeric(coho_dat_rows$PopID)
coho_dat_rows$method <- as.numeric(coho_dat_rows$method)
coho_dat_rows <- left_join(coho_dat_rows, pop_list, by = "PopID")
coho_dat_rows$ESANAME <- sub(".*\\((.*?)\\).*", "\\1", coho_dat_rows$ESAPOPNAME)
# coho_dat_rows <- coho_dat_rows[coho_dat_rows$ESAPOPNAME != "N/A", ]
coho_dat_rows <- coho_dat_rows[-c(3:9)]
coho_dat_rows$ESAcode <- as.numeric(as.factor(coho_dat_rows$ESANAME))
coho_dat <- coho_dat[-c(46)]
colnames(coho_dat) <- substr(colnames(coho_dat), 8, 11)
years <- colnames(coho_dat)
coho_dat <- as.matrix(coho_dat)

stel_dat <- stel_dat[,order(colnames(stel_dat))]
stel_dat_rows <- as.data.frame(stringr::str_split_fixed(stel_dat$popmethod, "_", 2))
colnames(stel_dat_rows) <- c("PopID", "method")
stel_dat_rows$PopID <- as.numeric(stel_dat_rows$PopID)
stel_dat_rows$method <- as.numeric(stel_dat_rows$method)
stel_dat_rows <- left_join(stel_dat_rows, pop_list, by = "PopID")
stel_dat_rows$ESANAME <- sub(".*\\((.*?)\\).*", "\\1", stel_dat_rows$ESAPOPNAME)
# stel_dat_rows <- stel_dat_rows[stel_dat_rows$ESAPOPNAME != "N/A", ]
stel_dat_rows <- stel_dat_rows[-c(3:9)]
stel_dat_rows$ESAcode <- as.numeric(as.factor(stel_dat_rows$ESANAME))
stel_dat <- stel_dat[-c(46)]
colnames(stel_dat) <- substr(colnames(stel_dat), 8, 11)
years <- colnames(stel_dat)
stel_dat <- as.matrix(stel_dat)

# set controls
con.list <- list(maxit = 5000, allow.degen = TRUE)

## model chinook
# constructing R and a and Z
# R
n_chin <- nrow(chin_dat)
R_chin.model <- matrix(list(0), n_chin, n_chin)
diag(R_chin.model) <- paste0("r", chin_dat_rows$method)

# a
scale <- "9"
# sets relative value against which other survey methods will be scaled
# 9 -> dam counts - accurate (according to parsons and Skalski)
a_chin.model <- matrix(list(0), n_chin, 1)
for(i in 1:length(a_chin.model)){
  if(chin_dat_rows$method[i] != scale){
    a_chin.model[i] <- paste0("a", chin_dat_rows$method[i])
  }
}

# Z
pops_chin <- c(unique(chin_dat_rows$ESAcode))
Z_chin.model <- matrix(0, nrow=nrow(chin_dat), ncol=length(unique(chin_dat_rows$ESAcode)))
for(i in seq(length(pops_chin))){
  Z_chin.model[chin_dat_rows$ESAcode == pops_chin[i], i] <- 1
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
if(!file.exists(here::here("data", "clean", paste("ssm_ESUchinM", scale, ".rds", sep="")))){
  ptm <- proc.time()
  ssm_ESUchin <- MARSS(chin_dat, model = mod_chin.list, method = "kem", control = con.list)
  saveRDS(ssm_ESUchin, file=here::here("data", "clean", paste("ssm_ESUchinM", scale, ".rds", sep="")))
  chin_time <- proc.time()[3] - ptm
  chin_time
}
# load in ssm_ESUchin
ssm_ESUchin <- readRDS(file=here::here("data", "clean", paste("ssm_ESUchinM", scale, ".rds", sep="")))


## model coho
# constructing R and a and Z
# R
n_coho <- nrow(coho_dat)
R_coho.model <- matrix(list(0), n_coho, n_coho)
diag(R_coho.model) <- paste0("r", coho_dat_rows$method)

# a
scale <- "9"
# sets relative value against which other survey methods will be scaled
# 9 -> dam counts - accurate (according to parsons and Skalski)
a_coho.model <- matrix(list(0), n_coho, 1)
for(i in 1:length(a_coho.model)){
  if(coho_dat_rows$method[i] != scale){
    a_coho.model[i] <- paste0("a", coho_dat_rows$method[i])
  }
}

# Z
pops_coho <- c(unique(coho_dat_rows$ESAcode))
Z_coho.model <- matrix(0, nrow=nrow(coho_dat), ncol=length(unique(coho_dat_rows$ESAcode)))
for(i in seq(length(pops_coho))){
  Z_coho.model[coho_dat_rows$ESAcode == pops_coho[i], i] <- 1
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
if(!file.exists(here::here("data", "clean", paste("ssm_ESUcohoM", scale, ".rds", sep="")))){
  ptm <- proc.time()
  ssm_ESUcoho <- MARSS(coho_dat, model = mod_coho.list, method = "kem", control = con.list)
  saveRDS(ssm_ESUcoho, file=here::here("data", "clean", paste("ssm_ESUcohoM", scale, ".rds", sep="")))
  coho_time <- proc.time()[3] - ptm
  coho_time
}
# load in ssm_ESUcoho
ssm_ESUcoho <- readRDS(file=here::here("data", "clean", paste("ssm_ESUcohoM", scale, ".rds", sep="")))


## model steelhead
# constructing R and a and Z
# R
n_stel <- nrow(stel_dat)
R_stel.model <- matrix(list(0), n_stel, n_stel)
diag(R_stel.model) <- paste0("r", stel_dat_rows$method)

# a
scale <- "9"
# sets relative value against which other survey methods will be scaled
# 9 -> dam counts - accurate (according to parsons and Skalski)
a_stel.model <- matrix(list(0), n_stel, 1)
for(i in 1:length(a_stel.model)){
  if(stel_dat_rows$method[i] != scale){
    a_stel.model[i] <- paste0("a", stel_dat_rows$method[i])
  }
}

# Z
pops_stel <- c(unique(stel_dat_rows$ESAcode))
Z_stel.model <- matrix(0, nrow=nrow(stel_dat), ncol=length(unique(stel_dat_rows$ESAcode)))
for(i in seq(length(pops_stel))){
  Z_stel.model[stel_dat_rows$ESAcode == pops_stel[i], i] <- 1
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
if(!file.exists(here::here("data", "clean", paste("ssm_ESUstelM", scale, ".rds", sep="")))){
  ptm <- proc.time()
  ssm_ESUstel <- MARSS(stel_dat, model = mod_stel.list, method = "kem", control = con.list)
  saveRDS(ssm_ESUstel, file=here::here("data", "clean", paste("ssm_ESUstelM", scale, ".rds", sep="")))
  stel_time <- proc.time()[3] - ptm
  stel_time
}
# load in ssm_ESUstel
ssm_ESUstel <- readRDS(file=here::here("data", "clean", paste("ssm_ESUstelM", scale, ".rds", sep="")))
ssm_ESUstel
