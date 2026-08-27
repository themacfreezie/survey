## SET WORKING DIR & PACKAGES
library(here)
library(MARSS)
library(panelr)
library(tidyverse)

here::i_am("code/primary/03.3-POPSIZEmodel_build.R")
options(max.print=2000)

# pull in data
load(here("data", "clean", "nosa_codes.Rda"))
nosa <- merge

# natural log of counts
nosa$lnnosa <- log(nosa$NOSA + 1)

# different species
nosa_chin <- nosa %>% filter(CommonName=="Chinook Salmon")
nosa_coho <- nosa %>% filter(CommonName=="Coho Salmon")
nosa_stel <- nosa %>% filter(CommonName=="Steelhead")

# what is average population observation per species (this is rough)
mean(nosa_chin$lnnosa)
  # 5.563811
mean(nosa_coho$lnnosa)
  # 7.537512
mean(nosa_stel$lnnosa)
  # 6.786005

# create scale variable to track whether a population is observed to be "large" or "small" based on average
nosa_chin$scale <- ifelse(nosa_chin$lnnosa > mean(nosa_chin$lnnosa), "L", "S")
nosa_coho$scale <- ifelse(nosa_coho$lnnosa > mean(nosa_coho$lnnosa), "L", "S")
nosa_stel$scale <- ifelse(nosa_stel$lnnosa > mean(nosa_stel$lnnosa), "L", "S")
  # is there some issue with basing "large" and "small" off nosa observations when they may be directionally biased?

# new method varIDs including size
nosa_chin$MethodSizeID <- paste0(as.character(nosa_chin$MethodNameID),"", as.character(nosa_chin$scale))
nosa_coho$MethodSizeID <- paste0(as.character(nosa_coho$MethodNameID),"", as.character(nosa_coho$scale))
nosa_stel$MethodSizeID <- paste0(as.character(nosa_stel$MethodNameID),"", as.character(nosa_stel$scale))

# new popid/method var
nosa_chin$popmethod <- paste0(as.character(nosa_chin$PopID),"_", as.character(nosa_chin$MethodSizeID))
nosa_coho$popmethod <- paste0(as.character(nosa_coho$PopID),"_", as.character(nosa_coho$MethodSizeID))
nosa_stel$popmethod <- paste0(as.character(nosa_stel$PopID),"_", as.character(nosa_stel$MethodSizeID))

# still issues with pop 11 somehow (chin)
nosa_chin <- nosa_chin %>%
  filter(TimeSeriesID != 599005)

# how often are particular survey methods used
counts_chin <- table(nosa_chin$MethodSizeID)
counts_chin
# 4 methods < 10 obs
counts_coho <- table(nosa_coho$MethodSizeID)
counts_coho
# 5 methods < 10 obs
counts_stel <- table(nosa_stel$MethodSizeID)
counts_stel
# 7 methods < 10 obs

# will drop those methods for which fewer than 10 observations exist
low_counts_chin <- names(counts_chin[counts_chin < 10])
nosa_chin <- nosa_chin %>%
  filter(!MethodSizeID %in% low_counts_chin)
table(nosa_chin$MethodSizeID)

low_counts_coho <- names(counts_coho[counts_coho < 10])
nosa_coho <- nosa_coho %>%
  filter(!MethodSizeID %in% low_counts_coho)
table(nosa_coho$MethodSizeID)

low_counts_stel <- names(counts_stel[counts_stel < 10])
nosa_stel <- nosa_stel %>%
  filter(!MethodSizeID %in% low_counts_stel)
table(nosa_stel$MethodSizeID)

## vital information - I'm not sure if this gets used?
methodsTable_chin <- nosa_chin %>% 
  pivot_wider(names_from = "Year", values_from = "MethodSizeID", id_cols = "PopID")
print(methodsTable_chin)
methodsTable_coho <- nosa_coho %>% 
  pivot_wider(names_from = "Year", values_from = "MethodSizeID", id_cols = "PopID")
print(methodsTable_coho)
methodsTable_stel <- nosa_stel %>% 
  pivot_wider(names_from = "Year", values_from = "MethodSizeID", id_cols = "PopID")
print(methodsTable_stel)

# save these for reference to model dat
save(nosa_chin, file=here("data", "clean", "POPSIZEchin_modeldat.Rda"))
save(nosa_coho, file=here("data", "clean", "POPSIZEcoho_modeldat.Rda"))
save(nosa_stel, file=here("data", "clean", "POPSIZEstel_modeldat.Rda"))

## per species
# chinook
length(unique(nosa_chin$PopID))
  # 22 populations
unique(nosa_chin$MethodSizeID)
length(unique(nosa_chin$MethodSizeID))
  # 14 methods

# coho
length(unique(nosa_coho$PopID))
  # 29 populations
unique(nosa_coho$MethodSizeID)
length(unique(nosa_coho$MethodSizeID))
  # 17 methods

# steelhead
length(unique(nosa_stel$PopID))
  # 23 populations
unique(nosa_stel$MethodSizeID)
length(unique(nosa_stel$MethodSizeID))
  # 13 methods

# set up popID data for MARSS models
nosa_chin <- nosa_chin[-c(1, 3:9, 11, 12)]
nosa_coho <- nosa_coho[-c(1, 3:9, 11, 12)]
nosa_stel <- nosa_stel[-c(1, 3:9, 11, 12)]

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

# build model chinook
# R
n_chin <- nrow(nosa_chin)
R_chin.model <- matrix(list(0), n_chin, n_chin)
diag(R_chin.model) <- paste0("r", nosa_chin_rows$method)

# a
scale <- "9S"
  # sets relative value against which other survey methods will be scaled
  # 9 -> dam counts - accurate (according to parsons and Skalski)
    #9s is the only 9 method that appears across all three species
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
  Q = "equalvarcov",
  Z = Z_chin.model,
  A = a_chin.model,
  R = R_chin.model,
  x0 = "equal",
  V0 = "zero",
  tinitx = 0
)

# build model coho
# R
n_coho <- nrow(nosa_coho)
R_coho.model <- matrix(list(0), n_coho, n_coho)
diag(R_coho.model) <- paste0("r", nosa_coho_rows$method)

# a
scale <- "9S"
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
  Q = "equalvarcov",
  Z = Z_coho.model,
  A = a_coho.model,
  R = R_coho.model,
  x0 = "equal",
  V0 = "zero",
  tinitx = 0
)

# build model steelhead
# R
n_stel <- nrow(nosa_stel)
R_stel.model <- matrix(list(0), n_stel, n_stel)
diag(R_stel.model) <- paste0("r", nosa_stel_rows$method)

# a
scale <- "9S"
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
  Q = "equalvarcov",
  Z = Z_stel.model,
  A = a_stel.model,
  R = R_stel.model,
  x0 = "equal",
  V0 = "zero",
  tinitx = 0
)

# run MARSS model - chin
if(!file.exists(here::here("data", "clean", paste("POPSIZE2ssm_chinM", scale, ".rds", sep="")))){
  ptm <- proc.time()
  POPSIZE2ssm_chin <- MARSS(nosa_chin, model = mod_chin.list, method = "kem", control = con.list)
  saveRDS(POPSIZE2ssm_chin, file=here::here("data", "clean", paste("POPSIZE2ssm_chinM", scale, ".rds", sep="")))
  chin_time <- proc.time()[3] - ptm
  chin_time
}
# load in ssm_chin
POPSIZE2ssm_chin <- readRDS(file=here::here("data", "clean", paste("POPSIZE2ssm_chinM", scale, ".rds", sep="")))


# run MARSS model - coho
if(!file.exists(here::here("data", "clean", paste("POPSIZE2ssm_cohoM", scale, ".rds", sep="")))){
  ptm <- proc.time()
  POPSIZE2ssm_coho <- MARSS(nosa_coho, model = mod_coho.list, method = "kem", control = con.list)
  saveRDS(POPSIZE2ssm_coho, file=here::here("data", "clean", paste("POPSIZE2ssm_cohoM", scale, ".rds", sep="")))
  coho_time <- proc.time()[3] - ptm
  coho_time
}
# load in ssm_coho
POPSIZE2ssm_coho <- readRDS(file=here::here("data", "clean", paste("POPSIZE2ssm_cohoM", scale, ".rds", sep="")))

# run MARSS model - stel
if(!file.exists(here::here("data", "clean", paste("POPSIZE2ssm_stelM", scale, ".rds", sep="")))){
  ptm <- proc.time()
  POPSIZE2ssm_stel <- MARSS(nosa_stel, model = mod_stel.list, method = "kem", control = con.list)
  saveRDS(POPSIZE2ssm_stel, file=here::here("data", "clean", paste("POPSIZE2ssm_stelM", scale, ".rds", sep="")))
  stel_time <- proc.time()[3] - ptm
  stel_time
}
# load in ssm_stel
POPSIZE2ssm_stel <- readRDS(file=here::here("data", "clean", paste("POPSIZE2ssm_stelM", scale, ".rds", sep="")))

# load in non-popsize models for comparison
ssm_chin <- readRDS(file=here::here("data", "clean", "ssm_chinM9.rds"))
ssm_coho <- readRDS(file=here::here("data", "clean", "ssm_cohoM9.rds"))
ssm_stel <- readRDS(file=here::here("data", "clean", "ssm_stelM9.rds"))

# check for best fit
# check fit
ssm_chin$AICc
  # 1569.68
POPSIZE2ssm_chin$AICc
  # 1296.671 - best fit

ssm_coho$AICc
  # 2218.874
POPSIZE2ssm_coho$AICc
  # 1871.073 - best fit

ssm_stel$AICc
  # 1215.657
POPSIZE2ssm_stel$AICc
  # 873.6264 - best fit

## SEPARATING SURVEY METHODS BY POPSIZE IS MUCH BETTER FIT
  # Okay, but to what extent do I slice the data - i.e. small and large, small medium large, etc?

## repeat the above process but with 3rds
# pull in data
load(here("data", "clean", "nosa_codes.Rda"))
nosa <- merge

# natural log of counts
nosa$lnnosa <- log(nosa$NOSA + 1)

# different species
nosa_chin <- nosa %>% filter(CommonName=="Chinook Salmon")
nosa_coho <- nosa %>% filter(CommonName=="Coho Salmon")
nosa_stel <- nosa %>% filter(CommonName=="Steelhead")

# create scale variable to track whether a population is observed to be "large", "medium", or "small" based on average
cutoffs <- quantile(nosa_chin$lnnosa, probs = c(1/3, 2/3), na.rm = TRUE)
nosa_chin$scale <- ifelse(nosa_chin$lnnosa <= cutoffs[1], "S",
                          ifelse(nosa_chin$lnnosa <= cutoffs[2], "M", "L"))

cutoffs <- quantile(nosa_coho$lnnosa, probs = c(1/3, 2/3), na.rm = TRUE)
nosa_coho$scale <- ifelse(nosa_coho$lnnosa <= cutoffs[1], "S",
                          ifelse(nosa_coho$lnnosa <= cutoffs[2], "M", "L"))

cutoffs <- quantile(nosa_stel$lnnosa, probs = c(1/3, 2/3), na.rm = TRUE)
nosa_stel$scale <- ifelse(nosa_stel$lnnosa <= cutoffs[1], "S",
                          ifelse(nosa_stel$lnnosa <= cutoffs[2], "M", "L"))

# new method varIDs including size
nosa_chin$MethodSizeID <- paste0(as.character(nosa_chin$MethodNameID),"", as.character(nosa_chin$scale))
nosa_coho$MethodSizeID <- paste0(as.character(nosa_coho$MethodNameID),"", as.character(nosa_coho$scale))
nosa_stel$MethodSizeID <- paste0(as.character(nosa_stel$MethodNameID),"", as.character(nosa_stel$scale))

# new popid/method var
nosa_chin$popmethod <- paste0(as.character(nosa_chin$PopID),"_", as.character(nosa_chin$MethodSizeID))
nosa_coho$popmethod <- paste0(as.character(nosa_coho$PopID),"_", as.character(nosa_coho$MethodSizeID))
nosa_stel$popmethod <- paste0(as.character(nosa_stel$PopID),"_", as.character(nosa_stel$MethodSizeID))

# still issues with pop 11 somehow (chin)
nosa_chin <- nosa_chin %>%
  filter(TimeSeriesID != 599005)

# how often are particular survey methods used
    # losing some more information here... 
counts_chin <- table(nosa_chin$MethodSizeID)
counts_chin
  # 7 methods < 10 obs
counts_coho <- table(nosa_coho$MethodSizeID)
counts_coho
  # 14 methods < 10 obs
counts_stel <- table(nosa_stel$MethodSizeID)
counts_stel
  # 10 methods < 10 obs

# will drop those methods for which fewer than 10 observations exist
low_counts_chin <- names(counts_chin[counts_chin < 10])
nosa_chin <- nosa_chin %>%
  filter(!MethodSizeID %in% low_counts_chin)
table(nosa_chin$MethodSizeID)
  # dropped 30 obs

low_counts_coho <- names(counts_coho[counts_coho < 10])
nosa_coho <- nosa_coho %>%
  filter(!MethodSizeID %in% low_counts_coho)
table(nosa_coho$MethodSizeID)
  # dropped 66 obs

low_counts_stel <- names(counts_stel[counts_stel < 10])
nosa_stel <- nosa_stel %>%
  filter(!MethodSizeID %in% low_counts_stel)
table(nosa_stel$MethodSizeID)
  # dropped 43 obs

## vital information - I'm not sure if this gets used?
methodsTable_chin <- nosa_chin %>% 
  pivot_wider(names_from = "Year", values_from = "MethodSizeID", id_cols = "PopID")
print(methodsTable_chin)
methodsTable_coho <- nosa_coho %>% 
  pivot_wider(names_from = "Year", values_from = "MethodSizeID", id_cols = "PopID")
print(methodsTable_coho)
methodsTable_stel <- nosa_stel %>% 
  pivot_wider(names_from = "Year", values_from = "MethodSizeID", id_cols = "PopID")
print(methodsTable_stel)

# save these for reference to model dat
save(nosa_chin, file=here("data", "clean", "POPSIZEchin_modeldat.Rda"))
save(nosa_coho, file=here("data", "clean", "POPSIZEcoho_modeldat.Rda"))
save(nosa_stel, file=here("data", "clean", "POPSIZEstel_modeldat.Rda"))

## per species
# chinook
length(unique(nosa_chin$PopID))
  # 22 populations
unique(nosa_chin$MethodSizeID)
length(unique(nosa_chin$MethodSizeID))
  # 19 methods

# coho
length(unique(nosa_coho$PopID))
  # 29 populations
unique(nosa_coho$MethodSizeID)
length(unique(nosa_coho$MethodSizeID))
  # 18 methods

# steelhead
length(unique(nosa_stel$PopID))
  # 23 populations
unique(nosa_stel$MethodSizeID)
length(unique(nosa_stel$MethodSizeID))
  # 16 methods

# set up popID data for MARSS models
nosa_chin <- nosa_chin[-c(1, 3:9, 11, 12)]
nosa_coho <- nosa_coho[-c(1, 3:9, 11, 12)]
nosa_stel <- nosa_stel[-c(1, 3:9, 11, 12)]

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
nosa_coho <- nosa_coho[-c(44)]
  # lost 1985 and 1986 entirely from dropping observations
nosa_coho$lnnosa_1985 <- NA_real_
nosa_coho$lnnosa_1986 <- NA_real_
nosa_coho <- nosa_coho[,order(colnames(nosa_coho))]
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

# build model chinook
# R
n_chin <- nrow(nosa_chin)
R_chin.model <- matrix(list(0), n_chin, n_chin)
diag(R_chin.model) <- paste0("r", nosa_chin_rows$method)

# a
scale <- "9S"
# sets relative value against which other survey methods will be scaled
# 9 -> dam counts - accurate (according to parsons and Skalski)
#9s is the only 9 method that appears across all three species
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
  Q = "equalvarcov",
  Z = Z_chin.model,
  A = a_chin.model,
  R = R_chin.model,
  x0 = "equal",
  V0 = "zero",
  tinitx = 0
)

# build model coho
# R
n_coho <- nrow(nosa_coho)
R_coho.model <- matrix(list(0), n_coho, n_coho)
diag(R_coho.model) <- paste0("r", nosa_coho_rows$method)

# a
scale <- "9S"
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
  Q = "equalvarcov",
  Z = Z_coho.model,
  A = a_coho.model,
  R = R_coho.model,
  x0 = "equal",
  V0 = "zero",
  tinitx = 0
)

# build model steelhead
# R
n_stel <- nrow(nosa_stel)
R_stel.model <- matrix(list(0), n_stel, n_stel)
diag(R_stel.model) <- paste0("r", nosa_stel_rows$method)

# a
scale <- "9S"
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
  Q = "equalvarcov",
  Z = Z_stel.model,
  A = a_stel.model,
  R = R_stel.model,
  x0 = "equal",
  V0 = "zero",
  tinitx = 0
)

# run MARSS model - chin
if(!file.exists(here::here("data", "clean", paste("POPSIZE3ssm_chinM", scale, ".rds", sep="")))){
  ptm <- proc.time()
  POPSIZE3ssm_chin <- MARSS(nosa_chin, model = mod_chin.list, method = "kem", control = con.list)
  saveRDS(POPSIZE3ssm_chin, file=here::here("data", "clean", paste("POPSIZE3ssm_chinM", scale, ".rds", sep="")))
  chin_time <- proc.time()[3] - ptm
  chin_time
}
# load in ssm_chin
POPSIZE3ssm_chin <- readRDS(file=here::here("data", "clean", paste("POPSIZE3ssm_chinM", scale, ".rds", sep="")))


# run MARSS model - coho
if(!file.exists(here::here("data", "clean", paste("POPSIZE3ssm_cohoM", scale, ".rds", sep="")))){
  ptm <- proc.time()
  POPSIZE3ssm_coho <- MARSS(nosa_coho, model = mod_coho.list, method = "kem", control = con.list)
  saveRDS(POPSIZE3ssm_coho, file=here::here("data", "clean", paste("POPSIZE3ssm_cohoM", scale, ".rds", sep="")))
  coho_time <- proc.time()[3] - ptm
  coho_time
}
# load in ssm_coho
POPSIZE3ssm_coho <- readRDS(file=here::here("data", "clean", paste("POPSIZE3ssm_cohoM", scale, ".rds", sep="")))

# run MARSS model - stel
if(!file.exists(here::here("data", "clean", paste("POPSIZE3ssm_stelM", scale, ".rds", sep="")))){
  ptm <- proc.time()
  POPSIZE3ssm_stel <- MARSS(nosa_stel, model = mod_stel.list, method = "kem", control = con.list)
  saveRDS(POPSIZE3ssm_stel, file=here::here("data", "clean", paste("POPSIZE3ssm_stelM", scale, ".rds", sep="")))
  stel_time <- proc.time()[3] - ptm
  stel_time
}
# load in ssm_stel
POPSIZE3ssm_stel <- readRDS(file=here::here("data", "clean", paste("POPSIZE3ssm_stelM", scale, ".rds", sep="")))

# check for best fit
POPSIZE2ssm_chin$AICc
  # 1569.68
POPSIZE3ssm_chin$AICc
  # 1296.671 - best fit

POPSIZE2ssm_coho$AICc
  # 2218.874
POPSIZE3ssm_coho$AICc
  # 1871.073 - best fit

POPSIZE2ssm_stel$AICc
  # 1215.657
POPSIZE3ssm_stel$AICc
  # 873.6264 - best fit
