## SET WORKING DIR & PACKAGES
library(here)
library(MARSS)
library(panelr)
library(tidyverse)

here::i_am("code/development/03.3.2-POPSIZEmodel_buildTESTfifths.R")
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

### CHINOOK
# dividing into halfs, thirds, and fourths requires twelvths
cutoffs <- quantile(nosa_chin$lnnosa, probs = c(1/15, 2/15, 1/5, 4/15, 1/3, 2/5, 7/15, 8/15, 3/5, 2/3, 11/15, 4/5, 13/15, 14/15), na.rm = TRUE)
nosa_chin$scale <- ifelse(nosa_chin$lnnosa <= cutoffs[1], "Q1",
                          ifelse(nosa_chin$lnnosa <= cutoffs[2], "Q2",
                                 ifelse(nosa_chin$lnnosa <= cutoffs[3], "Q3",
                                        ifelse(nosa_chin$lnnosa <= cutoffs[4], "Q4",
                                               ifelse(nosa_chin$lnnosa <= cutoffs[5], "Q5", 
                                                      ifelse(nosa_chin$lnnosa <= cutoffs[6], "Q6",
                                                             ifelse(nosa_chin$lnnosa <= cutoffs[7], "Q7",
                                                                    ifelse(nosa_chin$lnnosa <= cutoffs[8], "Q8",
                                                                           ifelse(nosa_chin$lnnosa <= cutoffs[9], "Q9",
                                                                                  ifelse(nosa_chin$lnnosa <= cutoffs[10], "Q10",
                                                                                         ifelse(nosa_chin$lnnosa <= cutoffs[11], "Q11",
                                                                                                ifelse(nosa_chin$lnnosa <= cutoffs[12], "Q12",
                                                                                                       ifelse(nosa_chin$lnnosa <= cutoffs[13], "Q13",
                                                                                                              ifelse(nosa_chin$lnnosa <= cutoffs[14], "Q14", "Q15"
                                                                                         ))))))))))))))

# new method varIDs including size
nosa_chin$MethodSizeID<- paste0(as.character(nosa_chin$MethodNameID),"", as.character(nosa_chin$scale))

# new popid/method var
nosa_chin$popmethod <- paste0(as.character(nosa_chin$PopID),"_", as.character(nosa_chin$MethodSizeID))

# still issues with pop 11 somehow (chin)
nosa_chin <- nosa_chin %>%
  filter(TimeSeriesID != 599005)

# how often are particular survey methods used
counts_chin <- table(nosa_chin$MethodNameID)
counts_chin
# 2 methods < 10 obs

# will drop those methods for which fewer than 10 observations exist
# underlying must be the same for AICc comparsions to these drops will be based off MethodNameID, not MethodSizeID
low_count_ids <- names(counts_chin[counts_chin < 10])
low_counts_chin <- as.numeric(low_count_ids)
nosa_chin <- nosa_chin %>%
  filter(!MethodNameID %in% low_counts_chin)
table(nosa_chin$MethodNameID)

# set up popID data for MARSS models
nosa_chin <- nosa_chin[-c(1, 3:9, 11:(ncol(nosa_chin)-1))]

# set data wide (rows = popid/method, columns = year)
nosa_chin <- panel_data(nosa_chin, id = popmethod, wave = Year)
nosa_chin <- widen_panel(nosa_chin, separator = "_")

# some resorting and cleaning
nosa_chin <- nosa_chin[,order(colnames(nosa_chin))]
nosa_chin_rows <- as.data.frame(stringr::str_split_fixed(nosa_chin$popmethod, "_", 2))
colnames(nosa_chin_rows) <- c("popid", "method")
nosa_chin_rows$methodNo <- sub("(^\\d+)Q.*", "\\1", nosa_chin_rows$method)
nosa_chin_rows$nth <- sub("^\\d+(Q.*)", "\\1", nosa_chin_rows$method)

nosa_chin_rows$method3 <- ifelse(nosa_chin_rows$nth %in% c("Q1", "Q2", "Q3", "Q4", "Q5"), "S", 
                                 ifelse(nosa_chin_rows$nth %in% c("Q6", "Q7", "Q8", "Q9", "Q10"), "M", "L"
                                 ))
nosa_chin_rows$method3 <- paste0(nosa_chin_rows$methodNo, "",nosa_chin_rows$method3)

nosa_chin_rows$method5 <- ifelse(nosa_chin_rows$nth %in% c("Q1", "Q2", "Q3"), "Q1", 
                                 ifelse(nosa_chin_rows$nth %in% c("Q4", "Q5", "Q6"), "Q2",
                                        ifelse(nosa_chin_rows$nth %in% c("Q7", "Q8", "Q9"), "Q3",
                                               ifelse(nosa_chin_rows$nth %in% c("Q10", "Q11", "Q12"), "Q4", "Q5"
                                 ))))
nosa_chin_rows$method5 <- paste0(nosa_chin_rows$methodNo, "",nosa_chin_rows$method5)

nosa_chin <- nosa_chin[-c(46)]
colnames(nosa_chin) <- substr(colnames(nosa_chin), 8, 11)
years <- colnames(nosa_chin)
nosa_chin <- as.matrix(nosa_chin)

# set controls
con.list <- list(maxit = 1000, allow.degen = TRUE)

# build model chinook
# R
n_chin <- nrow(nosa_chin)
POPSIZE5R_chin.model <- matrix(list(0), n_chin, n_chin)
diag(POPSIZE5R_chin.model) <- paste0("r", nosa_chin_rows$method5)

n_chin <- nrow(nosa_chin)
POPSIZE3R_chin.model <- matrix(list(0), n_chin, n_chin)
diag(POPSIZE3R_chin.model) <- paste0("r", nosa_chin_rows$method3)

n_chin <- nrow(nosa_chin)
POPSIZE1R_chin.model <- matrix(list(0), n_chin, n_chin)
diag(POPSIZE1R_chin.model) <- paste0("r", nosa_chin_rows$methodNo)

# a
scale5 <- "9Q3"
# sets relative value against which other survey methods will be scaled
  # 9 -> dam counts - accurate (according to parsons and Skalski)
  # 9s is the only 9 method that appears across all three species
POPSIZE5a_chin.model <- matrix(list(0), n_chin, 1)
for(i in 1:length(POPSIZE5a_chin.model)){
  if(nosa_chin_rows$method5[i] != scale5){
    POPSIZE5a_chin.model[i] <- paste0("a", nosa_chin_rows$method5[i])
  }
}

scale3 <- "9M"
# sets relative value against which other survey methods will be scaled
POPSIZE3a_chin.model <- matrix(list(0), n_chin, 1)
for(i in 1:length(POPSIZE3a_chin.model)){
  if(nosa_chin_rows$method3[i] != scale3){
    POPSIZE3a_chin.model[i] <- paste0("a", nosa_chin_rows$method3[i])
  }
}

scale1 <- "9"
# sets relative value against which other survey methods will be scaled
POPSIZE1a_chin.model <- matrix(list(0), n_chin, 1)
for(i in 1:length(POPSIZE1a_chin.model)){
  if(nosa_chin_rows$methodNo[i] != scale1){
    POPSIZE1a_chin.model[i] <- paste0("a", nosa_chin_rows$methodNo[i])
  }
}

# Z
pops_chin <- c(unique(nosa_chin_rows$popid))
Z_chin.model <- matrix(0, nrow=nrow(nosa_chin), ncol=length(unique(nosa_chin_rows$popid)))
for(i in seq(length(pops_chin))){
  Z_chin.model[nosa_chin_rows$popid == pops_chin[i], i] <- 1
}

# model lists
POPSIZE1mod_chin.list <- list(
  B = "identity",
  U = "zero",
  Q = "equalvarcov",
  Z = Z_chin.model,
  A = POPSIZE1a_chin.model,
  R = POPSIZE1R_chin.model,
  x0 = "equal",
  V0 = "zero",
  tinitx = 0
)

POPSIZE3mod_chin.list <- list(
  B = "identity",
  U = "zero",
  Q = "equalvarcov",
  Z = Z_chin.model,
  A = POPSIZE3a_chin.model,
  R = POPSIZE3R_chin.model,
  x0 = "equal",
  V0 = "zero",
  tinitx = 0
)

POPSIZE5mod_chin.list <- list(
  B = "identity",
  U = "zero",
  Q = "equalvarcov",
  Z = Z_chin.model,
  A = POPSIZE5a_chin.model,
  R = POPSIZE5R_chin.model,
  x0 = "equal",
  V0 = "zero",
  tinitx = 0
)

# run MARSS models - chin
# agnostic to pop size
if(!file.exists(here::here("data", "clean", paste("POPSIZE1ssm_chinM", scale1, "S5TEST.rds", sep="")))){
  ptm <- proc.time()
  POPSIZE1ssm_chin <- MARSS(nosa_chin, model = POPSIZE1mod_chin.list, method = "kem", control = con.list)
  saveRDS(POPSIZE1ssm_chin, file=here::here("data", "clean", paste("POPSIZE1ssm_chinM", scale1, "S5TEST.rds", sep="")))
  chin_time <- proc.time()[3] - ptm
  chin_time
}
# load in ssm_chin
POPSIZE1ssm_chin <- readRDS(file=here::here("data", "clean", paste("POPSIZE1ssm_chinM", scale1, "S5TEST.rds", sep="")))

# large, medium, and small
if(!file.exists(here::here("data", "clean", paste("POPSIZE3ssm_chinM", scale3, "S5TEST.rds", sep="")))){
  ptm <- proc.time()
  POPSIZE3ssm_chin <- MARSS(nosa_chin, model = POPSIZE3mod_chin.list, method = "kem", control = con.list)
  saveRDS(POPSIZE3ssm_chin, file=here::here("data", "clean", paste("POPSIZE3ssm_chinM", scale3, "S5TEST.rds", sep="")))
  chin_time <- proc.time()[3] - ptm
  chin_time
}
# load in ssm_chin
POPSIZE3ssm_chin <- readRDS(file=here::here("data", "clean", paste("POPSIZE3ssm_chinM", scale3, "S5TEST.rds", sep="")))

# five fifths
if(!file.exists(here::here("data", "clean", paste("POPSIZE5ssm_chinM", scale5, "S5TEST.rds", sep="")))){
  ptm <- proc.time()
  POPSIZE5ssm_chin <- MARSS(nosa_chin, model = POPSIZE5mod_chin.list, method = "kem", control = con.list)
  saveRDS(POPSIZE5ssm_chin, file=here::here("data", "clean", paste("POPSIZE5ssm_chinM", scale5, "S5TEST.rds", sep="")))
  chin_time <- proc.time()[3] - ptm
  chin_time
}
# load in ssm_chin
POPSIZE5ssm_chin <- readRDS(file=here::here("data", "clean", paste("POPSIZE5ssm_chinM", scale5, "S5TEST.rds", sep="")))

# check for best fit
POPSIZE1ssm_chin$AICc
POPSIZE3ssm_chin$AICc
POPSIZE5ssm_chin$AICc

### COHO
# dividing into halfs, thirds, and fourths requires twelvths
cutoffs <- quantile(nosa_coho$lnnosa, probs = c(1/15, 2/15, 1/5, 4/15, 1/3, 2/5, 7/15, 8/15, 3/5, 2/3, 11/15, 4/5, 13/15, 14/15), na.rm = TRUE)
nosa_coho$scale <- ifelse(nosa_coho$lnnosa <= cutoffs[1], "Q1",
                          ifelse(nosa_coho$lnnosa <= cutoffs[2], "Q2",
                                 ifelse(nosa_coho$lnnosa <= cutoffs[3], "Q3",
                                        ifelse(nosa_coho$lnnosa <= cutoffs[4], "Q4",
                                               ifelse(nosa_coho$lnnosa <= cutoffs[5], "Q5", 
                                                      ifelse(nosa_coho$lnnosa <= cutoffs[6], "Q6",
                                                             ifelse(nosa_coho$lnnosa <= cutoffs[7], "Q7",
                                                                    ifelse(nosa_coho$lnnosa <= cutoffs[8], "Q8",
                                                                           ifelse(nosa_coho$lnnosa <= cutoffs[9], "Q9",
                                                                                  ifelse(nosa_coho$lnnosa <= cutoffs[10], "Q10",
                                                                                         ifelse(nosa_coho$lnnosa <= cutoffs[11], "Q11",
                                                                                                ifelse(nosa_coho$lnnosa <= cutoffs[12], "Q12",
                                                                                                       ifelse(nosa_coho$lnnosa <= cutoffs[13], "Q13",
                                                                                                              ifelse(nosa_coho$lnnosa <= cutoffs[14], "Q14", "Q15"
                                                                                                              ))))))))))))))

# new method varIDs including size
nosa_coho$MethodSizeID<- paste0(as.character(nosa_coho$MethodNameID),"", as.character(nosa_coho$scale))

# new popid/method var
nosa_coho$popmethod <- paste0(as.character(nosa_coho$PopID),"_", as.character(nosa_coho$MethodSizeID))

# how often are particular survey methods used
counts_coho <- table(nosa_coho$MethodNameID)
counts_coho
  # 2 methods < 10 obs

# will drop those methods for which fewer than 10 observations exist
# underlying must be the same for AICc comparsions to these drops will be based off MethodNameID, not MethodSizeID
low_count_ids <- names(counts_coho[counts_coho < 10])
low_counts_coho <- as.numeric(low_count_ids)
nosa_coho <- nosa_coho %>%
  filter(!MethodNameID %in% low_counts_coho)
table(nosa_coho$MethodNameID)

# set up popID data for MARSS models
nosa_coho <- nosa_coho[-c(1, 3:9, 11:(ncol(nosa_coho)-1))]

# set data wide (rows = popid/method, columns = year)
nosa_coho <- panel_data(nosa_coho, id = popmethod, wave = Year)
nosa_coho <- widen_panel(nosa_coho, separator = "_")

# some resorting and cleaning
nosa_coho <- nosa_coho[,order(colnames(nosa_coho))]
nosa_coho_rows <- as.data.frame(stringr::str_split_fixed(nosa_coho$popmethod, "_", 2))
colnames(nosa_coho_rows) <- c("popid", "method")
nosa_coho_rows$methodNo <- sub("(^\\d+)Q.*", "\\1", nosa_coho_rows$method)
nosa_coho_rows$nth <- sub("^\\d+(Q.*)", "\\1", nosa_coho_rows$method)

nosa_coho_rows$method3 <- ifelse(nosa_coho_rows$nth %in% c("Q1", "Q2", "Q3", "Q4", "Q5"), "S", 
                                 ifelse(nosa_coho_rows$nth %in% c("Q6", "Q7", "Q8", "Q9", "Q10"), "M", "L"
                                 ))
nosa_coho_rows$method3 <- paste0(nosa_coho_rows$methodNo, "",nosa_coho_rows$method3)

nosa_coho_rows$method5 <- ifelse(nosa_coho_rows$nth %in% c("Q1", "Q2", "Q3"), "Q1", 
                                 ifelse(nosa_coho_rows$nth %in% c("Q4", "Q5", "Q6"), "Q2",
                                        ifelse(nosa_coho_rows$nth %in% c("Q7", "Q8", "Q9"), "Q3",
                                               ifelse(nosa_coho_rows$nth %in% c("Q10", "Q11", "Q12"), "Q4", "Q5"
                                               ))))
nosa_coho_rows$method5 <- paste0(nosa_coho_rows$methodNo, "",nosa_coho_rows$method5)

nosa_coho <- nosa_coho[-c(46)]
colnames(nosa_coho) <- substr(colnames(nosa_coho), 8, 11)
years <- colnames(nosa_coho)
nosa_coho <- as.matrix(nosa_coho)

# set controls
con.list <- list(maxit = 1000, allow.degen = TRUE)

# build model coho
# R
n_coho <- nrow(nosa_coho)
POPSIZE5R_coho.model <- matrix(list(0), n_coho, n_coho)
diag(POPSIZE5R_coho.model) <- paste0("r", nosa_coho_rows$method5)

n_coho <- nrow(nosa_coho)
POPSIZE3R_coho.model <- matrix(list(0), n_coho, n_coho)
diag(POPSIZE3R_coho.model) <- paste0("r", nosa_coho_rows$method3)

n_coho <- nrow(nosa_coho)
POPSIZE1R_coho.model <- matrix(list(0), n_coho, n_coho)
diag(POPSIZE1R_coho.model) <- paste0("r", nosa_coho_rows$methodNo)

# a
scale5 <- "9Q3"
# sets relative value against which other survey methods will be scaled
# 9 -> dam counts - accurate (according to parsons and Skalski)
# 9s is the only 9 method that appears across all three species
POPSIZE5a_coho.model <- matrix(list(0), n_coho, 1)
for(i in 1:length(POPSIZE5a_coho.model)){
  if(nosa_coho_rows$method5[i] != scale5){
    POPSIZE5a_coho.model[i] <- paste0("a", nosa_coho_rows$method5[i])
  }
}

scale3 <- "9M"
# sets relative value against which other survey methods will be scaled
POPSIZE3a_coho.model <- matrix(list(0), n_coho, 1)
for(i in 1:length(POPSIZE3a_coho.model)){
  if(nosa_coho_rows$method3[i] != scale3){
    POPSIZE3a_coho.model[i] <- paste0("a", nosa_coho_rows$method3[i])
  }
}

scale1 <- "9"
# sets relative value against which other survey methods will be scaled
POPSIZE1a_coho.model <- matrix(list(0), n_coho, 1)
for(i in 1:length(POPSIZE1a_coho.model)){
  if(nosa_coho_rows$methodNo[i] != scale1){
    POPSIZE1a_coho.model[i] <- paste0("a", nosa_coho_rows$methodNo[i])
  }
}

# Z
pops_coho <- c(unique(nosa_coho_rows$popid))
Z_coho.model <- matrix(0, nrow=nrow(nosa_coho), ncol=length(unique(nosa_coho_rows$popid)))
for(i in seq(length(pops_coho))){
  Z_coho.model[nosa_coho_rows$popid == pops_coho[i], i] <- 1
}

# model lists
POPSIZE1mod_coho.list <- list(
  B = "identity",
  U = "zero",
  Q = "equalvarcov",
  Z = Z_coho.model,
  A = POPSIZE1a_coho.model,
  R = POPSIZE1R_coho.model,
  x0 = "equal",
  V0 = "zero",
  tinitx = 0
)

POPSIZE3mod_coho.list <- list(
  B = "identity",
  U = "zero",
  Q = "equalvarcov",
  Z = Z_coho.model,
  A = POPSIZE3a_coho.model,
  R = POPSIZE3R_coho.model,
  x0 = "equal",
  V0 = "zero",
  tinitx = 0
)

POPSIZE5mod_coho.list <- list(
  B = "identity",
  U = "zero",
  Q = "equalvarcov",
  Z = Z_coho.model,
  A = POPSIZE5a_coho.model,
  R = POPSIZE5R_coho.model,
  x0 = "equal",
  V0 = "zero",
  tinitx = 0
)

# run MARSS models - coho
# agnostic to pop size
if(!file.exists(here::here("data", "clean", paste("POPSIZE1ssm_cohoM", scale1, "S5TEST.rds", sep="")))){
  ptm <- proc.time()
  POPSIZE1ssm_coho <- MARSS(nosa_coho, model = POPSIZE1mod_coho.list, method = "kem", control = con.list)
  saveRDS(POPSIZE1ssm_coho, file=here::here("data", "clean", paste("POPSIZE1ssm_cohoM", scale1, "S5TEST.rds", sep="")))
  coho_time <- proc.time()[3] - ptm
  coho_time
}
# load in ssm_coho
POPSIZE1ssm_coho <- readRDS(file=here::here("data", "clean", paste("POPSIZE1ssm_cohoM", scale1, "S5TEST.rds", sep="")))

# large, medium, and small
if(!file.exists(here::here("data", "clean", paste("POPSIZE3ssm_cohoM", scale3, "S5TEST.rds", sep="")))){
  ptm <- proc.time()
  POPSIZE3ssm_coho <- MARSS(nosa_coho, model = POPSIZE3mod_coho.list, method = "kem", control = con.list)
  saveRDS(POPSIZE3ssm_coho, file=here::here("data", "clean", paste("POPSIZE3ssm_cohoM", scale3, "S5TEST.rds", sep="")))
  coho_time <- proc.time()[3] - ptm
  coho_time
}
# load in ssm_coho
POPSIZE3ssm_coho <- readRDS(file=here::here("data", "clean", paste("POPSIZE3ssm_cohoM", scale3, "S5TEST.rds", sep="")))

# five fifths
if(!file.exists(here::here("data", "clean", paste("POPSIZE5ssm_cohoM", scale5, "S5TEST.rds", sep="")))){
  ptm <- proc.time()
  POPSIZE5ssm_coho <- MARSS(nosa_coho, model = POPSIZE5mod_coho.list, method = "kem", control = con.list)
  saveRDS(POPSIZE5ssm_coho, file=here::here("data", "clean", paste("POPSIZE5ssm_cohoM", scale5, "S5TEST.rds", sep="")))
  coho_time <- proc.time()[3] - ptm
  coho_time
}
# load in ssm_coho
POPSIZE5ssm_coho <- readRDS(file=here::here("data", "clean", paste("POPSIZE5ssm_cohoM", scale5, "S5TEST.rds", sep="")))

# check for best fit
POPSIZE1ssm_coho$AICc
POPSIZE3ssm_coho$AICc
POPSIZE5ssm_coho$AICc

### STEELHEAD
# dividing into halfs, thirds, and fourths requires twelvths
cutoffs <- quantile(nosa_stel$lnnosa, probs = c(1/15, 2/15, 1/5, 4/15, 1/3, 2/5, 7/15, 8/15, 3/5, 2/3, 11/15, 4/5, 13/15, 14/15), na.rm = TRUE)
nosa_stel$scale <- ifelse(nosa_stel$lnnosa <= cutoffs[1], "Q1",
                          ifelse(nosa_stel$lnnosa <= cutoffs[2], "Q2",
                                 ifelse(nosa_stel$lnnosa <= cutoffs[3], "Q3",
                                        ifelse(nosa_stel$lnnosa <= cutoffs[4], "Q4",
                                               ifelse(nosa_stel$lnnosa <= cutoffs[5], "Q5", 
                                                      ifelse(nosa_stel$lnnosa <= cutoffs[6], "Q6",
                                                             ifelse(nosa_stel$lnnosa <= cutoffs[7], "Q7",
                                                                    ifelse(nosa_stel$lnnosa <= cutoffs[8], "Q8",
                                                                           ifelse(nosa_stel$lnnosa <= cutoffs[9], "Q9",
                                                                                  ifelse(nosa_stel$lnnosa <= cutoffs[10], "Q10",
                                                                                         ifelse(nosa_stel$lnnosa <= cutoffs[11], "Q11",
                                                                                                ifelse(nosa_stel$lnnosa <= cutoffs[12], "Q12",
                                                                                                       ifelse(nosa_stel$lnnosa <= cutoffs[13], "Q13",
                                                                                                              ifelse(nosa_stel$lnnosa <= cutoffs[14], "Q14", "Q15"
                                                                                                              ))))))))))))))

# new method varIDs including size
nosa_stel$MethodSizeID<- paste0(as.character(nosa_stel$MethodNameID),"", as.character(nosa_stel$scale))

# new popid/method var
nosa_stel$popmethod <- paste0(as.character(nosa_stel$PopID),"_", as.character(nosa_stel$MethodSizeID))

# still issues with pop 11 somehow (stel)
nosa_stel <- nosa_stel %>%
  filter(TimeSeriesID != 599005)

# how often are particular survey methods used
counts_stel <- table(nosa_stel$MethodNameID)
counts_stel
  # 6 methods < 10 obs

# will drop those methods for which fewer than 10 observations exist
# underlying must be the same for AICc comparsions to these drops will be based off MethodNameID, not MethodSizeID
low_count_ids <- names(counts_stel[counts_stel < 10])
low_counts_stel <- as.numeric(low_count_ids)
nosa_stel <- nosa_stel %>%
  filter(!MethodNameID %in% low_counts_stel)
table(nosa_stel$MethodNameID)

# set up popID data for MARSS models
nosa_stel <- nosa_stel[-c(1, 3:9, 11:(ncol(nosa_stel)-1))]

# set data wide (rows = popid/method, columns = year)
nosa_stel <- panel_data(nosa_stel, id = popmethod, wave = Year)
nosa_stel <- widen_panel(nosa_stel, separator = "_")

# some resorting and cleaning
nosa_stel <- nosa_stel[,order(colnames(nosa_stel))]
nosa_stel_rows <- as.data.frame(stringr::str_split_fixed(nosa_stel$popmethod, "_", 2))
colnames(nosa_stel_rows) <- c("popid", "method")
nosa_stel_rows$methodNo <- sub("(^\\d+)Q.*", "\\1", nosa_stel_rows$method)
nosa_stel_rows$nth <- sub("^\\d+(Q.*)", "\\1", nosa_stel_rows$method)

nosa_stel_rows$method3 <- ifelse(nosa_stel_rows$nth %in% c("Q1", "Q2", "Q3", "Q4", "Q5"), "S", 
                                 ifelse(nosa_stel_rows$nth %in% c("Q6", "Q7", "Q8", "Q9", "Q10"), "M", "L"
                                 ))
nosa_stel_rows$method3 <- paste0(nosa_stel_rows$methodNo, "",nosa_stel_rows$method3)

nosa_stel_rows$method5 <- ifelse(nosa_stel_rows$nth %in% c("Q1", "Q2", "Q3"), "Q1", 
                                 ifelse(nosa_stel_rows$nth %in% c("Q4", "Q5", "Q6"), "Q2",
                                        ifelse(nosa_stel_rows$nth %in% c("Q7", "Q8", "Q9"), "Q3",
                                               ifelse(nosa_stel_rows$nth %in% c("Q10", "Q11", "Q12"), "Q4", "Q5"
                                               ))))
nosa_stel_rows$method5 <- paste0(nosa_stel_rows$methodNo, "",nosa_stel_rows$method5)

nosa_stel <- nosa_stel[-c(46)]
colnames(nosa_stel) <- substr(colnames(nosa_stel), 8, 11)
years <- colnames(nosa_stel)
nosa_stel <- as.matrix(nosa_stel)

# set controls
con.list <- list(maxit = 1000, allow.degen = TRUE)

# build model steelhead
# R
n_stel <- nrow(nosa_stel)
POPSIZE5R_stel.model <- matrix(list(0), n_stel, n_stel)
diag(POPSIZE5R_stel.model) <- paste0("r", nosa_stel_rows$method5)

n_stel <- nrow(nosa_stel)
POPSIZE3R_stel.model <- matrix(list(0), n_stel, n_stel)
diag(POPSIZE3R_stel.model) <- paste0("r", nosa_stel_rows$method3)

n_stel <- nrow(nosa_stel)
POPSIZE1R_stel.model <- matrix(list(0), n_stel, n_stel)
diag(POPSIZE1R_stel.model) <- paste0("r", nosa_stel_rows$methodNo)

# a
scale5 <- "9Q3"
# sets relative value against which other survey methods will be scaled
# 9 -> dam counts - accurate (according to parsons and Skalski)
# 9s is the only 9 method that appears across all three species
POPSIZE5a_stel.model <- matrix(list(0), n_stel, 1)
for(i in 1:length(POPSIZE5a_stel.model)){
  if(nosa_stel_rows$method5[i] != scale5){
    POPSIZE5a_stel.model[i] <- paste0("a", nosa_stel_rows$method5[i])
  }
}

scale3 <- "9M"
# sets relative value against which other survey methods will be scaled
POPSIZE3a_stel.model <- matrix(list(0), n_stel, 1)
for(i in 1:length(POPSIZE3a_stel.model)){
  if(nosa_stel_rows$method3[i] != scale3){
    POPSIZE3a_stel.model[i] <- paste0("a", nosa_stel_rows$method3[i])
  }
}

scale1 <- "9"
# sets relative value against which other survey methods will be scaled
POPSIZE1a_stel.model <- matrix(list(0), n_stel, 1)
for(i in 1:length(POPSIZE1a_stel.model)){
  if(nosa_stel_rows$methodNo[i] != scale1){
    POPSIZE1a_stel.model[i] <- paste0("a", nosa_stel_rows$methodNo[i])
  }
}

# Z
pops_stel <- c(unique(nosa_stel_rows$popid))
Z_stel.model <- matrix(0, nrow=nrow(nosa_stel), ncol=length(unique(nosa_stel_rows$popid)))
for(i in seq(length(pops_stel))){
  Z_stel.model[nosa_stel_rows$popid == pops_stel[i], i] <- 1
}

# model lists
POPSIZE1mod_stel.list <- list(
  B = "identity",
  U = "zero",
  Q = "equalvarcov",
  Z = Z_stel.model,
  A = POPSIZE1a_stel.model,
  R = POPSIZE1R_stel.model,
  x0 = "equal",
  V0 = "zero",
  tinitx = 0
)

POPSIZE3mod_stel.list <- list(
  B = "identity",
  U = "zero",
  Q = "equalvarcov",
  Z = Z_stel.model,
  A = POPSIZE3a_stel.model,
  R = POPSIZE3R_stel.model,
  x0 = "equal",
  V0 = "zero",
  tinitx = 0
)

POPSIZE5mod_stel.list <- list(
  B = "identity",
  U = "zero",
  Q = "equalvarcov",
  Z = Z_stel.model,
  A = POPSIZE5a_stel.model,
  R = POPSIZE5R_stel.model,
  x0 = "equal",
  V0 = "zero",
  tinitx = 0
)

# run MARSS models - stel
# agnostic to pop size
if(!file.exists(here::here("data", "clean", paste("POPSIZE1ssm_stelM", scale1, "S5TEST.rds", sep="")))){
  ptm <- proc.time()
  POPSIZE1ssm_stel <- MARSS(nosa_stel, model = POPSIZE1mod_stel.list, method = "kem", control = con.list)
  saveRDS(POPSIZE1ssm_stel, file=here::here("data", "clean", paste("POPSIZE1ssm_stelM", scale1, "S5TEST.rds", sep="")))
  stel_time <- proc.time()[3] - ptm
  stel_time
}
# load in ssm_stel
POPSIZE1ssm_stel <- readRDS(file=here::here("data", "clean", paste("POPSIZE1ssm_stelM", scale1, "S5TEST.rds", sep="")))

# large, medium, and small
if(!file.exists(here::here("data", "clean", paste("POPSIZE3ssm_stelM", scale3, "S5TEST.rds", sep="")))){
  ptm <- proc.time()
  POPSIZE3ssm_stel <- MARSS(nosa_stel, model = POPSIZE3mod_stel.list, method = "kem", control = con.list)
  saveRDS(POPSIZE3ssm_stel, file=here::here("data", "clean", paste("POPSIZE3ssm_stelM", scale3, "S5TEST.rds", sep="")))
  stel_time <- proc.time()[3] - ptm
  stel_time
}
# load in ssm_stel
POPSIZE3ssm_stel <- readRDS(file=here::here("data", "clean", paste("POPSIZE3ssm_stelM", scale3, "S5TEST.rds", sep="")))

# five fifths
if(!file.exists(here::here("data", "clean", paste("POPSIZE5ssm_stelM", scale5, "S5TEST.rds", sep="")))){
  ptm <- proc.time()
  POPSIZE5ssm_stel <- MARSS(nosa_stel, model = POPSIZE5mod_stel.list, method = "kem", control = con.list)
  saveRDS(POPSIZE5ssm_stel, file=here::here("data", "clean", paste("POPSIZE5ssm_stelM", scale5, "S5TEST.rds", sep="")))
  stel_time <- proc.time()[3] - ptm
  stel_time
}
# load in ssm_stel
POPSIZE5ssm_stel <- readRDS(file=here::here("data", "clean", paste("POPSIZE5ssm_stelM", scale5, "S5TEST.rds", sep="")))

# check for best fit
POPSIZE1ssm_stel$AICc
POPSIZE3ssm_stel$AICc
POPSIZE5ssm_stel$AICc