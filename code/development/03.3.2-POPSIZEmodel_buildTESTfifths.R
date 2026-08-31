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
