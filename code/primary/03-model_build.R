## SET WORKING DIR & PACKAGES
library(here)
library(tidyverse)

here::i_am("code/primary/03-model_build.R")
options(max.print=2000)

# pull in data
load(here("data", "clean", "nosa_codes.Rda"))
nosa <- merge

# how often are particular survey methods used
table(nosa$MethodNameID)
  # but some of these may just be combinations of other methods...
table(nosa$MethodCode)

method_map <- nosa %>%
  add_count(MethodNameID, name = "MethodNameIDCt") %>%
  add_count(MethodCode, name = "MethodCodeCt") %>%
  distinct(MethodName, MethodNameID, MethodNameIDCt, MethodCode, MethodCodeCt)

# will drop those methods for which fewer than 30 coded (letter) observations exist
nosa <- nosa %>%
  filter(!MethodNameID %in% c(15, 25, 28, 30, 31))

# trying to build an all-species model..
  # must include species as a factor covariate in process equation?
  # following general form from 550models.R

# new popid/method var
nosa$popmethod <- paste0(as.character(nosa$PopID),"_", as.character(nosa$MethodNameID))

# natural log of counts
nosa$lnnosa <- log(nosa$NOSA)

# dismiss extra stuff
nosa_trim <- nosa[-c(1, 3, 4, 6:8)]

# set data wide (rows = popid/method, columns = year)
nosa_dat <- panel_data(nosa_trim, id = popmethod, wave = Year)
nosa_dat <- widen_panel(nosa_dat, separator = "_")

# some resorting and cleaning
nosa_dat <- nosa_dat[,order(colnames(nosa_dat))]
nosa_dat_rows <- as.data.frame(stringr::str_split_fixed(nosa_dat$popmethod, "_", 2))
colnames(nosa_dat_rows) <- c("popid", "method")
nosa_dat_rows$run <- nosa_dat$Run
nosa_dat_rows$code <- nosa_dat$MethodCode
nosa_dat <- nosa_dat[-c(46:48)]
colnames(nosa_dat) <- substr(colnames(nosa_dat), 8, 11)
years <- colnames(nosa_dat)
nosa_dat <- as.matrix(nosa_dat)

# begin structuring model
# constructing R and a and Z
# R
n <- nrow(nosa_dat)
R.model <- matrix(list(0), n, n)
diag(R.model) <- paste0("r", nosa_dat_rows$method)

# a
scale <- "15"
# sets relative value against which other survey methods will be scaled
  # 15 -> MArk-Recapture estimate at weir
a.model <- matrix(list(0), n, 1)
for(i in 1:length(a.model)){
  if(nosa_dat_rows$method[i] != scale){
    a.model[i] <- nosa_dat_rows$method[i]
  }
}
a.model <- method_map$MethodCode[match(a.model, method_map$MethodNameID)]
a.model <- gsub("([a-z])", "1*\\1", a.model)


# Z
pops <- c(unique(nosa_dat_rows$popid))
Z.model <- matrix(0, nrow=nrow(nosa_dat), ncol=length(unique(nosa_dat_rows$popid)))
for(i in seq(length(pops))){
  Z.model[nosa_dat_rows$popid == pops[i], i] <- 1
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

# set controls
con.list <- list(maxit = 5000, allow.degen = TRUE)

# run MARSS model
if(!file.exists(here::here("data", "clean", paste("ssm_nosa", scale, ".rds", sep="")))){
  ptm <- proc.time()
  ssm <- MARSS(nosa_dat, model = mod.list, method = "kem", control = con.list)
  saveRDS(ssm, file=here::here("data", "clean", paste("ssm_nosa", scale, ".rds", sep="")))
  time <- proc.time()[3] - ptm
  time
}
# load in ssm_chin
ssm_nosa <- readRDS(file=here::here("data", "clean", paste("ssm_nosa", scale, ".rds", sep="")))

