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
length(unique(nosa$MethodNameID))
  # 27 methods
counts <- table(nosa$MethodNameID)
counts

# will drop those methods for which fewer than 30 observations exist
low_count_ids <- names(counts[counts < 30])
low_counts <- as.numeric(low_count_ids)

nosa <- nosa %>%
  filter(!MethodNameID %in% low_counts)
table(nosa$MethodNameID)
length(unique(nosa$MethodNameID))
  # this drops 13 methods...

# new popid/method var
nosa$popmethod <- paste0(as.character(nosa$PopID),"_", as.character(nosa$MethodNameID))

# natural log of counts
nosa$lnnosa <- log(nosa$NOSA + 1)

# vital information
methodsTable <- nosa %>% 
  pivot_wider(names_from = "Year", values_from = "MethodNameID", id_cols = "PopID")
print(methodsTable)

# evaluate
length(unique(nosa$PopID))
  # 73 populations
unique(nosa$MethodNameID)
length(unique(nosa$MethodNameID))
  # 14 methods
table(nosa$MethodNameID)

# throw away junk
nosaFULL <- nosa

# preserve time series @ level of populations
nosaPOP <- nosaFULL
nosaPOP <- nosaPOP[-c(3:10)]

# set data wide (rows = popid, columns = year)
nosaPOP <- panel_data(nosaPOP, id = PopID, wave = Year)
nosaPOP <- widen_panel(nosaPOP, separator = "_")

# save these for later
save(nosaPOP, file=here::here("data", "clean", "nosaPOP.Rda"))

# set up popID data for MARSS models
nosa <- nosa[-c(1, 3:9)]

# set data wide (rows = popid/method, columns = year)
nosa <- panel_data(nosa, id = popmethod, wave = Year)
nosa <- widen_panel(nosa, separator = "_")

# some resorting and cleaning
nosa <- nosa[,order(colnames(nosa))]
nosa_rows <- as.data.frame(stringr::str_split_fixed(nosa$popmethod, "_", 2))
colnames(nosa_rows) <- c("popid", "method")
nosa <- nosa[-c(46)]
colnames(nosa) <- substr(colnames(nosa), 8, 11)
years <- colnames(nosa)
nosa <- as.matrix(nosa)

# grab table relating species to popID
popspecies_key <- nosaFULL[-c(2,3,5:11)]
popspecies_key <- unique(popspecies_key[c("PopID", "CommonName")])

popmethodspecies_key <- nosaFULL[-c(1:3,5:9,11)]
popmethodspecies_key <- unique(popmethodspecies_key[c("CommonName", "popmethod")])
  # same for popmethod

# set controls
con.list <- list(maxit = 10000, allow.degen = TRUE)

## model chinook
# constructing R and a and Z
# R
n <- nrow(nosa)
R.model <- matrix(list(0), n, n)
diag(R.model) <- paste0("r", nosa_rows$method)

# a
scale <- "9"
# sets relative value against which other survey methods will be scaled
# 9 -> dam counts - accurate (according to parsons and Skalski)
a.model <- matrix(list(0), n, 1)
for(i in 1:length(a.model)){
  if(nosa_rows$method[i] != scale){
    a.model[i] <- paste0("a", nosa_rows$method[i])
  }
}

# Z
pops <- c(unique(nosa_rows$popid))
Z.model <- matrix(0, nrow=nrow(nosa), ncol=length(unique(nosa_rows$popid)))
for(i in seq(length(pops))){
  Z.model[nosa_rows$popid == pops[i], i] <- 1
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

# run MARSS model
if(!file.exists(here::here("data", "clean", paste("ssm_nosaM", scale, ".rds", sep="")))){
  ptm <- proc.time()
  ssm <- MARSS(nosa, model = mod.list, method = "kem", control = con.list)
  saveRDS(ssm, file=here::here("data", "clean", paste("ssm_nosaM", scale, ".rds", sep="")))
  time <- proc.time()[3] - ptm
  time
}
# load in ssm
ssm <- readRDS(file=here::here("data", "clean", paste("ssm_nosaM", scale, ".rds", sep="")))
