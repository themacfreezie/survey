## SET WORKING DIR & PACKAGES
library(here)
library(MARSS)
library(panelr)
library(tidyverse)

here::i_am("code/primary/03.0-model_fitcheck.R")
options(max.print=2000)

# pull in data
load(here("data", "clean", "nosa_codes.Rda"))
nosa <- merge

# how often are particular survey methods used
counts <- table(nosa$MethodNameID)
counts
# some methods used very infrequently

# new popid/method var
nosa$popmethod <- paste0(as.character(nosa$PopID),"_", as.character(nosa$MethodNameID))

# natural log of counts
nosa$lnnosa <- log(nosa$NOSA + 1)

# different species
nosa_chin <- nosa %>% filter(CommonName=="Chinook Salmon")
nosa_coho <- nosa %>% filter(CommonName=="Coho Salmon")
nosa_stel <- nosa %>% filter(CommonName=="Steelhead")

nosa_chin_methods <- nosa_chin[-c(4:11)]
nosa_coho_methods <- nosa_coho[-c(4:11)]
nosa_stel_methods <- nosa_stel[-c(4:11)]

# save these for plotting bias adjustment
save(nosa_chin_methods, file=here("data", "clean", "nosa_chin_methods.Rda"))
save(nosa_coho_methods, file=here("data", "clean", "nosa_coho_methods.Rda"))
save(nosa_stel_methods, file=here("data", "clean", "nosa_stel_methods.Rda"))

# still issues with pop 11 somehow (chin)
nosa_chin <- nosa_chin %>%
  filter(TimeSeriesID != 599005)
# got it

# how often are particular survey methods used
counts_chin <- table(nosa_chin$MethodNameID)
counts_chin
# 2 methods < 10 obs
counts_coho <- table(nosa_coho$MethodNameID)
counts_coho
# 2 methods < 10 obs
counts_stel <- table(nosa_stel$MethodNameID)
counts_stel
# 6 methods < 10 obs

# will drop those methods for which fewer than 10 observations exist
low_count_ids <- names(counts_chin[counts_chin < 10])
low_counts_chin <- as.numeric(low_count_ids)

nosa_chin <- nosa_chin %>%
  filter(!MethodNameID %in% low_counts_chin)
table(nosa_chin$MethodNameID)

low_count_ids <- names(counts_coho[counts_coho < 10])
low_counts_coho <- as.numeric(low_count_ids)

nosa_coho <- nosa_coho %>%
  filter(!MethodNameID %in% low_counts_coho)
table(nosa_coho$MethodNameID)

low_count_ids <- names(counts_stel[counts_stel < 10])
low_counts_stel <- as.numeric(low_count_ids)

nosa_stel <- nosa_stel %>%
  filter(!MethodNameID %in% low_counts_stel)
table(nosa_stel$MethodNameID)

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

# save these for reference to model dat
save(nosa_chin, file=here("data", "clean", "chin_modeldat.Rda"))
save(nosa_coho, file=here("data", "clean", "coho_modeldat.Rda"))
save(nosa_stel, file=here("data", "clean", "stel_modeldat.Rda"))

## per species
# chinook
length(unique(nosa_chin$PopID))
# 22 populations
unique(nosa_chin$MethodNameID)
length(unique(nosa_chin$MethodNameID))
# 9 methods
table(nosa_chin$MethodNameID)

# coho
length(unique(nosa_coho$PopID))
# 29 populations
unique(nosa_coho$MethodNameID)
length(unique(nosa_coho$MethodNameID))
# 10 methods
table(nosa_coho$MethodNameID)

# steelhead
length(unique(nosa_stel$PopID))
# 23 populations
unique(nosa_stel$MethodNameID)
length(unique(nosa_stel$MethodNameID))
# 8 methods
table(nosa_stel$MethodNameID)

# throw away junk
nosa_chinFULL <- nosa_chin
nosa_cohoFULL <- nosa_coho
nosa_stelFULL <- nosa_stel

# preserve time series @ level of populations
nosa_chinPOP <- nosa_chinFULL
nosa_cohoPOP <- nosa_cohoFULL
nosa_stelPOP <- nosa_stelFULL

nosa_chinPOP <- nosa_chinPOP[-c(3:10)]
nosa_cohoPOP <- nosa_cohoPOP[-c(3:10)]
nosa_stelPOP <- nosa_stelPOP[-c(3:10)]

# set data wide (rows = popid/method, columns = year)
nosa_chinPOP <- panel_data(nosa_chinPOP, id = PopID, wave = Year)
nosa_chinPOP <- widen_panel(nosa_chinPOP, separator = "_")

nosa_cohoPOP <- panel_data(nosa_cohoPOP, id = PopID, wave = Year)
nosa_cohoPOP <- widen_panel(nosa_cohoPOP, separator = "_")

nosa_stelPOP <- panel_data(nosa_stelPOP, id = PopID, wave = Year)
nosa_stelPOP <- widen_panel(nosa_stelPOP, separator = "_")

# gotta clean up coho and steelhead column order
first_col <- "PopID"

coho_yearcols <- setdiff(names(nosa_cohoPOP), first_col)
coho_years <- as.numeric(gsub("lnnosa_", "", coho_yearcols))
coho_sortedyearcols <- coho_yearcols[order(coho_years)]
nosa_cohoPOP <- nosa_cohoPOP[, c(first_col, coho_sortedyearcols)]

stel_yearcols <- setdiff(names(nosa_stelPOP), first_col)
stel_years <- as.numeric(gsub("lnnosa_", "", stel_yearcols))
stel_sortedyearcols <- stel_yearcols[order(stel_years)]
nosa_stelPOP <- nosa_stelPOP[, c(first_col, stel_sortedyearcols)]

# save these for later
save(nosa_chinPOP, file=here::here("data", "clean", "nosa_chinPOP.Rda"))
save(nosa_cohoPOP, file=here::here("data", "clean", "nosa_cohoPOP.Rda"))
save(nosa_stelPOP, file=here::here("data", "clean", "nosa_stelPOP.Rda"))

# set up popID data for MARSS models
nosa_chin <- nosa_chin[-c(1, 3:9)]
nosa_coho <- nosa_coho[-c(1, 3:9)]
nosa_stel <- nosa_stel[-c(1, 3:9)]

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
mod_chin_u1z1.list <- list(
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

mod_chin_u1z2.list <- list(
  B = "identity",
  U = "zero",
  Q = "diagonal and equal",
  Z = Z_chin.model,
  A = a_chin.model,
  R = R_chin.model,
  x0 = "equal",
  V0 = "zero",
  tinitx = 0
)

mod_chin_u1z3.list <- list(
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

mod_chin_u2z1.list <- list(
  B = "identity",
  U = "unequal",
  Q = "diagonal and unequal",
  Z = Z_chin.model,
  A = a_chin.model,
  R = R_chin.model,
  x0 = "equal",
  V0 = "zero",
  tinitx = 0
)

mod_chin_u2z2.list <- list(
  B = "identity",
  U = "unequal",
  Q = "diagonal and equal",
  Z = Z_chin.model,
  A = a_chin.model,
  R = R_chin.model,
  x0 = "equal",
  V0 = "zero",
  tinitx = 0
)

mod_chin_u2z3.list <- list(
  B = "identity",
  U = "unequal",
  Q = "equalvarcov",
  Z = Z_chin.model,
  A = a_chin.model,
  R = R_chin.model,
  x0 = "equal",
  V0 = "zero",
  tinitx = 0
)

# fit models
ssm_chin_u1z1 <- MARSS(nosa_chin, model = mod_chin_u1z1.list, method = "kem", control = con.list)
ssm_chin_u1z2 <- MARSS(nosa_chin, model = mod_chin_u1z2.list, method = "kem", control = con.list)
ssm_chin_u1z3 <- MARSS(nosa_chin, model = mod_chin_u1z3.list, method = "kem", control = con.list)
ssm_chin_u2z1 <- MARSS(nosa_chin, model = mod_chin_u2z1.list, method = "kem", control = con.list)
ssm_chin_u2z2 <- MARSS(nosa_chin, model = mod_chin_u2z2.list, method = "kem", control = con.list)
ssm_chin_u2z3 <- MARSS(nosa_chin, model = mod_chin_u2z3.list, method = "kem", control = con.list)

# check fit
ssm_chin_u1z1$AICc
  # 1590.313
ssm_chin_u1z2$AICc
  # 1648.042
ssm_chin_u1z3$AICc
  # 1569.68 - best fit for chinook
ssm_chin_u2z1$AICc
  # 1615.852
ssm_chin_u2z2$AICc
  # 1675.852
ssm_chin_u2z3$AICc
  # 1600.009

## model coho
# constructing R and a and Z
# R
n_coho <- nrow(nosa_coho)
R_coho.model <- matrix(list(0), n_coho, n_coho)
diag(R_coho.model) <- paste0("r", nosa_coho_rows$method)

# a
scale <- "9"
# sets relative value against which other survey methods will be scaled
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
mod_coho_u1z1.list <- list(
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

mod_coho_u1z2.list <- list(
  B = "identity",
  U = "zero",
  Q = "diagonal and equal",
  Z = Z_coho.model,
  A = a_coho.model,
  R = R_coho.model,
  x0 = "equal",
  V0 = "zero",
  tinitx = 0
)

mod_coho_u1z3.list <- list(
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

mod_coho_u2z1.list <- list(
  B = "identity",
  U = "unequal",
  Q = "diagonal and unequal",
  Z = Z_coho.model,
  A = a_coho.model,
  R = R_coho.model,
  x0 = "equal",
  V0 = "zero",
  tinitx = 0
)

mod_coho_u2z2.list <- list(
  B = "identity",
  U = "unequal",
  Q = "diagonal and equal",
  Z = Z_coho.model,
  A = a_coho.model,
  R = R_coho.model,
  x0 = "equal",
  V0 = "zero",
  tinitx = 0
)

mod_coho_u2z3.list <- list(
  B = "identity",
  U = "unequal",
  Q = "equalvarcov",
  Z = Z_coho.model,
  A = a_coho.model,
  R = R_coho.model,
  x0 = "equal",
  V0 = "zero",
  tinitx = 0
)

# fit models
ssm_coho_u1z1 <- MARSS(nosa_coho, model = mod_coho_u1z1.list, method = "kem", control = con.list)
ssm_coho_u1z2 <- MARSS(nosa_coho, model = mod_coho_u1z2.list, method = "kem", control = con.list)
ssm_coho_u1z3 <- MARSS(nosa_coho, model = mod_coho_u1z3.list, method = "kem", control = con.list)
ssm_coho_u2z1 <- MARSS(nosa_coho, model = mod_coho_u2z1.list, method = "kem", control = con.list)
ssm_coho_u2z2 <- MARSS(nosa_coho, model = mod_coho_u2z2.list, method = "kem", control = con.list)
ssm_coho_u2z3 <- MARSS(nosa_coho, model = mod_coho_u2z3.list, method = "kem", control = con.list)

# check fit
ssm_coho_u1z1$AICc
  # 2465.234
ssm_coho_u1z2$AICc
  # 2450.216
ssm_coho_u1z3$AICc
  # 2218.874 - best fit for coho
ssm_coho_u2z1$AICc
  # 2476.841
ssm_coho_u2z2$AICc
  # 2483.41
ssm_coho_u2z3$AICc
  # 2240.003

## model steelhead
# constructing R and a and Z
# R
n_stel <- nrow(nosa_stel)
R_stel.model <- matrix(list(0), n_stel, n_stel)
diag(R_stel.model) <- paste0("r", nosa_stel_rows$method)

# a
scale <- "9"
# sets relative value against which other survey methods will be scaled
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
mod_stel_u1z1.list <- list(
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

mod_stel_u1z2.list <- list(
  B = "identity",
  U = "zero",
  Q = "diagonal and equal",
  Z = Z_stel.model,
  A = a_stel.model,
  R = R_stel.model,
  x0 = "equal",
  V0 = "zero",
  tinitx = 0
)

mod_stel_u1z3.list <- list(
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

mod_stel_u2z1.list <- list(
  B = "identity",
  U = "unequal",
  Q = "diagonal and unequal",
  Z = Z_stel.model,
  A = a_stel.model,
  R = R_stel.model,
  x0 = "equal",
  V0 = "zero",
  tinitx = 0
)

mod_stel_u2z2.list <- list(
  B = "identity",
  U = "unequal",
  Q = "diagonal and equal",
  Z = Z_stel.model,
  A = a_stel.model,
  R = R_stel.model,
  x0 = "equal",
  V0 = "zero",
  tinitx = 0
)

mod_stel_u2z3.list <- list(
  B = "identity",
  U = "unequal",
  Q = "equalvarcov",
  Z = Z_stel.model,
  A = a_stel.model,
  R = R_stel.model,
  x0 = "equal",
  V0 = "zero",
  tinitx = 0
)

# fit models
ssm_stel_u1z1 <- MARSS(nosa_stel, model = mod_stel_u1z1.list, method = "kem", control = con.list)
ssm_stel_u1z2 <- MARSS(nosa_stel, model = mod_stel_u1z2.list, method = "kem", control = con.list)
ssm_stel_u1z3 <- MARSS(nosa_stel, model = mod_stel_u1z3.list, method = "kem", control = con.list)
ssm_stel_u2z1 <- MARSS(nosa_stel, model = mod_stel_u2z1.list, method = "kem", control = con.list)
ssm_stel_u2z2 <- MARSS(nosa_stel, model = mod_stel_u2z2.list, method = "kem", control = con.list)
ssm_stel_u2z3 <- MARSS(nosa_stel, model = mod_stel_u2z3.list, method = "kem", control = con.list)

# check fit
ssm_stel_u1z1$AICc
  # 1336.101
ssm_stel_u1z2$AICc
  # 1343.792
ssm_stel_u1z3$AICc
  # 1215.657 - best fit for steelhead
ssm_stel_u2z1$AICc
  # 1375.884
ssm_stel_u2z2$AICc
  # 1385.541
ssm_stel_u2z3$AICc
  # 1251.642