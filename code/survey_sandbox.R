## SET WORKING DIR & PACKAGES

library(here)
library(MARSS)
library(marssTMB)
library(panelr)
library(readxl)

here::i_am("code/survey_sandbox.R")
options(max.print=2000)

# import data
nosa <- read_excel(here("data", "bills_nosa_data.xlsx"))
methods <- read_excel(here("data", "METHODS_ODFW_Recompiled2_and_figs.xlsx"), sheet = "data-wide")


## DATA CLEANING

# subdivide data by species
nosa_chin <- nosa %>% filter(species=="chin")
nosa_coho <- nosa %>% filter(species=="coho")
nosa_stel <- nosa %>% filter(species=="steel")

# two datasets to be set wide - counts (yt) and methods and pdo (dt)
nosa_chin_ct <- nosa_chin[-c(2, 4, 6:48)]
nosa_chin_mt <- nosa_chin[-c(2, 4:12, 14:48)]
nosa_chin_pd <- nosa_chin[-c(2, 4:5, 7:48)]

nosa_coho_ct <- nosa_coho[-c(2, 4, 6:48)]
nosa_coho_mt <- nosa_coho[-c(2, 4:12, 14:48)]
nosa_coho_pd <- nosa_coho[-c(2, 4:6, 8:48)]

nosa_stel_ct <- nosa_stel[-c(2, 4, 6:48)]
nosa_stel_mt <- nosa_stel[-c(2, 4:12, 14:48)]

# natural log of counts (yt)
nosa_chin_ct$ct <- nosa_chin_ct$nosa + 1
nosa_coho_ct$ct <- nosa_coho_ct$nosa + 1
nosa_stel_ct$ct <- nosa_stel_ct$nosa + 1

nosa_chin_ct$ct <- log(nosa_chin_ct$ct)
nosa_coho_ct$ct <- log(nosa_coho_ct$ct)
nosa_stel_ct$ct <- log(nosa_stel_ct$ct)

# drop untransformed counts
nosa_chin_ct <- nosa_chin_ct[-c(3)]
nosa_coho_ct <- nosa_coho_ct[-c(3)]
nosa_stel_ct <- nosa_stel_ct[-c(3)]

# # standardize ln(ct) by population (state)
# nosa_chin_ct <- nosa_chin_ct %>% 
#   group_by(popid) %>% 
#   mutate(standard_ct=scale(ct))
# nosa_chin_ct <- nosa_chin_ct[-c(3)]
# 
# nosa_coho_ct <- nosa_coho_ct %>% 
#   group_by(popid) %>% 
#   mutate(standard_ct=scale(ct))
# nosa_coho_ct <- nosa_coho_ct[-c(3)]
# 
# nosa_stel_ct <- nosa_stel_ct %>% 
#   group_by(popid) %>% 
#   mutate(standard_ct=scale(ct))
# nosa_stel_ct <- nosa_stel_ct[-c(3)]

# set count data wide (rows = IDs, columns = year)
nosa_chin_ct <- panel_data(nosa_chin_ct, id = popid, wave = calyear)
nosa_chin_ctW <- widen_panel(nosa_chin_ct, separator = "_")

nosa_coho_ct <- panel_data(nosa_coho_ct, id = popid, wave = calyear)
nosa_coho_ctW <- widen_panel(nosa_coho_ct, separator = "_")

nosa_stel_ct <- panel_data(nosa_stel_ct, id = popid, wave = calyear)
nosa_stel_ctW <- widen_panel(nosa_stel_ct, separator = "_")

# set methods data wide (rows = IDs, columns = year)
nosa_chin_mt <- panel_data(nosa_chin_mt, id = popid, wave = calyear)
nosa_chin_mtW <- widen_panel(nosa_chin_mt, separator = "_")

nosa_coho_mt <- panel_data(nosa_coho_mt, id = popid, wave = calyear)
nosa_coho_mtW <- widen_panel(nosa_coho_mt, separator = "_")

nosa_stel_mt <- panel_data(nosa_stel_mt, id = popid, wave = calyear)
nosa_stel_mtW <- widen_panel(nosa_stel_mt, separator = "_")

# build stacked matrix of binary indicators for survey methods
species <- list(chin, coho, stel)

for (j in species){
popid_j <- as.data.frame(nosa_j_ctW$popid)
}

as.character(unique(unlist(nosa_chin_mtW[,2:44])))

nosa_chin_mtW_21 <- nosa_chin_mtW[,2:44]
nosa_chin_mtW_21[nosa_chin_mtW_21 == 21] <- 1
nosa_chin_mtW_21[nosa_chin_mtW_21 != 1] <- 0

nosa_chin_mtW_21 <- cbind(popid_chin, nosa_chin_mtW_21)


# save wide dataframes
save(nosa_chin_ctW, file=here("data", "nosa_chin_ctW.Rda"))
save(nosa_chin_mtW, file=here("data", "nosa_chin_mtW.Rda"))

save(nosa_coho_ctW, file=here("data", "nosa_coho_ctW.Rda"))
save(nosa_coho_mtW, file=here("data", "nosa_coho_mtW.Rda"))

save(nosa_stel_ctW, file=here("data", "nosa_stel_ctW.Rda"))
save(nosa_stel_mtW, file=here("data", "nosa_stel_mtW.Rda"))


## MARSS models

# grab year lists, # of observations, & pure count data
years <- names(nosa_chin_ctW)
years <- years[-1]
years <- substring(years, first=4, last=7)

n_chin <- nrow(nosa_chin_ctW)
n_coho <- nrow(nosa_coho_ctW)
n_stel <- nrow(nosa_stel_ctW)

# convert counts to matrix
ct_chin <- data.matrix(nosa_chin_ctW[2:ncol(nosa_chin_ctW)])
ct_coho <- data.matrix(nosa_coho_ctW[2:ncol(nosa_coho_ctW)])
ct_stel <- data.matrix(nosa_stel_ctW[2:ncol(nosa_stel_ctW)])

# convert methods to matrix
mt_chin <- data.matrix(nosa_chin_mtW[2:ncol(nosa_chin_mtW)])
mt_coho <- data.matrix(nosa_coho_mtW[2:ncol(nosa_coho_mtW)])
mt_stel <- data.matrix(nosa_stel_mtW[2:ncol(nosa_stel_mtW)])

# specify matrices for MARSS models
b.model <- "identity"
q.model <- "identity"
z.model <- "identity"
a.model <- "zero"
r.model <- "diagonal and unequal"
x0.model <- "unequal"
v0.model <- "zero"

u_chin.model <- matrix(c(paste0("u", seq(n_chin))))
d_chin.model <- matrix(list(0), n_chin, n_chin)
diag(d_chin.model) <- paste0("d", seq(n_chin))

u_coho.model <- matrix(c(paste0("u", seq(n_coho))))
d_coho.model <- matrix(list(0), n_coho, n_coho)
diag(d_coho.model) <- paste0("d", seq(n_coho))

u_stel.model <- matrix(c(paste0("u", seq(n_stel))))
d_stel.model <- matrix(list(0), n_stel, n_stel)
diag(d_stel.model) <- paste0("d", seq(n_stel))

model_chin.list <- list(
  B = b.model, U = u_chin.model, Q = q.model,
  Z = z.model, A = a.model, R = r.model,
  x0 = x0.model, V0 = v0.model, tinitx = 0,
  D = d_chin.model, d = mt_chin)

model_coho.list <- list(
  B = b.model, U = u_coho.model, Q = q.model,
  Z = z.model, A = a.model, R = r.model,
  x0 = x0.model, V0 = v0.model, tinitx = 0,
  D = d_coho.model, d = mt_coho)

model_stel.list <- list(
  B = b.model, U = u_stel.model, Q = q.model,
  Z = z.model, A = a.model, R = r.model,
  x0 = x0.model, V0 = v0.model, tinitx = 0,
  D = d_stel.model, d = mt_stel)

# specify MARSS model
ptm <- proc.time()
ss_chin <- MARSS(ct_chin, model = model_chin.list, method = "TMB")
proc.time()[3] - ptm

ptm <- proc.time()
ss_coho <- MARSS(ct_coho, model = model_coho.list, method = "TMB")
proc.time()[3] - ptm

ptm <- proc.time()
ss_stel <- MARSS(ct_stel, model = model_stel.list, method = "TMB")
proc.time()[3] - ptm


# grab model estimates (a & R)
trd_chin.est <- ss_chin$par[[2]]
var_chin.est <- ss_chin$par[[3]]

trd_coho.est <- ss_coho$par[[2]]
var_coho.est <- ss_coho$par[[3]]

trd_stel.est <- ss_stel$par[[2]]
var_stel.est <- ss_stel$par[[3]]

