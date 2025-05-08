## SET WORKING DIR & PACKAGES

library(here)
library(MARSS)
library(marssTMB)
library(panelr)
library(readxl)
library(stringr)
library(tidyverse)

here::i_am("code/survey_sandbox.R")
options(max.print=2000)

# import data
nosa <- read_excel(here("data", "bills_nosa_data.xlsx"))

# we'll look at coho
nosa_chin <- nosa %>% filter(species=="chin")
unique(nosa_chin$popid)
  # 27 populations
unique(nosa_chin$method)
  # 15 methods including 0

# selecting two populations with some overlapping methods
table(nosa_chin$popid, nosa_chin$method)
# pilot <- nosa_chin %>% filter(popid==8|popid==11)
pilot <- nosa_chin

# throw away junk
pilot <- pilot[-c(2, 4, 6:10, 12, 14:48)]

# may need to clean to assure nosa = NA -> method = 0 and vice versa

# new popid/method var
pilot$popmethod <- paste0(as.character(pilot$popid),"_", as.character(pilot$method))
# throw away junk
data <- pilot[-c(1, 3, 5)]

# set data wide (rows = popid/method, columns = year)
data <- panel_data(data, id = popmethod, wave = calyear)
data <- widen_panel(data, separator = "_")

# some resorting and cleaning
data <- data[,order(colnames(data))]
data_rows <- as.data.frame(stringr::str_split_fixed(data$popmethod, "_", 2))
colnames(data_rows) <- c("popid", "method")
data <- data[-c(44)]
colnames(data) <- substr(colnames(data), 8, 11)
years <- colnames(data)
data <- as.matrix(data)

# constructing R and a and Z
# R
n <- nrow(data)
R.model <- matrix(list(0), n, n)
diag(R.model) <- paste0("r", data_rows$method)

# a
a.model <- matrix(0, n, 1)
scale <- "21"
  # sets relative value against which other survey methods will be scaled
for(i in 1:length(a.model)){
  if(data_rows$method[i] != scale){
    a.model[i] <- paste0("a", data_rows$method[i])
  }
}

# Z
pops <- c(unique(data_rows$popid))
Z.model <- matrix(0, nrow=nrow(data), ncol=length(unique(data_rows$popid)))
for(i in seq(length(pops))){
  Z.model[data_rows$popid == pops[i], i] <- 1
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

# controls
con.list <- list(maxit = 3000, allow.degen = TRUE)

# run MARSS model
ssm <- MARSS(data, model = mod.list, method = "kem", control = con.list)