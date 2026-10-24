## SET WORKING DIR & PACKAGES
library(here)
library(MARSS)
library(panelr)
# library(parallel)
# library(doParallel)
library(tidyverse)

here::i_am("code/primary/04-bootstrap.R")
options(max.print=2000)

# pull in data
ssm_chin <- readRDS(file=here::here("data", "clean", "ssm_chin_Afull.rds"))
ssm_coho <- readRDS(file=here::here("data", "clean", "ssm_coho_Afull.rds"))
ssm_stel <- readRDS(file=here::here("data", "clean", "ssm_stel_Afull.rds"))

ssm_chinU <- readRDS(file=here::here("data", "clean", "ssm_chin_AfullU.rds"))
ssm_cohoU <- readRDS(file=here::here("data", "clean", "ssm_coho_AfullU.rds"))
ssm_stelU <- readRDS(file=here::here("data", "clean", "ssm_stel_AfullU.rds"))

# bootstrap estimates - 100 draws
if(!file.exists(here::here("data", "clean", "ssmBOOT_chin_Afull.rds"))){
  boot_chin <- MARSSboot(ssm_chin, nboot=100, output="parameters", sim = "parametric")
  saveRDS(boot_chin, file=here::here("data", "clean", "ssmBOOT_chin_Afull.rds"))
}
# load in
boot_chin <- readRDS(file=here::here("data", "clean", "ssmBOOT_chin_Afull.rds"))

if(!file.exists(here::here("data", "clean", "ssmBOOT_coho_Afull.rds"))){
  boot_coho <- MARSSboot(ssm_coho, nboot=100, output="parameters", sim = "parametric")
  saveRDS(boot_coho, file=here::here("data", "clean", "ssmBOOT_coho_Afull.rds"))
}
# load in
boot_coho <- readRDS(file=here::here("data", "clean", "ssmBOOT_coho_Afull.rds"))

if(!file.exists(here::here("data", "clean", "ssmBOOT_stel_Afull.rds"))){
  boot_stel <- MARSSboot(ssm_stel, nboot=100, output="parameters", sim = "parametric")
  saveRDS(boot_stel, file=here::here("data", "clean", "ssmBOOT_stel_Afull.rds"))
}  
# load in
boot_stel <- readRDS(file=here::here("data", "clean", "ssmBOOT_stel_Afull.rds"))

# bootstrap estimates - 1000 draws
if(!file.exists(here::here("data", "clean", "ssmBOOT_chin_Afull1K.rds"))){
  boot_chin1K <- MARSSboot(ssm_chin, nboot=1000, output="parameters", sim = "parametric")
  saveRDS(boot_chin1K, file=here::here("data", "clean", "ssmBOOT_chin_Afull1K.rds"))
}
# load in
boot_chin1K <- readRDS(file=here::here("data", "clean", "ssmBOOT_chin_Afull1K.rds"))

if(!file.exists(here::here("data", "clean", "ssmBOOT_coho_Afull1K.rds"))){
  boot_coho1K <- MARSSboot(ssm_coho, nboot=1000, output="parameters", sim = "parametric")
  saveRDS(boot_coho1K, file=here::here("data", "clean", "ssmBOOT_coho_Afull1K.rds"))
}
# load in
boot_coho1K <- readRDS(file=here::here("data", "clean", "ssmBOOT_coho_Afull1K.rds"))

if(!file.exists(here::here("data", "clean", "ssmBOOT_stel_Afull1K.rds"))){
  boot_stel1K <- MARSSboot(ssm_stel, nboot=1000, output="parameters", sim = "parametric")
  saveRDS(boot_stel1K, file=here::here("data", "clean", "ssmBOOT_stel_Afull1K.rds"))
}  
# load in
boot_stel1K <- readRDS(file=here::here("data", "clean", "ssmBOOT_stel_Afull1K.rds"))

# # bootstrap estimates - 10000 draws
# if(!file.exists(here::here("data", "clean", "ssmBOOT_chin_Afull10K.rds"))){
#   boot_chin10K <- MARSSboot(ssm_chin, nboot=10000, output="parameters", sim = "parametric")
#   saveRDS(boot_chin10K, file=here::here("data", "clean", "ssmBOOT_chin_Afull10K.rds"))
# }
# # load in
# boot_chin10K <- readRDS(file=here::here("data", "clean", "ssmBOOT_chin_Afull10K.rds"))
# 
# if(!file.exists(here::here("data", "clean", "ssmBOOT_coho_Afull10K.rds"))){
#   boot_coho10K <- MARSSboot(ssm_coho, nboot=10000, output="parameters", sim = "parametric")
#   saveRDS(boot_coho10K, file=here::here("data", "clean", "ssmBOOT_coho_Afull10K.rds"))
# }
# # load in
# boot_coho10K <- readRDS(file=here::here("data", "clean", "ssmBOOT_coho_Afull10K.rds"))
# 
# if(!file.exists(here::here("data", "clean", "ssmBOOT_stel_Afull10K.rds"))){
#   boot_stel10K <- MARSSboot(ssm_stel, nboot=10000, output="parameters", sim = "parametric")
#   saveRDS(boot_stel10K, file=here::here("data", "clean", "ssmBOOT_stel_Afull10K.rds"))
# }  
# # load in
# boot_stel10K <- readRDS(file=here::here("data", "clean", "ssmBOOT_stel_Afull10K.rds"))

# bootstrap estimates - 100 draws
if(!file.exists(here::here("data", "clean", "ssmBOOT_chin_AfullU.rds"))){
  boot_chinU <- MARSSboot(ssm_chinU, nboot=100, output="parameters", sim = "parametric")
  saveRDS(boot_chinU, file=here::here("data", "clean", "ssmBOOT_chin_AfullU.rds"))
}
# load in
boot_chinU <- readRDS(file=here::here("data", "clean", "ssmBOOT_chin_AfullU.rds"))

if(!file.exists(here::here("data", "clean", "ssmBOOT_coho_AfullU.rds"))){
  boot_cohoU <- MARSSboot(ssm_cohoU, nboot=100, output="parameters", sim = "parametric")
  saveRDS(boot_cohoU, file=here::here("data", "clean", "ssmBOOT_coho_AfullU.rds"))
}
# load in
boot_cohoU <- readRDS(file=here::here("data", "clean", "ssmBOOT_coho_AfullU.rds"))

if(!file.exists(here::here("data", "clean", "ssmBOOT_stel_AfullU.rds"))){
  boot_stelU <- MARSSboot(ssm_stelU, nboot=100, output="parameters", sim = "parametric")
  saveRDS(boot_stelU, file=here::here("data", "clean", "ssmBOOT_stel_AfullU.rds"))
}  
# load in
boot_stelU <- readRDS(file=here::here("data", "clean", "ssmBOOT_stel_AfullU.rds"))

# bootstrap estimates - 1000 draws
if(!file.exists(here::here("data", "clean", "ssmBOOT_chin_AfullU1K.rds"))){
  boot_chinU1K <- MARSSboot(ssm_chinU, nboot=1000, output="parameters", sim = "parametric")
  saveRDS(boot_chinU1K, file=here::here("data", "clean", "ssmBOOT_chin_AfullU1K.rds"))
}
# load in
boot_chinU1K <- readRDS(file=here::here("data", "clean", "ssmBOOT_chin_AfullU1K.rds"))

if(!file.exists(here::here("data", "clean", "ssmBOOT_coho_AfullU1K.rds"))){
  boot_cohoU1K <- MARSSboot(ssm_cohoU, nboot=1000, output="parameters", sim = "parametric")
  saveRDS(boot_cohoU1K, file=here::here("data", "clean", "ssmBOOT_coho_AfullU1K.rds"))
}
# load in
boot_cohoU1K <- readRDS(file=here::here("data", "clean", "ssmBOOT_coho_AfullU1K.rds"))

if(!file.exists(here::here("data", "clean", "ssmBOOT_stel_AfullU1K.rds"))){
  boot_stelU1K <- MARSSboot(ssm_stelU, nboot=1000, output="parameters", sim = "parametric")
  saveRDS(boot_stelU1K, file=here::here("data", "clean", "ssmBOOT_stel_AfullU1K.rds"))
}  
# load in
boot_stelU1K <- readRDS(file=here::here("data", "clean", "ssmBOOT_stel_AfullU1K.rds"))
