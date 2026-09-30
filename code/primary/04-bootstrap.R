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

# bootstrap estimates
boot_chin <- MARSSboot(ssm_chin, nboot=100, output="parameters", sim = "parametric")
saveRDS(boot_chin, file=here::here("data", "clean", "ssmBOOT_chin_Afull.rds"))

boot_coho <- MARSSboot(ssm_coho, nboot=100, output="parameters", sim = "parametric")
saveRDS(boot_coho, file=here::here("data", "clean", "ssmBOOT_coho_Afull.rds"))

boot_stel <- MARSSboot(ssm_stel, nboot=100, output="parameters", sim = "parametric")
saveRDS(boot_stel, file=here::here("data", "clean", "ssmBOOT_stel_Afull.rds"))
  
