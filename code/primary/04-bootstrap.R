## SET WORKING DIR & PACKAGES
library(here)
library(MARSS)
library(panelr)
library(tidyverse)

here::i_am("code/primary/04-bootstrap.R")
options(max.print=2000)

# pull in data
ssm_chin <- readRDS(file=here::here("data", "clean", "ssm_chinM16.rds"))
ssm_coho <- readRDS(file=here::here("data", "clean", "ssm_cohoM10.rds"))
ssm_stel <- readRDS(file=here::here("data", "clean", "ssm_stelM22.rds"))

# bootstrap estimates
boot_chin <- MARSSboot(ssm_chin, nboot=10000, output="parameters", sim = "parametric", param.gen = "hessian")
saveRDS(boot_chin, file=here::here("data", "clean", "ssmBOOT_chinM16.rds"))

boot_coho <- MARSSboot(ssm_coho, nboot=10000, output="parameters", sim = "parametric", param.gen = "hessian")
saveRDS(boot_coho, file=here::here("data", "clean", "ssmBOOT_cohoM10.rds"))

boot_stel <- MARSSboot(ssm_stel, nboot=10000, output="parameters", sim = "parametric", param.gen = "hessian")
saveRDS(boot_stel, file=here::here("data", "clean", "ssmBOOT_stelM22.rds"))