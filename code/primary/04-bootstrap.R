## SET WORKING DIR & PACKAGES
library(here)
library(MARSS)
library(panelr)
library(tidyverse)

here::i_am("code/primary/04-bootstrap.R")
options(max.print=2000)

# pull in data
ssm_chin <- readRDS(file=here::here("data", "clean", paste("ssm_chinM16.rds", sep="")))
ssm_coho <- readRDS(file=here::here("data", "clean", paste("ssm_cohoM10.rds", sep="")))
ssm_stel <- readRDS(file=here::here("data", "clean", paste("ssm_stelM22.rds", sep="")))

# bootstrap estimates
boot_chin <- MARSSboot(ssm_chin, nboot=10000, output="parameters", sim = "parametric", param.gen = "hessian")
saveRDS(boot_chin, file=here::here("data", "clean", paste("boot_chinM16.rds", sep="")))

boot_coho <- MARSSboot(ssm_coho, nboot=10000, output="parameters", sim = "parametric", param.gen = "hessian")
saveRDS(boot_coho, file=here::here("data", "clean", paste("boot_cohoM10.rds", sep="")))

boot_stel <- MARSSboot(ssm_stel, nboot=10000, output="parameters", sim = "parametric", param.gen = "hessian")
saveRDS(boot_stel, file=here::here("data", "clean", paste("boot_stelM22.rds", sep="")))