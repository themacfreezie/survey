## SET WORKING DIR & PACKAGES
library(here)
library(MARSS)
library(panelr)
library(tidyverse)

here::i_am("code/primary/09-TScomparison.R")
options(max.print=2000)

# pull in data - model objects
# ssm_chin <- readRDS(file=here::here("data", "clean", "ssm_chinM16.rds"))
# ssm_coho <- readRDS(file=here::here("data", "clean", "ssm_cohoM10.rds"))
# ssm_stel <- readRDS(file=here::here("data", "clean", "ssm_stelM22.rds"))

ssm_chin <- readRDS(file=here::here("data", "clean", "ssm_chinM9.rds"))
ssm_coho <- readRDS(file=here::here("data", "clean", "ssm_cohoM9.rds"))
ssm_stel <- readRDS(file=here::here("data", "clean", "ssm_stelM9.rds"))

# pull in data - observed time series
nosa_chinPOP <- readRDS(file=here::here("data", "clean", "nosa_chinPOP.rds"))
nosa_cohoPOP <- readRDS(file=here::here("data", "clean", "nosa_cohoPOP.rds"))
nosa_stelPOP <- readRDS(file=here::here("data", "clean", "nosa_stelPOP.rds"))

plot(ssm_chin)
  # 29 y time series
autoplot(ssm_chin, plot.type = "fitted.xtT")
summary(ssm_chin)
