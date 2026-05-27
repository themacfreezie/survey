## SET WORKING DIR & PACKAGES
library(here)
library(MARSS)
library(tidyverse)

# load in ssm_coho
ssm_coho <- readRDS(file=here::here("data", "clean", "ssm_cohoM9.rds", sep=""))

# load in raw coho data
load(here("data", "clean", "nosa_codes.Rda"))
nosa <- merge
nosa_coho <- nosa %>% filter(CommonName=="Coho Salmon")
