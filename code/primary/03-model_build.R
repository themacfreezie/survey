## SET WORKING DIR & PACKAGES
library(here)
library(tidyverse)

here::i_am("code/primary/03-model_build.R")
options(max.print=2000)

# pull in data
load(here("data", "clean", "nosa_codes.Rda"))
nosa <- merge