## SET WORKING DIR & PACKAGES
library(here)
library(tidyverse)

here::i_am("code/primary/02-method_labels.R")
options(max.print=2000)

# pull in data
load(here("data", "clean", "nosa_dat.Rda"))
nosa <- nosa_mod.dat
load(here("data", "clean", "methods_list.Rda"))
mtds <- method_list

# cleaning methods look-up table
mtds <- mtds %>%
  # fix capitalization for ID 20
  mutate(MethodName = ifelse(MethodNameID == 20, "PIT-tag expansion estimate", MethodName)) %>%
  # keep only one row per ID
  distinct(MethodNameID, .keep_all = TRUE)

mtds <- mtds %>%
  mutate(
    # if the ID starts with 21 (e.g., 21.1, 21.2), make it exactly 21
    MethodNameID = ifelse(floor(MethodNameID) == 21, 21, MethodNameID),
    # update the name for all ID 21 rows
    MethodName = ifelse(MethodNameID == 21, 
                        "Index redd count expansion * Fish per redd estimate", 
                        MethodName)
  ) %>% 
  distinct(MethodNameID, .keep_all = TRUE)
