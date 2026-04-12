## SET WORKING DIR & PACKAGES
library(here)
library(readxl)
library(tidyverse)

here::i_am("code/dataclean.R")
options(max.print=2000)

# import data
nosa <- read_excel(here("data", "raw", "cap-hli.xls"), sheet = "NOSA")
methods <- read_excel(here("data", "raw", "NCA_NOSA_Methods_ODFW_20260204.xlsx"), sheet = "MethodsSummary")

# get tester data for expanding method
test <- methods[-c(1,3:11,15)]

# test
test_long <- test %>%
  rowwise() %>%
  # create a sequence of years for each row
  mutate(Year = list(seq(FirstSpawningYear, LastSpawningYear))) %>%
  # unnest list into individual rows
  unnest(Year) %>%
  # remove the old start/end columns if they are no longer needed
  select(PopID, MethodNameID, Year)
    # seems to do the trick

# full methods
methods_long <- methods %>%
  rowwise() %>%
  mutate(Year = list(seq(FirstSpawningYear, LastSpawningYear))) %>%
  unnest(Year) %>%
  select(PopID, MethodNameID, Year, CommonName, Run, `MPG/Stratum`, CommonPopName, TimeSeriesID, MethodName)


# pull nosa data to match with methods_long
nosa_sub <- nosa[-c(1:2, 4, 6, 8, 11:13, 15:17, 19, 20, 22, 24:26, 28:83, 86:112)]

# merge nosa data w/ methods
nosa_sub <- nosa_sub %>%
  rename(
    PopID = POPID,
    Year = SPAWNINGYEAR
  )

nosa_merged <- nosa_sub %>%
  left_join(methods_long, by = c("PopID", "Year"))
  # hmm, too many observations in merged data - there must be doubles somewhere

methodsDBL <- methods_long %>% 
  count(PopID, Year) %>% 
  filter(n > 1)

nosaDBL <- nosa_sub %>% 
  count(PopID, Year) %>% 
  filter(n > 1)

