## SET WORKING DIR & PACKAGES
library(here)
library(readxl)
library(tidyverse)

here::i_am("code/primary/01-dataclean.R")
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

## issues w/ pop 11, 58, & 85
## bill adds up the nosa value (including jacks)
## methods is trickier 
  # 11 looks like its the same in all observations
    # differetn time series ID for each location even though the method is the same...
  # 58 has different methods in some years (2009)
    # Bill assigns one of the methods (29) to these...
  # 85 does not have doubles in the counts, only methods
    # Numbered 21 and 21.1, I think we could roll these all into 21...

### FOR NOW for purity, I'm just going to drop the obs w/ double NOSA 
nosa_pure <- nosa_sub %>%
  filter(
    PopID != 11,
    !(PopID == 58 & Year > 2007)
  )

nosaDBLpure <- nosa_pure %>% 
  count(PopID, Year) %>% 
  filter(n > 1)

methods_pure <- methods_long %>%
  filter(
    PopID != 11,
    !(PopID == 58 & Year > 2007),
    !(PopID == 85 & MethodNameID == 21.1)
  )

methodsDBLpure <- methods_pure %>% 
  count(PopID, Year) %>% 
  filter(n > 1)
    ## THIS IS JUST FOR NOW - WILL ADDRESS THESE POPS

nosa_merged <- nosa_pure %>%
  left_join(methods_pure, by = c("PopID", "Year"))
  # same number of obs as nosa_pure

# create NOSA variable 
# if NOSAIJ exists, NOSA = NOSAIJ
# if NOSAIJ is 'na' but NOSAEJ exists, NOSA = NOSAEJ
# if both are 'na', drop observation
nosa_merged2 <- nosa_merged %>%
  # pick NOSAIJ first, then NOSAEJ if NOSAIJ is missing
  mutate(NOSA = coalesce(NOSAIJ, NOSAEJ)) %>%
  # drop if both were NA
  filter(!is.na(NOSA))
nosa_merged2 <- nosa_merged2[-c(9,10)]

# drop year prior to 1980 (why?)
  # this is what I initally had, but it seems like it's throwing away good data?
nosa_merged2 <- nosa_merged2 %>%
  filter(Year >= 1980)
nosa_merged2 <- nosa_merged2 %>%
  filter(MethodNameID != 0)
  # no survey conducted (even though some have counts? interpolation?)
nosa_merged2 <- nosa_merged2 %>%
  filter(MethodNameID != 99)
  # hatchery counts - j&k say do not use these
nosa_merged2 <- nosa_merged2 %>%
  filter(!is.na(MethodNameID))

# what do I absoultely need for models?
nosa_mod.dat <- nosa_merged2[-c(1,3:6,8:10, 14, 15)]

# method list
method_list <- nosa_mod.dat[-c(1, 2, 4:6, 8)]
method_list <- method_list %>%
  distinct()

# population info
pop_list <- nosa_merged2[-c(7:11, 16:18)]
pop_list <- pop_list %>%
  distinct()

# let's save what we've got here...
save(nosa_mod.dat, file=here("data", "clean", "nosa_dat.Rda"))
save(method_list, file=here("data", "clean", "methods_list.Rda"))
save(pop_list, file=here("data", "clean", "populations_list.Rda"))

