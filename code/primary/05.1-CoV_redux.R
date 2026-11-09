## SET WORKING DIR & PACKAGES
library(here)
library(tidyverse)

here::i_am("code/primary/05.1-CoV_redux.R")
options(max.print=2000)

# load data
load(here("data", "clean", "merged_ARdata.rda"))
load(here("data", "clean", "mtds.Rda"))

Rdata <- merge(merged_ARdata, mtds, by = c("MethodNameID"))
Rdata <- Rdata[-c(1, 3:5)]
Rdata <- Rdata %>% 
            distinct()

# calculate CoV
Rdata$pre <- 1/Rdata$R
Rdata$CV <- sqrt(exp(Rdata$R)-1)

# reorder columns
ARtable <- Rdata %>% select(CommonName, MethodName, a, R, pre, CV)
