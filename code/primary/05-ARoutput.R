## SET WORKING DIR & PACKAGES
library(here)
library(tidyverse)

here::i_am("code/primary/05-ARoutput.R")
options(max.print=2000)

# pull in data
ssm_chin <- readRDS(file=here::here("data", "clean", "ssm_chinM16.rds"))
ssm_coho <- readRDS(file=here::here("data", "clean", "ssm_cohoM10.rds"))
ssm_stel <- readRDS(file=here::here("data", "clean", "ssm_stelM22.rds"))

load(here("data", "clean", "nosa_codes.Rda"))
nosa <- merge

load(here("data", "clean", "populations_list.Rda"))

nmfs_popid <- read_excel(here("data", "raw", "cap-hli.xls"), sheet = "NOSA")

# pull a & r estimates from ssms
# modular code
mod <- ssm_chin

# grab parameter estimates for a & r
A.data <- as.data.frame(mod$par$A)
A.data <- tibble::rownames_to_column(A.data, "MethodNameID")
A.data$MethodNameID <- substr(A.data$MethodNameID, 2, nchar(A.data$MethodNameID))

R.data <- as.data.frame(mod$par$R)
R.data <- tibble::rownames_to_column(R.data, "MethodNameID")
R.data$MethodNameID <- substr(R.data$MethodNameID, 2, nchar(R.data$MethodNameID))

data <- merge(R.data, A.data, by = "MethodNameID", all.x = TRUE, all.y = TRUE)
colnames(data) <- c("MethodNameID","R", "a")
data[is.na(data)] <- 0
  
chin_ARdata <- data 

# modular code
mod <- ssm_coho

# grab parameter estimates for a & r
A.data <- as.data.frame(mod$par$A)
A.data <- tibble::rownames_to_column(A.data, "MethodNameID")
A.data$MethodNameID <- substr(A.data$MethodNameID, 2, nchar(A.data$MethodNameID))

R.data <- as.data.frame(mod$par$R)
R.data <- tibble::rownames_to_column(R.data, "MethodNameID")
R.data$MethodNameID <- substr(R.data$MethodNameID, 2, nchar(R.data$MethodNameID))

data <- merge(R.data, A.data, by = "MethodNameID", all.x = TRUE, all.y = TRUE)
colnames(data) <- c("MethodNameID","R", "a")
data[is.na(data)] <- 0

coho_ARdata <- data 

# modular code
mod <- ssm_stel

# grab parameter estimates for a & r
A.data <- as.data.frame(mod$par$A)
A.data <- tibble::rownames_to_column(A.data, "MethodNameID")
A.data$MethodNameID <- substr(A.data$MethodNameID, 2, nchar(A.data$MethodNameID))

R.data <- as.data.frame(mod$par$R)
R.data <- tibble::rownames_to_column(R.data, "MethodNameID")
R.data$MethodNameID <- substr(R.data$MethodNameID, 2, nchar(R.data$MethodNameID))

data <- merge(R.data, A.data, by = "MethodNameID", all.x = TRUE, all.y = TRUE)
colnames(data) <- c("MethodNameID","R", "a")
data[is.na(data)] <- 0

stel_ARdata <- data 

# clean up nosa
nosa$lnnosa <- log(nosa$NOSA + 1)
nosa <- nosa[-c(5:9)]

# add common names to ARdata
chin_ARdata$CommonName <- "Chinook Salmon"
coho_ARdata$CommonName <- "Coho Salmon"
stel_ARdata$CommonName <- "Steelhead"

merged_chin <- merge(nosa, chin_ARdata, by = c("MethodNameID", "CommonName"))
merged_coho <- merge(nosa, coho_ARdata, by = c("MethodNameID", "CommonName"))
merged_stel <- merge(nosa, stel_ARdata, by = c("MethodNameID", "CommonName"))
  # 173 obs less than nosa (due to dropped observations with little-used methods)

merged_ARdata <- bind_rows(merged_chin, merged_coho, merged_stel)

# collapse around population
collapsed_ARdata <- merged_ARdata %>%
  group_by(PopID, CommonName) %>%
  summarize(mean_lnnosa = mean(lnnosa, na.rm = TRUE), 
            mean_a = mean(a, na.rm = TRUE), 
            mean_R = mean(R, na.rm = TRUE))

# merge in population data
popavgAR <- merge(collapsed_ARdata, pop_list, by = c("PopID"))
  # why are there more lines than in collapsed_ARdata

# steelhead
length(unique(collapsed_ARdata$PopID))
length(unique(pop_list$PopID))
  # 3 doubles?
table(pop_list$PopID)
  # 239, 290, 503

# try this again
pop_list <- pop_list[-c(52, 68, 69), ]
popavgAR <- merge(collapsed_ARdata, pop_list, by = c("PopID", "CommonName"))
  # matched up

# bring in nmfs popid
nmfs_popid <- nmfs_popid[-c(1:4, 7:112)]

lookup_nmfs <- nmfs_popid %>%
  distinct(POPID, NMFS_POPID)

length(unique(lookup_nmfs$POPID))
table(lookup_nmfs$POPID)

length(unique(lookup_nmfs$NMFS_POPID))
table(lookup_nmfs$NMFS_POPID)
  # popid 500 to 506 has na for NMFS popID 
    # 3 steelhead and 3 chinook pops - john day and the lower columbia

lookup_nmfs <- lookup_nmfs %>%
  rename(PopID = POPID,
         NWFSC_POP_ID = NMFS_POPID)

# merge in NWFSC pop IDs (to match with gis)
popavgAR <- left_join(popavgAR, lookup_nmfs, by = "PopID")

# different species
popavgAR_chin <- popavgAR %>% filter(CommonName=="Chinook Salmon")
popavgAR_coho <- popavgAR %>% filter(CommonName=="Coho Salmon")
popavgAR_stel <- popavgAR %>% filter(CommonName=="Steelhead")

# save it up
saveRDS(popavgAR_chin, file=here::here("data", "clean", "popavgAR_chin.rds"))
saveRDS(popavgAR_coho, file=here::here("data", "clean", "popavgAR_coho.rds"))
saveRDS(popavgAR_stel, file=here::here("data", "clean", "popavgAR_stel.rds"))
