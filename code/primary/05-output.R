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

load(here("data", "clean", "nosa_codes.Rda"))
nosa <- merge

load(here("data", "clean", "populations_list.Rda"))

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
