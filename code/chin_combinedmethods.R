# DESCRIPTION: Compares combined methods (15 + 21 = 29) in 5 chinook populations
# 15 -> Mark-Recapture estimate at weir
# 21 -> Redd count expansion * Fish per redd estimate
# 29 -> Mark-Recapture estimate at weir + Redd Count expansion * Fish per redd estimate

## SET WORKING DIR & PACKAGES
library(here)
library(MARSS)
library(panelr)
library(readxl)
library(stringr)
library(tidyverse)

# here::i_am("code/550models.R")
options(max.print=2000)

# import data
nosa <- read_excel(here("data", "bills_nosa_data.xlsx"))

# some exploratory tables
table(nosa$species, nosa$popid)
table(nosa$species, nosa$method)
methodsTable <- nosa %>% 
  pivot_wider(names_from = "calyear", values_from = "method", id_cols = "popid")
print(methodsTable)

# different species
nosa_chin <- nosa %>% filter(species=="chin")

methodsTable_chin <- nosa_chin %>% 
  pivot_wider(names_from = "calyear", values_from = "method", id_cols = "popid")
print(methodsTable_chin)
  #7 to 12

# keep pops of interest
nosa_chin <- nosa_chin %>% filter(popid < 13)

unique(nosa_chin$popid)
unique(nosa_chin$method)
  #215 obs across 5 populations

methodsTable_chinCOMBO <- nosa_chin %>% 
  pivot_wider(names_from = "calyear", values_from = "method", id_cols = "popid")
print(methodsTable_chinCOMBO)
  # only 3 missing observations

# throw away junk
nosa_chin <- nosa_chin[-c(2, 4, 6:10, 12, 14:48)]

nosa_chin <- nosa_chin[nosa_chin$method != 0, ]
  # drop handful of observations with no method recorded

# new popid/method var
nosa_chin$popmethod <- paste0(as.character(nosa_chin$popid),"_", as.character(nosa_chin$method))
# throw away more junk
nosa_chin <- nosa_chin[-c(1, 3, 5)]

# drop some missing nosa values
nosa_chin <- na.omit(nosa_chin)
  # only 2

# set data wide (rows = popid/method, columns = year)
nosa_chin <- panel_data(nosa_chin, id = popmethod, wave = calyear)
nosa_chin <- widen_panel(nosa_chin, separator = "_")

# resorting and cleaning
nosa_chin <- nosa_chin[,order(colnames(nosa_chin))]
nosa_chin_rows <- as.data.frame(stringr::str_split_fixed(nosa_chin$popmethod, "_", 2))
colnames(nosa_chin_rows) <- c("popid", "method")
nosa_chin <- nosa_chin[-c(44)]
colnames(nosa_chin) <- substr(colnames(nosa_chin), 8, 11)
years <- colnames(nosa_chin)
nosa_chin <- as.matrix(nosa_chin)

# set controls
con.list <- list(maxit = 5000, allow.degen = TRUE)

## model chin
# constructing R and a and Z
# R
n_chin <- nrow(nosa_chin)
R_chin.model <- matrix(list(0), n_chin, n_chin)
diag(R_chin.model) <- paste0("r", nosa_chin_rows$method)

# a
scale <- "29"
# sets relative value against which other survey methods will be scaled, combined method
# 29 -> Mark-Recapture estimate at weir + Redd Count expansion * Fish per redd estimate
a_chin.model <- matrix(list(0), n_chin, 1)
for(i in 1:length(a_chin.model)){
  if(nosa_chin_rows$method[i] != scale){
    a_chin.model[i] <- paste0("a", nosa_chin_rows$method[i])
  }
}

# Z
pops_chin <- c(unique(nosa_chin_rows$popid))
Z_chin.model <- matrix(0, nrow=nrow(nosa_chin), ncol=length(unique(nosa_chin_rows$popid)))
for(i in seq(length(pops_chin))){
  Z_chin.model[nosa_chin_rows$popid == pops_chin[i], i] <- 1
}

# model list
mod_chinCOMBO.list <- list(
  B = "identity",
  U = "zero",
  Q = "diagonal and unequal",
  Z = Z_chin.model,
  A = a_chin.model,
  R = R_chin.model,
  x0 = "equal",
  V0 = "zero",
  tinitx = 0
)

# run MARSS model
if(!file.exists(here::here("data", paste("ssm_chinCOMBO_M", scale, ".rds", sep="")))){
  ssm_chinCOMBO <- MARSS(nosa_chin, model = mod_chinCOMBO.list, method = "kem", control = con.list)
  saveRDS(ssm_chinCOMBO, file=here::here("data", paste("ssm_chinCOMBO_M", scale, ".rds", sep="")))
}
# load in ssm_chin
ssm_chinCOMBO <- readRDS(file=here::here("data", paste("ssm_chinCOMBO_M", scale, ".rds", sep="")))

# modular code
mod <- ssm_chinCOMBO

# grab parameter estimates for a & r
A.data <- as.data.frame(mod$par$A)
A.data <- tibble::rownames_to_column(A.data, "method")
A.data$method <- substr(A.data$method, 2, nchar(A.data$method))

R.data <- as.data.frame(mod$par$R)
R.data <- tibble::rownames_to_column(R.data, "method")
R.data$method <- substr(R.data$method, 2, nchar(R.data$method))

data <- merge(R.data, A.data, by = "method", all.x = TRUE, all.y = TRUE)
colnames(data) <- c("method","R", "a")
data[is.na(data)] <- 0

# keep methods of interest
data <- data %>% filter(method != 22)
data <- data %>% filter(method != 30)

# plot
plot(data$R, data$a)

# pull in legend
legend <- read_excel(here("data", "method_key.xlsx"), col_names = TRUE)
legend$method <- legend$Method
legend <- legend[-c(1,3)]

data <- merge(data, legend, by = "method", all.x = TRUE, all.y = TRUE)
data <- na.omit(data)

# better plot
points <- ggplot(data=data, aes(x=R, y=a, color=Name)) + 
  geom_text(aes(label = method), size=8) +
  # geom_point(size=8) + 
  labs(x = "Variance",
       title='Chinook',
       y='Relative Bias') +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_color_manual(values = c("#c1a13c",
                                "#c772c5",
                                # "#5b3c90",
                                # "#b85c37",
                                # "#b94656",
                                # "#b0457b",
                                "#729a43"
                                # "#6d85db",
                                # "#4dc48f"
  )) +
  theme_classic()
chin_points <- points + guides(color=guide_legend(title="Method"))
chin_points <- chin_points + theme(legend.position = "bottom",  legend.direction = "vertical")
chin_points
chin_data <- data
chin_data$species <- "Chinook"
