# DESCRIPTION: Compares 'AUC:Monitoring Area' methods against 'AUC:Population' method
# 1 -> Area Under Curve: Monitoring Area (Peak count)
# 2 -> Area Under Curve: Monitoring Area (Total live spawners)
# 3 -> Area Under Curve: Population (Total live spawners)

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
nosa_coho <- nosa %>% filter(species=="coho")

methodsTable_coho <- nosa_coho %>% 
  pivot_wider(names_from = "calyear", values_from = "method", id_cols = "popid")
print(methodsTable_coho)
  #113 to 162 & 164

nosa_coho <- nosa_coho %>% filter(popid > 112)
nosa_coho <- nosa_coho %>% filter(popid < 165)
nosa_coho <- nosa_coho %>% filter(popid != 163)

nosa_coho <- nosa_coho %>% filter(calyear > 1989)
nosa_coho <- nosa_coho %>% filter(calyear < 2020)
  # things get weird after covid

unique(nosa_coho$popid)
unique(nosa_coho$method)
  #480 obs across 17 populations w/ only method 1 & 2, or 3

methodsTable_cohoAUC <- nosa_coho %>% 
  pivot_wider(names_from = "calyear", values_from = "method", id_cols = "popid")
print(methodsTable_cohoAUC)
  # data frame is full - 30 observations per population
  # 1990-97 method 1, 1998-2003 method 2, 2004-19 method 3 (exepct pop 113)

# throw away junk
nosa_coho <- nosa_coho[-c(2, 4, 6:10, 12, 14:48)]

nosa_coho <- nosa_coho[nosa_coho$method != 7, ]
  # some obs with method 7 in pop 113 to be dropped

# new popid/method var
nosa_coho$popmethod <- paste0(as.character(nosa_coho$popid),"_", as.character(nosa_coho$method))
# throw away more junk
nosa_coho <- nosa_coho[-c(1, 3, 5)]

# set data wide (rows = popid/method, columns = year)
nosa_coho <- panel_data(nosa_coho, id = popmethod, wave = calyear)
nosa_coho <- widen_panel(nosa_coho, separator = "_")

# resorting and cleaning
nosa_coho <- nosa_coho[,order(colnames(nosa_coho))]
nosa_coho_rows <- as.data.frame(stringr::str_split_fixed(nosa_coho$popmethod, "_", 2))
colnames(nosa_coho_rows) <- c("popid", "method")
nosa_coho <- nosa_coho[-c(31)]
colnames(nosa_coho) <- substr(colnames(nosa_coho), 8, 11)
years <- colnames(nosa_coho)
nosa_coho <- as.matrix(nosa_coho)

# set controls
con.list <- list(maxit = 5000, allow.degen = TRUE)

## model coho
# constructing R and a and Z
# R
n_coho <- nrow(nosa_coho)
R_coho.model <- matrix(list(0), n_coho, n_coho)
diag(R_coho.model) <- paste0("r", nosa_coho_rows$method)

# a
scale <- "3"
  # sets relative value against which other survey methods will be scaled
  # 3 -> Area Under Curve: Population (Total live spawners)
a_coho.model <- matrix(list(0), n_coho, 1)
for(i in 1:length(a_coho.model)){
  if(nosa_coho_rows$method[i] != scale){
    a_coho.model[i] <- paste0("a", nosa_coho_rows$method[i])
  }
}

## work on this
# a_coho2.model <- matrix(list(0), n_coho2, 1)
# for(i in 1:length(a_coho2.model)){
#   if(nosa_coho2_rows$method[i] != scale){
#     a_coho2.model[i] <- paste0("a", nosa_coho2_rows$method[i])
#   }
# }

# Z
pops_coho <- c(unique(nosa_coho_rows$popid))
Z_coho.model <- matrix(0, nrow=nrow(nosa_coho), ncol=length(unique(nosa_coho_rows$popid)))
for(i in seq(length(pops_coho))){
  Z_coho.model[nosa_coho_rows$popid == pops_coho[i], i] <- 1
}


# model list
mod_cohoAUC.list <- list(
  B = "identity",
  U = "zero",
  Q = "diagonal and unequal",
  Z = Z_coho.model,
  A = a_coho.model,
  R = R_coho.model,
  x0 = "equal",
  V0 = "zero",
  tinitx = 0
)

# mod_coho2AUC.list <- list(
#   B = "identity",
#   U = "zero",
#   Q = "diagonal and unequal",
#   Z = Z_coho.model,
#   A = a_coho2.model,
#   R = R_coho.model,
#   x0 = "equal",
#   V0 = "zero",
#   tinitx = 0
# )

# run MARSS model
if(!file.exists(here::here("data", paste("ssm_cohoAUC_M", scale, ".rds", sep="")))){
  ssm_cohoAUC <- MARSS(nosa_coho, model = mod_cohoAUC.list, method = "kem", control = con.list)
  saveRDS(ssm_cohoAUC, file=here::here("data", paste("ssm_cohoAUC_M", scale, ".rds", sep="")))
}
# load in ssm_coho
ssm_cohoAUC <- readRDS(file=here::here("data", paste("ssm_cohoAUC_M", scale, ".rds", sep="")))

# # run MARSS model
# if(!file.exists(here::here("data", paste("ssm_coho2AUC_M", scale, ".rds", sep="")))){
#   ssm_coho2AUC <- MARSS(nosa_coho2, model = mod_coho2AUC.list, method = "kem", control = con.list)
#   saveRDS(ssm_coho2AUC, file=here::here("data", paste("ssm_coho2AUC_M", scale, ".rds", sep="")))
# }
# # load in ssm_coho
# ssm_coho2AUC <- readRDS(file=here::here("data", paste("ssm_coho2AUC_M", scale, ".rds", sep="")))

# modular code
mod <- ssm_cohoAUC

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

# plot
plot(data$R, data$a)

# pull in legend
legend <- read_excel(here("data", "method_key.xlsx"), col_names = TRUE)
legend$method <- legend$Method
legend <- legend[-c(1,2)]

data <- merge(data, legend, by = "method", all.x = TRUE, all.y = TRUE)
data <- na.omit(data)

# better plot
ggplot(data=data, aes(x=R, y=a, color=Group)) + 
  # geom_point(size=4, show.legend = NA) + 
  geom_text(aes(label = method), size=8) + 
  labs(x = "variance",
       y='bias') +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(expand = c(0.01, 0)) +
  theme_classic()

# better plot
points <- ggplot(data=data, aes(x=R, y=a, color=Group)) + 
  geom_text(aes(label = method), size=8) +
  # geom_point(size=8) + 
  labs(x = "Variance",
       title='Coho',
       y='Relative Bias') +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_color_manual(values = c("#c1a13c",
                                "#c772c5",
                                # "#5b3c90",
                                "#b85c37",
                                "#b94656",
                                # "#b0457b",
                                "#729a43"
                                # "#6d85db",
                                # "#4dc48f"
  )) +
  theme_classic()
coho_points <- points + guides(color=guide_legend(title="Method Group"))
coho_points
coho_data <- data
coho_data$species <- "coho"
