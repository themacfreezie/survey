## SET WORKING DIR & PACKAGES
library(dlm)
here::i_am("code/sunnyTEST.R")
options(max.print=2000)


## CREATING SYNTHETIC TIME SERIES
## Simple version -
  # 2 poplations (1 large, 1 small)
  # 4 methods (1 restricted to large pop, 1 to small pop, 2 for both pops)
  # most simple - 1 process error variance (Q)
  # slightly more complex - population level process variance
  # starting w/ n = 1000, evenly distributed p1 & p2 = 500 each, all methods = 250

## set seed for reproducibility
set.seed(69420)

## generate process error terms
# same process variance
Q <- runif(1, 0, 1)

# different process variance - slightly more complex
Q_1 <- runif(1, 0, 1)
Q_2 <- runif(1, 0, 1) 

## generate observation error terms
R_a <- runif(1, 0, 1)
R_b <- runif(1, 0, 1) 
R_c <- runif(1, 0, 1)
R_d <- runif(1, 0, 1)
R <- c(R_a, R_b, R_c, R_d)

## generate run size scalers
U <- c(7, 4.5)

## GENERATE TIME SERIES
## generate process time series - 50 time steps
# set params
pops <- 2
methods <- 4
t <- 44 # must be even

# work through it
states_list <- vector("list", pops)
for(j in 1:pops) {
  states_list[[j]][1] <- U[j]
  for (i in 2:(t+1)) {
    states_list[[j]][i] <- states_list[[j]][i - 1] + rnorm(1, 0, sqrt(Q))
  }
}
states_list <- lapply(states_list, dropFirst)
states_df <- data.frame(matrix(unlist(states_list), nrow = length(states_list), byrow=TRUE))

## generate observed time series - 50 time steps
obs_list <- states_list
for(j in 1:pops) {
  for(i in 1:(t/2)) {
    obs_list[[j]][i] <- obs_list[[j]][i] + rnorm(1, 0, R[j])  
  }
  for(i in ((t/2)+1):(t-10)) {
    obs_list[[j]][i] <- obs_list[[j]][i] + rnorm(1, 0, R[(j+2)])  
  }
  for(i in (t-9):t) {
    obs_list[[j]][i] <- obs_list[[j]][i] + rnorm(1, 0, R[abs(j-5)])  
  }
}
obs_df <- data.frame(matrix(unlist(obs_list), nrow = length(obs_list), byrow=TRUE))
  # this is very "neat" - could it be more randomized?

# assign methods and population names
df <- data.frame(t(obs_df))
colnames(df) <- c("Plarge", "Psmall")
df <- stack(df, select = c("Plarge", "Psmall"))
df$method <- c(rep("Ma", (t/2)), 
               rep("Mc", (t/2) - 10), 
               rep("Md", 10), 
               rep("Mb", (t/2)), 
               rep("Mc", (t/2)-10), 
               rep("Md", 10))
df$year <- c(rep((1+1975):(t+1975), pops))

## getting the data in shape for MARSS
# construct popmethod
df$popmethod <- paste0(as.character(df$ind),"_", as.character(df$method))
df <- df[-c(2, 3)]

# set data wide (rows = popid/method, columns = year)
df <- panel_data(df, id = popmethod, wave = year)
df <- widen_panel(df, separator = "_")

# some resorting and cleaning
df <- df[,order(colnames(df))]
df_rows <- as.data.frame(stringr::str_split_fixed(df$popmethod, "_", 2))
colnames(df_rows) <- c("popid", "method")
df <- df[-c(1)]
colnames(df) <- substr(colnames(df), 8, 11)
years <- colnames(df)
df <- as.matrix(df)

## setting up MARSS stuff
# set controls
con.list <- list(maxit = 5000, allow.degen = TRUE)

## model chinook
# constructing R and a and Z
# R
n <- nrow(df)
R.model <- matrix(list(0), n, n)
diag(R.model) <- paste0("r", df_rows$method)

# Z
pops_df <- c(unique(df_rows$popid))
Z.model <- matrix(0, nrow=nrow(df), ncol=length(unique(df_rows$popid)))
for(i in seq(length(pops_df))){
  Z.model[df_rows$popid == pops_df[i], i] <- 1
}

# model list
mod.list <- list(
  B = "identity",
  U = "zero",
  Q = "diagonal and equal",
  Z = Z.model,
  A = "zero",
  R = R.model,
  x0 = "equal",
  V0 = "zero",
  tinitx = 0
)

# run MARSS model
if(!file.exists(here::here("data", "clean", "ssm_dfTEST.rds"))){
  ptm <- proc.time()
  ssm <- MARSS(df, model = mod.list, method = "kem", control = con.list)
  saveRDS(ssm, file=here::here("data", "clean", "ssm_dfTEST.rds"))
  time <- proc.time()[3] - ptm
  time
}
# load in ssm_chin
ssm <- readRDS(file=here::here("data", "clean", "ssm_dfTEST.rds"))