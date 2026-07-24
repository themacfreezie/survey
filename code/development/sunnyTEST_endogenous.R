## SET WORKING DIR & PACKAGES
library(dlm)
library(here)
library(MARSS)
library(MASS)
library(panelr)
library(tidyverse)

here::i_am("code/development/sunnyTEST_endogenous.R")
options(max.print=2000)

## creating synthetic time series
set.seed(694201) # set seed for reproducibility

# set params
P_populations <- 75
M_methods <- 15
T_steps <- 40
max <- 3 # maximum number of large/small exclusive survey methods

# assign populations to categories (25 each)
pop_categories <- tibble(
  Population_ID = 1:P_populations,
  Category = sample(rep(c("Small", "Medium", "Large"), each = P_populations / 3))
)

# determine number of exclusive methods for small and large (set max)
num_exclusive_small <- sample(1:max, 1)
num_exclusive_large <- sample(1:max, 1)

# randomly assign specific methods to be exclusive
all_methods <- 1:M_methods
exclusive_small_methods <- sample(all_methods, num_exclusive_small)
remaining_methods <- setdiff(all_methods, exclusive_small_methods)
exclusive_large_methods <- sample(remaining_methods, num_exclusive_large)

methods_for_small  <- c(setdiff(all_methods, exclusive_large_methods))
methods_for_medium <- setdiff(all_methods, c(exclusive_small_methods, exclusive_large_methods))
methods_for_large  <- c(setdiff(all_methods, exclusive_small_methods))

# generate Q matrix (population level process variance)
W <- matrix(runif(P_populations * P_populations, min = -2, max = 2), nrow = P_populations, ncol = P_populations)
W <- diag(diag(W))
Q <- W %*% t(W)
Q <- Q + diag(0.1, P_populations)

# generate R matrix (population level process variance)
V <- matrix(runif(M_methods * M_methods, min = -1, max = 1), nrow = M_methods, ncol = M_methods)
V <- diag(diag(V))
R <- V %*% t(V)
R <- R + diag(0.1, M_methods)

# simulate states (x_t,p) with overlapping tails
x <- matrix(0, nrow = T_steps, ncol = P_populations)
for(p in 1:P_populations) {
  cat <- pop_categories$Category[p]
  if(cat == "Small") {
    x[1, p] <- rnorm(1, mean = 2.75, sd = 0.8)
  }
  if(cat == "Medium") {
    x[1, p] <- rnorm(1, mean = 5.50, sd = 1.0)
  }
  if(cat == "Large") {
    x[1, p] <- rnorm(1, mean = 8.75, sd = 1.2)
  }
}

for (t in 2:T_steps) {
  v_t <- mvrnorm(1, mu = rep(0, P_populations), Sigma = Q)
  proposed_x <- x[t - 1, ] + v_t
  x[t, ] <- pmin(pmax(proposed_x, 0.6931472), 10.927161) 
}

# generate noise matrix
w_matrix <- mvrnorm(T_steps, mu = rep(0, M_methods), Sigma = R) 
chosen_method_idx <- matrix(NA, nrow = T_steps, ncol = P_populations)

# helper function to get legal methods for a population
get_legal_methods <- function(pop_idx) {
  cat <- pop_categories$Category[pop_idx]
  if(cat == "Small")  return(methods_for_small)
  if(cat == "Medium") return(methods_for_medium)
  return(methods_for_large)
}

# time step 1: randomly pick an initial legal method
for(p in 1:P_populations) {
  legal <- get_legal_methods(p)
  chosen_method_idx[1, p] <- sample(legal, 1)
}

# time steps 2 to T: Apply switching probability within legal pools
switch_prob <- 0.15
for (t in 2:T_steps) {
  will_switch <- runif(P_populations) < switch_prob
  chosen_method_idx[t, ] <- chosen_method_idx[t - 1, ]
  
  if (any(will_switch)) {
    switcher_indices <- which(will_switch)
    chosen_method_idx[t, switcher_indices] <- sapply(switcher_indices, function(p) {
      prev_method <- chosen_method_idx[t - 1, p]
      legal <- get_legal_methods(p)
      pool <- setdiff(legal, prev_method)
      # fallback if a pool only has 1 method total and cannot switch
      if(length(pool) == 0) return(prev_method) 
      sample(pool, 1)
    })
  }
}

# build the final dataframe
df_list <- list()
for (p in 1:P_populations) {
  p_methods <- chosen_method_idx[, p]
  p_states <- x[, p]
  p_noise <- w_matrix[matrix(c(1:T_steps, p_methods), ncol = 2)]
  
  df_list[[p]] <- data.frame(
    Time = 1:T_steps,
    Population = paste0("Population_", p),
    Method = paste0("Method_", p_methods),
    True_State = p_states,
    Observation = p_states + p_noise
  )
}

df <- do.call(rbind, df_list)

# grab variance terms
df_popvar <- data.frame(Population = paste0("Population_", 1:P_populations), TRUEvariance = diag(Q))
df_popvar$Population <- sprintf("%02d", as.numeric(gsub("Population_", "", df_popvar$Population)))
df_popvar$Population <- as.numeric(df_popvar$Population)
colnames(pop_categories) <- c("Population", "Category")
df_popvar <- df_popvar %>% 
  left_join(pop_categories, by = "Population") %>% 
  select(Population, Category, TRUEvariance)

df_metvar <- data.frame(Method = paste0("Method_", 1:M_methods), TRUEvariance = diag(R))
df_metvar$Method <- sprintf("%02d", as.numeric(gsub("Method_", "", df_metvar$Method)))
df_metvar$Method <- as.numeric(df_metvar$Method)
df_metvar <- df_metvar %>%
  mutate(
    Category = case_when(
      Method %in% exclusive_small_methods ~ "Exclusive to Small",
      Method %in% exclusive_large_methods ~ "Exclusive to Large",
      TRUE                                    ~ "Shared/Common"
    )
  ) %>%
  select(Method, Category, TRUEvariance)

# drop strings and format variables
df$Population <- sprintf("%02d", as.numeric(gsub("Population_", "", df$Population)))
df$Method <- sprintf("%02d", as.numeric(gsub("Method_", "", df$Method)))
df$Time <- as.numeric(sprintf("%02d", df$Time)) + 1979
df$popmethod <- paste0(df$Population, "_", df$Method)

# Format variance references
df_popvar$Parameter <- sprintf("Population_%02d", as.numeric(gsub("Population_", "", df_popvar$Population)))
df_metvar$Parameter <- sprintf("Method_%02d", as.numeric(gsub("Method_", "", df_metvar$Method)))
df_allvar <- rbind(df_popvar[, c("Parameter", "TRUEvariance", "Category")], df_metvar[, c("Parameter", "TRUEvariance", "Category")])

# transform data to wide format matrix
test <- df[-c(2:4)]
test <- panel_data(test, id = popmethod, wave = Time)
testW <- widen_panel(test, separator = "_")
testW <- testW[, order(colnames(testW))]
rows <- as.data.frame(stringr::str_split_fixed(testW$popmethod, "_", 2))
colnames(rows) <- c("population", "method")
testW <- testW[-c((T_steps + 1))]
test_data <- as.matrix(testW)

## model build
# set controls
con.list <- list(maxit = 1000, allow.degen = TRUE, trace = 1)

# R
n <- nrow(test_data)
R.model <- matrix(list(0), n, n)
diag(R.model) <- paste0("r", rows$method)

# Z
pops <- c(unique(rows$population))
Z.model <- matrix(0, nrow=nrow(test_data), ncol=length(pops))
for(i in seq(length(pops))){
  Z.model[rows$population == pops[i], i] <- 1
}

# a
scale <- sample(1:M_methods, 1)
a.model <- matrix(list(0), n, 1)
for(i in 1:length(a.model)){
  if(rows$method[i] != scale){
    a.model[i] <- paste0("a", rows$method[i])
  }
}

# model list
mod.list <- list(
  B = "identity",
  U = "zero",
  Q = "diagonal and unequal",
  Z = Z.model,
  A = a.model,
  R = R.model,
  x0 = "equal",
  V0 = "zero",
  tinitx = 0
)

# run MARSS model
if(!file.exists(here::here("data", "clean", paste("ssmTEST_endogenous-P", P_populations, "M", M_methods, "T", T_steps, ".rds", sep="")))){
  ptm <- proc.time()
  ssm <- MARSS(test_data, model = mod.list, method = "kem", control = con.list)
  saveRDS(ssm, file=here::here("data", "clean", paste("ssmTEST_endogenous-P", P_populations, "M", M_methods, "T", T_steps, ".rds", sep="")))
  time <- proc.time()[3] - ptm
  time
}
ssm <- readRDS(file=here::here("data", "clean", paste("ssmTEST_endogenous-P", P_populations, "M", M_methods, "T", T_steps, ".rds", sep="")))

# need to pull R. and Q. estimates from ssm
fitted_matrices <- coef(ssm, type = "matrix")
fitted_Q <- fitted_matrices$Q
fitted_Q <- diag(fitted_Q)
fitted_R <- fitted_matrices$R
fitted_R <- diag(fitted_R)

# bootstrap vairance terms to create confidence intervals
if(!file.exists(here::here("data", "clean", paste("ssmBOOT_endogenous-P", P_populations, "M", M_methods, "T", T_steps, ".rds", sep="")))){
  ptm <- proc.time()
  boot <- MARSSboot(ssm, nboot=10000, output="parameters", sim = "parametric", param.gen = "hessian")
  saveRDS(boot, file=here::here("data", "clean", paste("ssmBOOT_endogenous-P", P_populations, "M", M_methods, "T", T_steps, ".rds", sep="")))
  time <- proc.time()[3] - ptm
  time
}
boot <- readRDS(file=here::here("data", "clean", paste("ssmBOOT_endogenous-P", P_populations, "M", M_methods, "T", T_steps, ".rds", sep="")))

# extract parameters
boot_params <- boot$boot.params
ci_matrix <- apply(boot_params, 1, quantile, probs = c(0.025, 0.975))
df_cis <- data.frame(
  Parameter = rownames(boot_params),
  Lower_CI  = ci_matrix[1, ],
  Upper_CI  = ci_matrix[2, ],
  row.names = NULL
)
df_cis <- df_cis %>%
  mutate(
    Parameter = if_else(
      str_detect(Parameter, "^R\\.r"),
      paste0("Method_", as.numeric(str_extract(Parameter, "(?<=r)[0-9]+"))),
      Parameter
    ),
    Parameter = if_else(
      str_detect(Parameter, "^Q\\."),
      paste0("Population_", as.numeric(str_extract(Parameter, "(?<=X)[0-9]+"))),
      Parameter
    )
  )

df_cis <- df_cis %>%
  mutate(
    # extract the prefix text (Method_ or Population_)
    Prefix = str_extract(Parameter, "^[A-Za-z_]+"),
    # extract the trailing number, convert to numeric, and pad with a leading zero if < 10
    Num_Padded = sprintf("%02d", as.numeric(str_extract(Parameter, "[0-9]+"))),
    # combine them back together into the Parameter column
    Parameter = paste0(Prefix, Num_Padded)
  ) %>%
  # drop the temporary helper columns
  select(-Prefix, -Num_Padded)

# merge in true variance
df_merged <- left_join(df_allvar, df_cis, by = "Parameter")

# logical column checking if variance is within the CI
df_merged <- df_merged %>%
  mutate(
    Within_CI = between(TRUEvariance, Lower_CI, Upper_CI)
  )

# overall success rate
capture_rate <- mean(df_merged$Within_CI, na.missing = TRUE) * 100
cat("Percentage of true variances within the CIs:", capture_rate, "%\n")
