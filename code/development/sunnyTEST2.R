## SET WORKING DIR & PACKAGES
library(dlm)
library(here)
library(MARSS)
library(MASS)
library(panelr)
library(tidyverse)

here::i_am("code/development/sunnyTEST2.R")
options(max.print=2000)

## creating synthetic time series
# set seed for reproducibility
set.seed(694201)

# set params
P_populations <- 75
M_methods <- 15
T_steps <- 40

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

# simulate states (x_t,p)
x <- matrix(0, nrow = T_steps, ncol = P_populations)
x[1, ] <- runif(P_populations, min = 2, max = 10) 
  # initial state 

for (t in 2:T_steps) {
  v_t <- mvrnorm(1, mu = rep(0, P_populations), Sigma = Q)
  proposed_x <- x[t - 1, ] + v_t
  
   x[t, ] <- pmin(pmax(proposed_x, 0.6931472), 10.927161)
    # upper and lower bounds based on nosa data
}

# generate sqequential observations (1 method per pop per time step)

# pre-generate the entire noise matrix for all time steps and methods
# rows = T_steps, cols = M_methods
w_matrix <- mvrnorm(T_steps, mu = rep(0, M_methods), Sigma = R)

# initialize tracking matrices for the chosen methods
# rows = T_steps, cols = P_populations
chosen_method_idx <- matrix(NA, nrow = T_steps, ncol = P_populations)

# time step 1: randomly pick an initial method (1 to 15) for each population
chosen_method_idx[1, ] <- sample(1:M_methods, P_populations, replace = TRUE)

# time steps 2 to T: Apply the 15% switching probability
switch_prob <- 0.15

for (t in 2:T_steps) {
  # determine which populations will switch methods at this time step
  will_switch <- runif(P_populations) < switch_prob
  
  # for those staying the same, carry over the previous method index
  chosen_method_idx[t, ] <- chosen_method_idx[t - 1, ]
  
  if (any(will_switch)) {
    # count how many populations need a new method
    num_switchers <- sum(will_switch)
    
    # randomly select a NEW method out of the remaining 14 options for each switcher
    chosen_method_idx[t, will_switch] <- replicate(num_switchers, {
      prev_method <- chosen_method_idx[t - 1, will_switch]
      # sample from all methods EXCEPT the previous one
      sample(setdiff(1:M_methods, prev_method), 1) 
    })
  }
}

# 3. build the final 3,000-row dataframe using fast vector indexing
df_list <- list()

for (p in 1:P_populations) {
  # extract the sequence of chosen methods for population 'p' across all time
  p_methods <- chosen_method_idx[, p]
  
  # extract the true state sequence for population 'p'
  p_states <- x[, p]
  
  # extract the corresponding noise values based on the chosen method matrix coordinates
  # this matches the correct row (Time) and column (Method) from w_matrix
  p_noise <- w_matrix[matrix(c(1:T_steps, p_methods), ncol = 2)]
  
  # create a compact dataframe for this population
  df_list[[p]] <- data.frame(
    Time = 1:T_steps,
    Population = paste0("Population_", p),
    Method = paste0("Method_", p_methods),
    True_State = p_states,
    Observation = p_states + p_noise
  )
}

# combine the list of 75 population chunks into one dataframe
df <- do.call(rbind, df_list)

# grab variance terms
df_popvar <- data.frame(
  Population = paste0("Population_", 1:P_populations),
  Process_Variance = diag(Q)
)

df_metvar <- data.frame(
  Method = paste0("Method_", 1:M_methods),
  Observation_Variance = diag(R)
)

# drop strings in pop and methods
df$Population <- as.numeric(gsub("Population_", "", df$Population))
df$Method <- as.numeric(gsub("Method_", "", df$Method))

# add zeroes to populations and methods
df$Population <- sprintf("%02d", df$Population)
df$Method <- sprintf("%02d", df$Method)

# integer version for time as well
df$Time <- as.numeric(sprintf("%02d", df$Time))
df$Time <- df$Time + 1979

# build popmethod varaible
df$popmethod <- paste0(df$Population, "_", df$Method)

# info table
## vital information
methodsTable <- df %>% 
  pivot_wider(names_from = "Time", values_from = "Method", id_cols = "Population")
print(methodsTable)

# set data wide (rows = popid/method, columns = year) and create matrix
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

# model list
mod.list <- list(
  B = "identity",
  U = "zero",
  Q = "diagonal and unequal",
  Z = Z.model,
  A = "zero",
  R = R.model,
  x0 = "equal",
  V0 = "zero",
  tinitx = 0
)

# run MARSS model
if(!file.exists(here::here("data", "clean", "ssm_test.rds"))){
  ptm <- proc.time()
  ssm <- MARSS(test_data, model = mod.list, method = "kem", control = con.list)
  saveRDS(ssm, file=here::here("data", "clean", "ssm_test.rds"))
  time <- proc.time()[3] - ptm
  time
}
# load in ssm
ssm <- readRDS(file=here::here("data", "clean", "ssm_test.rds"))

# need to pull R. and Q. estimates from ssm
fitted_matrices <- coef(ssm, type = "matrix")
fitted_Q <- fitted_matrices$Q
fitted_Q <- diag(fitted_Q)
fitted_R <- fitted_matrices$R
fitted_R <- diag(fitted_R)

# bootstrap vairance terms to create confidence intervals
boot <- MARSSboot(ssm, nboot=10000, output="parameters", sim = "parametric", param.gen = "hessian")
saveRDS(boot, file=here::here("data", "clean", "ssmBOOT_test.rds"))

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

