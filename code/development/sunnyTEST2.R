## SET WORKING DIR & PACKAGES
library(dlm)
library(here)
library(MARSS)
library(MASS)
library(panelr)

here::i_am("code/development/sunnyTEST2.R")
options(max.print=2000)

## CREATING SYNTHETIC TIME SERIES
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

# generate Observations (y_t,m,p)
dataset_list <- list()
counter <- 1

for (t in 1:T_steps) {
  w_t <- mvrnorm(1, mu = rep(0, M_methods), Sigma = R)
  
  for (p in 1:P_populations) {
    for (m in 1:M_methods) {
      y_tmp <- x[t, p] + w_t[m]
      
      dataset_list[[counter]] <- data.frame(
        Time = t,
        Population = paste0("Population_", p),
        Method = paste0("Method_", m),
        True_State = x[t, p],
        Observation = y_tmp
      )
      counter <- counter + 1
    }
  }
}

df <- do.call(rbind, dataset_list)