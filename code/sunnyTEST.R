library(dlm)

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
t <- 50 # must be even

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

df <- data.frame(t(obs_df))
colnames(df) <- c("P1", "P2")
df <- stack(df, select = c("P1", "P2"))
df$method <- c(rep("M1", 25), 
               rep("M3", 15), 
               rep("M4", 10), 
               rep("M2", 25), 
               rep("M4", 15), 
               rep("M3", 10))


            