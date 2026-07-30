library(future)
library(future.apply)
library(here)
library(MARSS)

here::i_am("code/development/robustTEST_multicore-pilot.R")
ssm <- readRDS(file=here::here("data", "clean", "ssmTEST_endogenous-P30M15T40.rds"))

ptm <- proc.time()

# plan parallel execution (use 7 of 8 cores)
plan(multisession, workers = 7)

# create list of seeds or boot configurations
boot_seeds <- 1:7

# run MARSSboot in parallel
# each worker gets a seed and computes a subset of bootstrap samples
boot_results <- future_lapply(boot_seeds, function(seed) {
  set.seed(seed)
  
  # run 10 boot iterations per core (10 * 7 = 70 total iterations)
  MARSSboot(
    boot <- MARSSboot(ssm, 
                      nboot=10, 
                      output="parameters",  # "all", "param", or "innovations"
                      sim = "parametric",   # or "nonparametric"
                      silent = TRUE
  ))
}, future.seed = TRUE)

# shut down workers when done
plan(sequential)

time <- proc.time()[3] - ptm
time