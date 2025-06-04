library(tidyverse)

# load model objects
ssm_chin <- readRDS(file=here::here("550", "data", "ssm_chin.rds"))
ssm_coho <- readRDS(file=here::here("550", "data", "ssm_coho.rds"))
ssm_stel <- readRDS(file=here::here("550", "data", "ssm_stel.rds"))

# modular code
mod <- ssm_stel

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


