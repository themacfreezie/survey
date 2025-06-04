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

# pull in legend
legend <- read_excel(here("550", "data", "method_key.xlsx"), col_names = TRUE)
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

# histogram of variance bins