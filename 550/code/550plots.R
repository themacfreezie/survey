library(here)
library(readxl)
library(tidyverse)

# load model objects
ssm_chin <- readRDS(file=here::here("550", "data", "ssm_chinM15.rds"))
ssm_coho <- readRDS(file=here::here("550", "data", "ssm_cohoM10.rds"))
ssm_stel <- readRDS(file=here::here("550", "data", "ssm_stelM14.rds"))

# modular code
mod <- ssm_chin

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

# better plot
points <- ggplot(data=data, aes(x=R, y=a, color=Group)) + 
  geom_text(aes(label = method), size=8) +
  # geom_point(size=8) + 
  labs(x = "Variance",
       title='Chinook',
       y='Relative Bias') +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_color_manual(values = c("#c1a13c",
                               # "#c772c5",
                               "#5b3c90",
                               # "#b85c37",
                               "#b94656",
                               # "#b0457b",
                               "#729a43",
                               "#6d85db",
                               "#4dc48f")) +
  theme_classic()
chin_points <- points + guides(color=guide_legend(title="Method Group"))
chin_points
chin_data <- data
chin_data$species <- "Chinook"

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

# better plot
points <- ggplot(data=data, aes(x=R, y=a, color=Group)) + 
  geom_text(aes(label = method), size=8) +
  # geom_point(size=8) + 
  labs(x = "Variance",
       title='Steelhead',
       y='Relative Bias') +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_color_manual(values = c("#c1a13c",
                               "#c772c5",
                               # "#5b3c90",
                               # "#b85c37",
                               # "#b94656",
                               # "#b0457b",
                               # "#729a43",
                               "#6d85db",
                               "#4dc48f")) +
  theme_classic()
stel_points <- points + guides(color=guide_legend(title="Method Group"))
stel_points
stel_data <- data
stel_data$species <- "steelhead"

# modular code
mod <- ssm_coho

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

# bind all data into one set
ssm_chin <- readRDS(file=here::here("550", "data", "ssm_chinM11.rds"))
ssm_coho <- readRDS(file=here::here("550", "data", "ssm_cohoM11.rds"))
ssm_stel <- readRDS(file=here::here("550", "data", "ssm_stelM11.rds"))

# modular code
mod <- ssm_chin

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

# pull in legend
legend <- read_excel(here("550", "data", "method_key.xlsx"), col_names = TRUE)
legend$method <- legend$Method
legend <- legend[-c(1,2)]

data <- merge(data, legend, by = "method", all.x = TRUE, all.y = TRUE)
data <- na.omit(data)

data$species <- 'Chinook'
chin_data <- data

# modular code
mod <- ssm_coho

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

# pull in legend
legend <- read_excel(here("550", "data", "method_key.xlsx"), col_names = TRUE)
legend$method <- legend$Method
legend <- legend[-c(1,2)]

data <- merge(data, legend, by = "method", all.x = TRUE, all.y = TRUE)
data <- na.omit(data)

data$species <- "Coho"
coho_data <- data

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

# pull in legend
legend <- read_excel(here("550", "data", "method_key.xlsx"), col_names = TRUE)
legend$method <- legend$Method
legend <- legend[-c(1,2)]

data <- merge(data, legend, by = "method", all.x = TRUE, all.y = TRUE)
data <- na.omit(data)

data$species <- "Steelhead"
stel_data <- data

data <- dplyr::bind_rows(chin_data, coho_data, stel_data)

points <- ggplot(data=data, aes(x=R, y=a, color=Group, shape = species)) + 
  geom_text(aes(label = method), size=8) +
  # geom_point(data=data, size=8) +
  labs(x = "Variance",
       title='All species',
       y='Relative Bias') +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_color_manual(values = c("#c1a13c",
                                "#c772c5",
                                "#5b3c90",
                                "#b85c37",
                                "#b94656",
                                # "#b0457b",
                                "#729a43",
                                "#6d85db",
                                "#4dc48f"
  )) +
  theme_classic()
points <- points + guides(color=guide_legend(title="Method Group"))
points

ggsave(here("550", "output", "figures", "chin_points.png"), plot=chin_points, device="png", dpi=300)
ggsave(here("550", "output", "figures", "coho_points.png"), plot=coho_points, device="png", dpi=300)
ggsave(here("550", "output", "figures", "stel_points.png"), plot=stel_points, device="png", dpi=300)

