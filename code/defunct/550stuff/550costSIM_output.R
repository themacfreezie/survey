library(here)
library(latticeExtra)
library(readxl)
library(reshape2)
library(tidyverse)

# load model objects
mod <- readRDS(here("550", "data", "boot_chinM15.rds"))

# pull in and adjust legend
legend <- read_excel(here("550", "data", "method_key.xlsx"), col_names = TRUE)
legend$method <- legend$Method
legend <- legend[-c(1)]

# grab bootstrap parameter estimates for a & r
df <- mod$boot.params
df <- data.frame(t(df))
df <- df[, -c(18:36)]
df_a <- df[, -c(9:17)]
df_r <- df[, -c(1:8)]

# grab mean and sd
names_a <- colnames(df_a)
names_r <- colnames(df_r)

mean_a <- sapply(df_a, mean)
mean_r <- sapply(df_r, mean)

sd_a <- sapply(df_a, sd)
sd_r <- sapply(df_r, sd)

points_a <- data.frame(Method = names_a, mean_a = mean_a, sd_a = sd_a)
points_r <- data.frame(Method = names_r, mean_r = mean_r, sd_r = sd_r)

# adjust column names
df_a <- melt(df_a)
df_a$method <- as.character(df_a$variable)
df_a$method <- substr(df_a$method, 4, nchar(df_a$method))

df_r <- melt(df_r)
df_r$method <- as.character(df_r$variable)
df_r$method <- substr(df_r$method, 4, nchar(df_r$method))

points_a$method <- as.character(points_a$Method)
points_a$method <- substr(points_a$method, 4, nchar(points_a$method))

points_r$method <- as.character(points_r$Method)
points_r$method <- substr(points_r$method, 4, nchar(points_r$method))

points <- merge(points_a, points_r, by = "method", all.x = TRUE, all.y = TRUE)
points <- points[-c(2,5)]
points <- replace(points, is.na(points), 0)

# merge in legend
df_a <- merge(df_a, legend, by = "method", all.x = TRUE, all.y = TRUE)
df_a <- na.omit(df_a)
df_a <- df_a[-c(1,2)]

df_r <- merge(df_r, legend, by = "method", all.x = TRUE, all.y = TRUE)
df_r <- na.omit(df_r)
df_r <- df_r[-c(1,2)]

points <- merge(points, legend, by = "method", all.x = TRUE, all.y = TRUE)
points <- na.omit(points)

# plots
china_bplot <- ggplot(data=df_a, aes(x = Name, y = value, fill=Group)) + 
  geom_boxplot() +
  labs(x = NULL,
       title='Chinook Bias Estimates',
       subtitle='Bias relative to mark-recapture estimate at weir',
       y=NULL) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("#c1a13c",
                               # "#c772c5",
                               "#5b3c90",
                               # "#b85c37",
                               "#b94656",
                               # "#b0457b",
                               "#729a43",
                               "#6d85db",
                               "#4dc48f"
  )) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 350, hjust = 0, vjust = 0.9))
china_bplot

chinr_bplot <- ggplot(data=df_r, aes(x = Name, y = value, fill=Group)) + 
  geom_boxplot() +
  labs(x = NULL,
       title='Chinook Variance Estimates',
       y=NULL) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("#c1a13c",
                               # "#c772c5",
                               "#5b3c90",
                               # "#b85c37",
                               "#b94656",
                               # "#b0457b",
                               "#729a43",
                               "#6d85db",
                               "#4dc48f"
  )) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 345, hjust = 0, vjust = 0.9))
chinr_bplot

chin_splot <- ggplot(data=points, aes(x = mean_r, y = mean_a, color = Group)) +
  geom_point() +
  labs(y = 'Relative Bias (mark-recapture estimate at weir)',
       title='Chinook',
       x = 'Variance') +
  scale_color_manual(values = c("#c1a13c",
                                # "#c772c5",
                                "#5b3c90",
                                # "#b85c37",
                                "#b94656",
                                # "#b0457b",
                                "#729a43",
                                "#6d85db",
                                "#4dc48f"
  )) +
  geom_errorbarh(data=points, 
                 aes(xmin=ifelse(mean_r - 1.96*sd_r < 0, 0, mean_r - 1.96*sd_r), 
                     xmax=(mean_r + 1.96*sd_r), 
                     y = mean_a), 
                 linewidth = 1) +
  geom_errorbar(data=points, 
                aes(ymin=(mean_a - 1.96*sd_a), 
                    ymax=(mean_a + 1.96*sd_a), 
                    x = mean_r),
                width = 0.01, 
                linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_classic()
chin_splot

points$cost <- sample(1:100, size = nrow(points), replace = TRUE)


chin_splot <- ggplot(data=points, aes(x = mean_r, y = mean_a, color = Group, size = cost)) +
  geom_point() +
  labs(y = 'Relative Bias (mark-recapture estimate at weir)',
       title='Chinook',
       x = 'Variance') +
  scale_color_manual(values = c("#c1a13c",
                                # "#c772c5",
                                "#5b3c90",
                                # "#b85c37",
                                "#b94656",
                                # "#b0457b",
                                "#729a43",
                                "#6d85db",
                                "#4dc48f"
  )) +
  geom_errorbarh(data=points, 
                 aes(xmin=ifelse(mean_r - 1.96*sd_r < 0, 0, mean_r - 1.96*sd_r), 
                     xmax=(mean_r + 1.96*sd_r), 
                     y = mean_a), 
                 linewidth = 1) +
  geom_errorbar(data=points, 
                aes(ymin=(mean_a - 1.96*sd_a), 
                    ymax=(mean_a + 1.96*sd_a), 
                    x = mean_r),
                width = 0.01, 
                linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_classic()
chin_splot