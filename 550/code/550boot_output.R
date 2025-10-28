library(gridExtra)
library(here)
library(readxl)
library(reshape2)
library(tidyverse)

# load model objects
boot_chinM15 <- readRDS(here("550", "data", "boot_chinM15.rds"))
boot_cohoM10 <- readRDS(here("550", "data", "boot_cohoM10.rds"))
boot_stelM14 <- readRDS(here("550", "data", "boot_stelM14.rds"))

# pull in and adjust legend
legend <- read_excel(here("550", "data", "method_key.xlsx"), col_names = TRUE)
legend$method <- legend$Method
legend <- legend[-c(1)]

# modular code - Chinook
mod <- boot_chinM15

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

# # match colnames
# matched_indices <- match(colnames(df_a), legend$method_a)
# colnames(df_a) <- legend$Name[matched_indices]
# 
# matched_indices <- match(colnames(df_r), legend$method_r)
# colnames(df_r) <- legend$Name[matched_indices]
# # no longer necessary but nice trick

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

# modular code - coho
mod <- boot_cohoM10

# grab bootstrap parameter estimates for a & r
df <- mod$boot.params
df <- data.frame(t(df))
df <- df[, -c(22:49)]
df_a <- df[, -c(11:21)]
df_r <- df[, -c(1:10)]

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
cohoa_bplot <- ggplot(data=df_a, aes(x = Name, y = value, fill=Group)) + 
  geom_boxplot() +
  labs(x = NULL,
       title='Coho Bias Estimates',
       subtitle='Bias relative to dam counts - video',
       y = NULL) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("#c1a13c",
                                "#c772c5",
                                # "#5b3c90",
                                "#b85c37",
                                "#b94656",
                                # "#b0457b",
                                "#729a43"
                                # "#6d85db",
                                # "#4dc48f"
                                )) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 350, hjust = 0, vjust = 0.9))
cohoa_bplot

cohor_bplot <- ggplot(data=df_r, aes(x = Name, y = value, fill=Group)) + 
  geom_boxplot() +
  labs(x = NULL,
       title='Coho Variance estimates',
       y=NULL) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("#c1a13c",
                                "#c772c5",
                                # "#5b3c90",
                                "#b85c37",
                                "#b94656",
                                # "#b0457b",
                                "#729a43"
                                # "#6d85db",
                                # "#4dc48f"
                                )) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 345, hjust = 0, vjust = 0.9))
cohor_bplot

coho_splot <- ggplot(data=points, aes(x = mean_r, y = mean_a, color = Group)) +
  geom_point() +
  labs(y = 'Relative Bias (dam counts - video)',
       title='Coho',
       x = 'Variance') +
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
coho_splot

# modular code - steelhead
mod <- boot_stelM14

# grab bootstrap parameter estimates for a & r
df <- mod$boot.params
df <- data.frame(t(df))
df <- df[, -c(18:41)]
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
stela_bplot <- ggplot(data=df_a, aes(x = Name, y = value, fill=Group)) + 
  geom_boxplot() +
  labs(x = NULL,
       title='Steelhead Bias Estimates',
       subtitle='Bias relative to in-river weir counts',
       y=NULL) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("#c1a13c",
                                "#c772c5",
                                # "#5b3c90",
                                # "#b85c37",
                                # "#b94656",
                                # "#b0457b",
                                # "#729a43",
                                "#6d85db",
                                "#4dc48f"
                                )) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 350, hjust = 0, vjust = 0.9))
stela_bplot

stelr_bplot <- ggplot(data=df_r, aes(x = Name, y = value, fill=Group)) + 
  geom_boxplot() +
  labs(x = NULL,
       title='Steelhead Variance Estimates',
       y=NULL) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("#c1a13c",
                                "#c772c5",
                                # "#5b3c90",
                                # "#b85c37",
                                # "#b94656",
                                # "#b0457b",
                                # "#729a43",
                                "#6d85db",
                                "#4dc48f"
                                )) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 345, hjust = 0, vjust = 0.9))
stelr_bplot

stel_splot <- ggplot(data=points, aes(x = mean_r, y = mean_a, color = Group)) +
  geom_point() +
  labs(y = 'Relative Bias (in-river weir counts)',
       title='Steelhead',
       x = 'Variance') +
  scale_color_manual(values = c("#c1a13c",
                                "#c772c5",
                                # "#5b3c90",
                                # "#b85c37",
                                # "#b94656",
                                # "#b0457b",
                                # "#729a43",
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
stel_splot

ggsave(here("550", "output", "figures", "china_bplot.png"), plot=china_bplot, device="png", dpi=300)
ggsave(here("550", "output", "figures", "cohoa_bplot.png"), plot=cohoa_bplot, device="png", dpi=300)
ggsave(here("550", "output", "figures", "stela_bplot.png"), plot=stela_bplot, device="png", dpi=300)

ggsave(here("550", "output", "figures", "chinr_bplot.png"), plot=chinr_bplot, device="png", dpi=300)
ggsave(here("550", "output", "figures", "cohor_bplot.png"), plot=cohor_bplot, device="png", dpi=300)
ggsave(here("550", "output", "figures", "stelr_bplot.png"), plot=stelr_bplot, device="png", dpi=300)

ggsave(here("550", "output", "figures", "chin_error.png"), plot=chin_splot, device="png", dpi=300)
ggsave(here("550", "output", "figures", "coho_error.png"), plot=coho_splot, device="png", dpi=300)
ggsave(here("550", "output", "figures", "stel_error.png"), plot=stel_splot, device="png", dpi=300)

# goalish
p1 <- china_bplot
p2 <- chinr_bplot
grid.arrange(p1, p2, ncol = 1)
