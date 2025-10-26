library(here)
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

# adjust column names
df_a <- melt(df_a)
df_a$method <- as.character(df_a$variable)
df_a$method <- substr(df_a$method, 4, nchar(df_a$method))

df_r <- melt(df_r)
df_r$method <- as.character(df_r$variable)
df_r$method <- substr(df_r$method, 4, nchar(df_r$method))

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

# box and whisker plots
china_bplot <- ggplot(data=df_a, aes(x = Name, y = value, fill=Group)) + 
  geom_boxplot() +
  labs(x = NULL,
       title='Chinook',
       y='Relative Bias') +
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
       title='Chinook',
       y='Variance') +
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

# modular code - coho
mod <- boot_cohoM10

# grab bootstrap parameter estimates for a & r
df <- mod$boot.params
df <- data.frame(t(df))
df <- df[, -c(18:36)]
df_a <- df[, -c(9:17)]
df_r <- df[, -c(1:8)]

# adjust column names
df_a <- melt(df_a)
df_a$method <- as.character(df_a$variable)
df_a$method <- substr(df_a$method, 4, nchar(df_a$method))

df_r <- melt(df_r)
df_r$method <- as.character(df_r$variable)
df_r$method <- substr(df_r$method, 4, nchar(df_r$method))

# merge in legend
df_a <- merge(df_a, legend, by = "method", all.x = TRUE, all.y = TRUE)
df_a <- na.omit(df_a)
df_a <- df_a[-c(1,2)]

df_r <- merge(df_r, legend, by = "method", all.x = TRUE, all.y = TRUE)
df_r <- na.omit(df_r)
df_r <- df_r[-c(1,2)]

# box and whisker plots
cohoa_bplot <- ggplot(data=df_a, aes(x = Name, y = value, fill=Group)) + 
  geom_boxplot() +
  labs(x = NULL,
       title='coho',
       y='Relative Bias') +
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
       title='coho',
       y='Variance') +
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

# modular code - steelhead
mod <- boot_stelM14

# grab bootstrap parameter estimates for a & r
df <- mod$boot.params
df <- data.frame(t(df))
df <- df[, -c(18:36)]
df_a <- df[, -c(9:17)]
df_r <- df[, -c(1:8)]

# adjust column names
df_a <- melt(df_a)
df_a$method <- as.character(df_a$variable)
df_a$method <- substr(df_a$method, 4, nchar(df_a$method))

df_r <- melt(df_r)
df_r$method <- as.character(df_r$variable)
df_r$method <- substr(df_r$method, 4, nchar(df_r$method))

# merge in legend
df_a <- merge(df_a, legend, by = "method", all.x = TRUE, all.y = TRUE)
df_a <- na.omit(df_a)
df_a <- df_a[-c(1,2)]

df_r <- merge(df_r, legend, by = "method", all.x = TRUE, all.y = TRUE)
df_r <- na.omit(df_r)
df_r <- df_r[-c(1,2)]

# box and whisker plots
stela_bplot <- ggplot(data=df_a, aes(x = Name, y = value, fill=Group)) + 
  geom_boxplot() +
  labs(x = NULL,
       title='steelhead',
       y='Relative Bias') +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("#c1a13c",
                                "#c772c5",
                                # "#5b3c90",
                                "#b85c37",
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
       title='steelhead',
       y='Variance') +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("#c1a13c",
                                "#c772c5",
                                # "#5b3c90",
                                "#b85c37",
                                # "#b94656",
                                # "#b0457b",
                                # "#729a43",
                                "#6d85db",
                                "#4dc48f"
                                )) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 345, hjust = 0, vjust = 0.9))
stelr_bplot