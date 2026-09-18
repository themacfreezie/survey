## SET WORKING DIR & PACKAGES
library(here)
library(readxl)
library(tidyverse)

here::i_am("code/development/07.1-methodpop_boxplots.R")
options(max.print=2000)

# pull in data
load(here("data", "clean", "nosa_codes.Rda"))
nosa <- merge

legend <- read_excel(here("data", "clean", "method_key.xlsx"), col_names = TRUE)
legend$MethodNameID <- legend$Method
legend <- legend[-c(1)]

# natural log of counts
nosa$lnnosa <- log(nosa$NOSA + 1)

# different species
nosa_chin <- nosa %>% filter(CommonName=="Chinook Salmon")
nosa_coho <- nosa %>% filter(CommonName=="Coho Salmon")
nosa_stel <- nosa %>% filter(CommonName=="Steelhead")

# still issues with pop 11 somehow (chin)
nosa_chin <- nosa_chin %>%
  filter(TimeSeriesID != 599005)

# how often are particular survey methods used
counts_chin <- table(nosa_chin$MethodNameID)
counts_chin
  # 2 methods < 10 obs
counts_coho <- table(nosa_coho$MethodNameID)
counts_coho
  # 2 methods < 10 obs
counts_stel <- table(nosa_stel$MethodNameID)
counts_stel
  # 6 methods < 10 obs

# will drop those methods for which fewer than 10 observations exist
low_count_ids <- names(counts_chin[counts_chin < 10])
low_counts_chin <- as.numeric(low_count_ids)
nosa_chin <- nosa_chin %>%
  filter(!MethodNameID %in% low_counts_chin)
table(nosa_chin$MethodNameID)
nosa_chin <- nosa_chin[-c(1,2, 5:7, 9)]
nosa_chin <- merge(nosa_chin, legend, by = "MethodNameID", all.x = TRUE, all.y = TRUE)
nosa_chin <- na.omit(nosa_chin)
nosa_chin <- nosa_chin[-c(5)]

low_count_ids <- names(counts_coho[counts_coho < 10])
low_counts_coho <- as.numeric(low_count_ids)
nosa_coho <- nosa_coho %>%
  filter(!MethodNameID %in% low_counts_coho)
table(nosa_coho$MethodNameID)
nosa_coho <- nosa_coho[-c(1,2, 5:7, 9)]
nosa_coho <- merge(nosa_coho, legend, by = "MethodNameID", all.x = TRUE, all.y = TRUE)
nosa_coho <- na.omit(nosa_coho)
nosa_coho <- nosa_coho[-c(5)]

low_count_ids <- names(counts_stel[counts_stel < 10])
low_counts_stel <- as.numeric(low_count_ids)
nosa_stel <- nosa_stel %>%
  filter(!MethodNameID %in% low_counts_stel)
table(nosa_stel$MethodNameID)
nosa_stel <- nosa_stel[-c(1,2, 5:7, 9)]
nosa_stel <- merge(nosa_stel, legend, by = "MethodNameID", all.x = TRUE, all.y = TRUE)
nosa_stel <- na.omit(nosa_stel)
nosa_stel <- nosa_stel[-c(5)]

## plots
# chinook
chin_bplot2 <- ggplot(data = nosa_chin, aes(x = MethodName, y = lnnosa, fill = Group)) +
  geom_boxplot() +
  geom_hline(aes(yintercept = mean(lnnosa, na.rm = TRUE)), 
             color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Chinook",
    x = "",
    y = "ln(NOSA)"
  ) +
  scale_fill_manual(values = c("#c1a13c", # dam counts
                               # "#c772c5",
                               "#5b3c90", # mixed methods
                               # "#b85c37",
                               "#b94656", # peak spawner count
                               # "#b0457b",
                               "#729a43", # AUC Population
                               "#6d85db", # Redd counts
                               "#4dc48f" # Weir counts
  )) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 28),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    axis.title.y= element_text(size = 20),
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y= element_text(size = 18, color = "black"),
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  theme(axis.text.x = element_text(angle = 345, hjust = 0, vjust = 0.9))

chin_bplot3 <- ggplot(data = nosa_chin, aes(x = MethodName, y = lnnosa, fill = Group)) +
  geom_boxplot() +
  geom_hline(aes(yintercept = quantile(lnnosa, probs = 1/3, na.rm = TRUE)), 
             color = "red", linetype = "dashed", size = 1) +
  geom_hline(aes(yintercept = quantile(lnnosa, probs = 2/3, na.rm = TRUE)), 
             color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Chinook",
    x = "",
    y = "ln(NOSA)"
  ) +
  scale_fill_manual(values = c("#c1a13c", # dam counts
                               # "#c772c5",
                               "#5b3c90", # mixed methods
                               # "#b85c37",
                               "#b94656", # peak spawner count
                               # "#b0457b",
                               "#729a43", # AUC Population
                               "#6d85db", # Redd counts
                               "#4dc48f" # Weir counts
  )) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 28),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    axis.title.y= element_text(size = 20),
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y= element_text(size = 18, color = "black"),
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  theme(axis.text.x = element_text(angle = 345, hjust = 0, vjust = 0.9))

chin_vplot2 <- ggplot(data = nosa_chin, aes(x = MethodName, y = lnnosa, fill = Group)) +
  geom_violin(scale = "count", alpha = 0.8) +
  geom_boxplot(width = 0.1, color = "black", linetype = "dashed", outlier.shape = NA, alpha = 0.2) +
  geom_hline(aes(yintercept = mean(lnnosa, na.rm = TRUE)), 
             color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Chinook",
    x = "",
    y = "ln(NOSA)"
  ) +
  scale_fill_manual(values = c("#c1a13c", # dam counts
                               # "#c772c5",
                               "#5b3c90", # mixed methods
                               # "#b85c37",
                               "#b94656", # peak spawner count
                               # "#b0457b",
                               "#729a43", # AUC Population
                               "#6d85db", # Redd counts
                               "#4dc48f" # Weir counts
  )) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 28),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    axis.title.y= element_text(size = 20),
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y= element_text(size = 18, color = "black"),
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  theme(axis.text.x = element_text(angle = 345, hjust = 0, vjust = 0.9))

chin_vplot3 <- ggplot(data = nosa_chin, aes(x = MethodName, y = lnnosa, fill = Group)) +
  geom_violin(scale = "count", alpha = 0.8) +
  geom_boxplot(width = 0.1, color = "black", linetype = "dashed", outlier.shape = NA, alpha = 0.2) +
  geom_hline(aes(yintercept = quantile(lnnosa, probs = 1/3, na.rm = TRUE)), 
             color = "red", linetype = "dashed", size = 1) +
  geom_hline(aes(yintercept = quantile(lnnosa, probs = 2/3, na.rm = TRUE)), 
             color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Chinook",
    x = "",
    y = "ln(NOSA)"
  ) +
  scale_fill_manual(values = c("#c1a13c", # dam counts
                               # "#c772c5",
                               "#5b3c90", # mixed methods
                               # "#b85c37",
                               "#b94656", # peak spawner count
                               # "#b0457b",
                               "#729a43", # AUC Population
                               "#6d85db", # Redd counts
                               "#4dc48f" # Weir counts
  )) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 28),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    axis.title.y= element_text(size = 20),
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y= element_text(size = 18, color = "black"),
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  theme(axis.text.x = element_text(angle = 345, hjust = 0, vjust = 0.9))

chin_vplotORDERS <- ggplot(data = nosa_chin, aes(x = MethodName, y = lnnosa, fill = Group)) +
  geom_violin(scale = "count", alpha = 0.8) +
  geom_boxplot(width = 0.1, color = "black", linetype = "dashed", outlier.shape = NA, alpha = 0.2) +
  geom_hline(yintercept = log(100), 
             color = "red", linetype = "dashed", size = 1) +
  geom_hline(yintercept = log(1000), 
             color = "red", linetype = "dashed", size = 1) +
  geom_text(aes(x = 0.5, y = log(100), label = "100"), 
            color = "red", size = 5, vjust = 1.2, hjust = 0, inherit.aes = FALSE) +
  geom_text(aes(x = 0.5, y = log(1000), label = "1000"), 
            color = "red", size = 5, vjust = 1.2, hjust = 0, inherit.aes = FALSE) +
  labs(
    title = "Chinook",
    x = "",
    y = "ln(NOSA)"
  ) +
  scale_fill_manual(values = c("#c1a13c", # dam counts
                               # "#c772c5",
                               "#5b3c90", # mixed methods
                               # "#b85c37",
                               "#b94656", # peak spawner count
                               # "#b0457b",
                               "#729a43", # AUC Population
                               "#6d85db", # Redd counts
                               "#4dc48f" # Weir counts
  )) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 28),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    axis.title.y= element_text(size = 20),
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y= element_text(size = 18, color = "black"),
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  theme(axis.text.x = element_text(angle = 345, hjust = 0, vjust = 0.9))

# coho
coho_bplot2 <- ggplot(data = nosa_coho, aes(x = MethodName, y = lnnosa, fill = Group)) +
  geom_boxplot() +
  geom_hline(aes(yintercept = mean(lnnosa, na.rm = TRUE)), 
             color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Coho",
    x = "",
    y = "ln(NOSA)"
  ) +
  scale_fill_manual(values = c("#c1a13c", # dam counts
                               "#c772c5", # AUC monitoring
                               # "#5b3c90", # mixed methods
                               # "#b85c37",
                               "#b94656", # peak spawner count
                               # "#b0457b",
                               "#729a43" # AUC Population
                               # "#6d85db", # Redd counts
                               # "#4dc48f" # Weir counts
  )) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 28),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    axis.title.y= element_text(size = 20),
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y= element_text(size = 18, color = "black"),
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  theme(axis.text.x = element_text(angle = 345, hjust = 0, vjust = 0.9))

coho_bplot3 <- ggplot(data = nosa_coho, aes(x = MethodName, y = lnnosa, fill = Group)) +
  geom_boxplot() +
  geom_hline(aes(yintercept = quantile(lnnosa, probs = 1/3, na.rm = TRUE)), 
             color = "red", linetype = "dashed", size = 1) +
  geom_hline(aes(yintercept = quantile(lnnosa, probs = 2/3, na.rm = TRUE)), 
             color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Coho",
    x = "",
    y = "ln(NOSA)"
  ) +
  scale_fill_manual(values = c("#c1a13c", # dam counts
                               "#c772c5", # AUC monitoring
                               # "#5b3c90", # mixed methods
                               # "#b85c37",
                               "#b94656", # peak spawner count
                               # "#b0457b",
                               "#729a43" # AUC Population
                               # "#6d85db", # Redd counts
                               # "#4dc48f" # Weir counts
  )) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 28),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    axis.title.y= element_text(size = 20),
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y= element_text(size = 18, color = "black"),
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  theme(axis.text.x = element_text(angle = 345, hjust = 0, vjust = 0.9))

coho_vplot2 <- ggplot(data = nosa_coho, aes(x = MethodName, y = lnnosa, fill = Group)) +
  geom_violin(scale = "count", alpha = 0.8) +
  geom_boxplot(width = 0.1, color = "black", linetype = "dashed", outlier.shape = NA, alpha = 0.2) +
  geom_hline(aes(yintercept = mean(lnnosa, na.rm = TRUE)), 
             color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Coho",
    x = "",
    y = "ln(NOSA)"
  ) +
  scale_fill_manual(values = c("#c1a13c", # dam counts
                               "#c772c5", # AUC monitoring
                               # "#5b3c90", # mixed methods
                               # "#b85c37",
                               "#b94656", # peak spawner count
                               # "#b0457b",
                               "#729a43" # AUC Population
                               # "#6d85db", # Redd counts
                               # "#4dc48f" # Weir counts
  )) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 28),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    axis.title.y= element_text(size = 20),
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y= element_text(size = 18, color = "black"),
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  theme(axis.text.x = element_text(angle = 345, hjust = 0, vjust = 0.9))

coho_vplot3 <- ggplot(data = nosa_coho, aes(x = MethodName, y = lnnosa, fill = Group)) +
  geom_violin(scale = "count", alpha = 0.8) +
  geom_boxplot(width = 0.1, color = "black", linetype = "dashed", outlier.shape = NA, alpha = 0.2) +
  geom_hline(aes(yintercept = quantile(lnnosa, probs = 1/3, na.rm = TRUE)), 
             color = "red", linetype = "dashed", size = 1) +
  geom_hline(aes(yintercept = quantile(lnnosa, probs = 2/3, na.rm = TRUE)), 
             color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Coho",
    x = "",
    y = "ln(NOSA)"
  ) +
  scale_fill_manual(values = c("#c1a13c", # dam counts
                               "#c772c5", # AUC monitoring
                               # "#5b3c90", # mixed methods
                               # "#b85c37",
                               "#b94656", # peak spawner count
                               # "#b0457b",
                               "#729a43" # AUC Population
                               # "#6d85db", # Redd counts
                               # "#4dc48f" # Weir counts
  )) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 28),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    axis.title.y= element_text(size = 20),
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y= element_text(size = 18, color = "black"),
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  theme(axis.text.x = element_text(angle = 345, hjust = 0, vjust = 0.9)) 

coho_vplotORDERS <- ggplot(data = nosa_coho, aes(x = MethodName, y = lnnosa, fill = Group)) +
  geom_violin(scale = "count", alpha = 0.8) +
  geom_boxplot(width = 0.1, color = "black", linetype = "dashed", outlier.shape = NA, alpha = 0.2) +
  geom_hline(yintercept = log(100), 
             color = "red", linetype = "dashed", size = 1) +
  geom_hline(yintercept = log(1000), 
             color = "red", linetype = "dashed", size = 1) +
  geom_hline(yintercept = log(10000), 
             color = "red", linetype = "dashed", size = 1) +
  geom_text(aes(x = 0.5, y = log(100), label = "100"), 
            color = "red", size = 5, vjust = 1.2, hjust = 0, inherit.aes = FALSE) +
  geom_text(aes(x = 0.5, y = log(1000), label = "1000"), 
            color = "red", size = 5, vjust = 1.2, hjust = 0, inherit.aes = FALSE) +
  geom_text(aes(x = 0.5, y = log(10000), label = "10000"), 
            color = "red", size = 5, vjust = 1.2, hjust = 0, inherit.aes = FALSE) +
  labs(
    title = "Coho",
    x = "",
    y = "ln(NOSA)"
  ) +
  scale_fill_manual(values = c("#c1a13c", # dam counts
                               "#c772c5", # AUC monitoring
                               # "#5b3c90", # mixed methods
                               # "#b85c37",
                               "#b94656", # peak spawner count
                               # "#b0457b",
                               "#729a43" # AUC Population
                               # "#6d85db", # Redd counts
                               # "#4dc48f" # Weir counts
  )) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 28),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    axis.title.y= element_text(size = 20),
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y= element_text(size = 18, color = "black"),
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  theme(axis.text.x = element_text(angle = 345, hjust = 0, vjust = 0.9)) 

# steelhead
stel_bplot2 <- ggplot(data = nosa_stel, aes(x = MethodName, y = lnnosa, fill = Group)) +
  geom_boxplot() +
  geom_hline(aes(yintercept = mean(lnnosa, na.rm = TRUE)), 
             color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Steelhead",
    x = "",
    y = "ln(NOSA)"
  ) +
  scale_fill_manual(values = c("#c1a13c", # dam counts
                               # "#c772c5", # AUC monitoring
                               # "#5b3c90", # mixed methods
                               # "#b85c37",
                               # "#b94656", # peak spawner count
                               # "#b0457b",
                               # "#729a43", # AUC Population
                               "#6d85db", # Redd counts
                               "#4dc48f" # Weir counts
  )) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 28),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    axis.title.y= element_text(size = 20),
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y= element_text(size = 18, color = "black"),
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  theme(axis.text.x = element_text(angle = 345, hjust = 0, vjust = 0.9)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30))

stel_bplot3 <- ggplot(data = nosa_stel, aes(x = MethodName, y = lnnosa, fill = Group)) +
  geom_boxplot() +
  geom_hline(aes(yintercept = quantile(lnnosa, probs = 1/3, na.rm = TRUE)), 
             color = "red", linetype = "dashed", size = 1) +
  geom_hline(aes(yintercept = quantile(lnnosa, probs = 2/3, na.rm = TRUE)), 
             color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Steelhead",
    x = "",
    y = "ln(NOSA)"
  ) +
  scale_fill_manual(values = c("#c1a13c", # dam counts
                               # "#c772c5", # AUC monitoring
                               # "#5b3c90", # mixed methods
                               # "#b85c37",
                               # "#b94656", # peak spawner count
                               # "#b0457b",
                               # "#729a43", # AUC Population
                               "#6d85db", # Redd counts
                               "#4dc48f" # Weir counts
  )) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 28),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    axis.title.y= element_text(size = 20),
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y= element_text(size = 18, color = "black"),
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  theme(axis.text.x = element_text(angle = 345, hjust = 0, vjust = 0.9)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30))

stel_vplot2 <- ggplot(data = nosa_stel, aes(x = MethodName, y = lnnosa, fill = Group)) +
  geom_violin(scale = "count", alpha = 0.8) +
  geom_boxplot(width = 0.1, color = "black", linetype = "dashed", outlier.shape = NA, alpha = 0.2) +
  geom_hline(aes(yintercept = mean(lnnosa, na.rm = TRUE)), 
             color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Steelhead",
    x = "",
    y = "ln(NOSA)"
  ) +
  scale_fill_manual(values = c("#c1a13c", # dam counts
                               # "#c772c5", # AUC monitoring
                               # "#5b3c90", # mixed methods
                               # "#b85c37",
                               # "#b94656", # peak spawner count
                               # "#b0457b",
                               # "#729a43", # AUC Population
                               "#6d85db", # Redd counts
                               "#4dc48f" # Weir counts
  )) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 28),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    axis.title.y= element_text(size = 20),
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y= element_text(size = 18, color = "black"),
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  theme(axis.text.x = element_text(angle = 345, hjust = 0, vjust = 0.9)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30))

stel_vplot3 <- ggplot(data = nosa_stel, aes(x = MethodName, y = lnnosa, fill = Group)) +
  geom_violin(scale = "count", alpha = 0.8) +
  geom_boxplot(width = 0.1, color = "black", linetype = "dashed", outlier.shape = NA, alpha = 0.2) +
  geom_hline(aes(yintercept = quantile(lnnosa, probs = 1/3, na.rm = TRUE)), 
             color = "red", linetype = "dashed", size = 1) +
  geom_hline(aes(yintercept = quantile(lnnosa, probs = 2/3, na.rm = TRUE)), 
             color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Steelhead",
    x = "",
    y = "ln(NOSA)"
  ) +
  scale_fill_manual(values = c("#c1a13c", # dam counts
                               # "#c772c5", # AUC monitoring
                               # "#5b3c90", # mixed methods
                               # "#b85c37",
                               # "#b94656", # peak spawner count
                               # "#b0457b",
                               # "#729a43", # AUC Population
                               "#6d85db", # Redd counts
                               "#4dc48f" # Weir counts
  )) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 28),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    axis.title.y= element_text(size = 20),
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y= element_text(size = 18, color = "black"),
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  theme(axis.text.x = element_text(angle = 345, hjust = 0, vjust = 0.9)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30))

stel_vplotORDERS <- ggplot(data = nosa_stel, aes(x = MethodName, y = lnnosa, fill = Group)) +
  geom_violin(scale = "count", alpha = 0.8) +
  geom_boxplot(width = 0.1, color = "black", linetype = "dashed", outlier.shape = NA, alpha = 0.2) +
  geom_hline(yintercept = log(100), 
             color = "red", linetype = "dashed", size = 1) +
  geom_hline(yintercept = log(1000), 
             color = "red", linetype = "dashed", size = 1) +
  geom_text(aes(x = 0.5, y = log(100), label = "100"), 
            color = "red", size = 5, vjust = 1.2, hjust = 0, inherit.aes = FALSE) +
  geom_text(aes(x = 0.5, y = log(1000), label = "1000"), 
            color = "red", size = 5, vjust = 1.2, hjust = 0, inherit.aes = FALSE) +
  labs(
    title = "Steelhead",
    x = "",
    y = "ln(NOSA)"
  ) +
  scale_fill_manual(values = c("#c1a13c", # dam counts
                               # "#c772c5", # AUC monitoring
                               # "#5b3c90", # mixed methods
                               # "#b85c37",
                               # "#b94656", # peak spawner count
                               # "#b0457b",
                               # "#729a43", # AUC Population
                               "#6d85db", # Redd counts
                               "#4dc48f" # Weir counts
  )) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 28),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    axis.title.y= element_text(size = 20),
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y= element_text(size = 18, color = "black"),
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  theme(axis.text.x = element_text(angle = 345, hjust = 0, vjust = 0.9)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30))

chin_vplot3 
coho_vplot3 
stel_vplot3 

chin_vplotORDERS
coho_vplotORDERS
stel_vplotORDERS
