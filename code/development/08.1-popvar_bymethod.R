## SET WORKING DIR & PACKAGES
library(ggrepel)
library(here)
library(readxl)
library(reshape2)
library(tidyverse)

here::i_am("code/development/08.1-popvar_bymethod.R")
options(max.print=2000)

# pull in data
load(here("data", "clean", "nosa_codes.Rda"))
nosa <- merge

legend <- read_excel(here("data", "clean", "method_key.xlsx"), col_names = TRUE)
legend$MethodNameID <- legend$Method
legend <- legend[-c(1)]

boot_chinM9 <- readRDS(here("data", "clean", "ssmBOOT_chinM9.rds"))
boot_cohoM9 <- readRDS(here("data", "clean", "ssmBOOT_cohoM9.rds"))
boot_stelM9 <- readRDS(here("data", "clean", "ssmBOOT_stelM9.rds"))

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

# getting mean and sd of nosa
pointschin_nosa <- nosa_chin %>%
  group_by(MethodNameID, MethodName, CommonName, Group) %>%
  summarise(
    mean_lnnosa = mean(lnnosa, na.rm = TRUE),
    sd_lnnosa   = sd(lnnosa, na.rm = TRUE),
    .groups     = "drop"
  )

pointscoho_nosa <- nosa_coho %>%
  group_by(MethodNameID, MethodName, CommonName, Group) %>%
  summarise(
    mean_lnnosa = mean(lnnosa, na.rm = TRUE),
    sd_lnnosa   = sd(lnnosa, na.rm = TRUE),
    .groups     = "drop"
  )

pointsstel_nosa <- nosa_stel %>%
  group_by(MethodNameID, MethodName, CommonName, Group) %>%
  summarise(
    mean_lnnosa = mean(lnnosa, na.rm = TRUE),
    sd_lnnosa   = sd(lnnosa, na.rm = TRUE),
    .groups     = "drop"
  )

# grab bootstrap parameter estimates for variance
dfchin <- boot_chinM9$boot.params
dfchin <- data.frame(t(dfchin))
dfchin_r <- dfchin[, -c(1:8, 18:41)]

dfchin_pre <- dfchin_r
dfchin_pre[] <- 1/dfchin_pre

dfcoho <- boot_cohoM9$boot.params
dfcoho <- data.frame(t(dfcoho))
dfcoho_r <- dfcoho[, -c(1:9, 20:49)]

dfstel <- boot_stelM9$boot.params
dfstel <- data.frame(t(dfstel))
dfstel_r <- dfstel[, -c(1:7, 16:39)]

# grab mean and sd
names_r <- colnames(dfchin_r)
mean_r <- sapply(dfchin_r, mean)
sd_r <- sapply(dfchin_r, sd)
pointschin_r <- data.frame(Method = names_r, mean_r = mean_r, sd_r = sd_r)
pointschin_r$mean_pre <- (1/pointschin_r$mean_r)
pointschin_r$sd_pre <- (1/pointschin_r$sd_r)

names_pre <- colnames(dfchin_pre)
mean_pre <- sapply(dfchin_pre, mean)
sd_pre <- sapply(dfchin_pre, sd)
pointschin_pre <- data.frame(Method = names_pre, mean_pre = mean_pre, sd_pre = sd_pre)

names_r <- colnames(dfcoho_r)
mean_r <- sapply(dfcoho_r, mean)
sd_r <- sapply(dfcoho_r, sd)
pointscoho_r <- data.frame(Method = names_r, mean_r = mean_r, sd_r = sd_r)

names_r <- colnames(dfstel_r)
mean_r <- sapply(dfstel_r, mean)
sd_r <- sapply(dfstel_r, sd)
pointsstel_r <- data.frame(Method = names_r, mean_r = mean_r, sd_r = sd_r)

dfchin_r <- melt(dfchin_r)
dfchin_r$method <- as.character(dfchin_r$variable)
dfchin_r$method <- substr(dfchin_r$method, 4, nchar(dfchin_r$method))

dfcoho_r <- melt(dfcoho_r)
dfcoho_r$method <- as.character(dfcoho_r$variable)
dfcoho_r$method <- substr(dfcoho_r$method, 4, nchar(dfcoho_r$method))

dfstel_r <- melt(dfstel_r)
dfstel_r$method <- as.character(dfstel_r$variable)
dfstel_r$method <- substr(dfstel_r$method, 4, nchar(dfstel_r$method))

pointschin_r$MethodNameID <- as.character(pointschin_r$Method)
pointschin_r$MethodNameID <- substr(pointschin_r$MethodNameID , 4, nchar(pointschin_r$MethodNameID ))
pointschin_r <- pointschin_r[-c(1)]
pointschin_r$MethodNameID <- as.numeric(pointschin_r$MethodNameID)

pointscoho_r$MethodNameID <- as.character(pointscoho_r$Method)
pointscoho_r$MethodNameID <- substr(pointscoho_r$MethodNameID , 4, nchar(pointscoho_r$MethodNameID ))
pointscoho_r <- pointscoho_r[-c(1)]
pointscoho_r$MethodNameID <- as.numeric(pointscoho_r$MethodNameID)

pointsstel_r$MethodNameID <- as.character(pointsstel_r$Method)
pointsstel_r$MethodNameID <- substr(pointsstel_r$MethodNameID , 4, nchar(pointsstel_r$MethodNameID ))
pointsstel_r <- pointsstel_r[-c(1)]
pointsstel_r$MethodNameID <- as.numeric(pointsstel_r$MethodNameID)

# merge 
points_chin <- merge(pointschin_nosa, pointschin_r, by = "MethodNameID", all.x = TRUE, all.y = TRUE)
points_coho <- merge(pointscoho_nosa, pointscoho_r, by = "MethodNameID", all.x = TRUE, all.y = TRUE)
points_stel <- merge(pointsstel_nosa, pointsstel_r, by = "MethodNameID", all.x = TRUE, all.y = TRUE)

# plots
chin_splot <- ggplot(data=points_chin, aes(x = mean_r, y = mean_lnnosa, color = Group)) +
  geom_point(size = 3) +
  geom_smooth(aes(group = 1), 
              method = "lm", 
              color = "black", 
              se = TRUE, 
              linetype = "dashed", 
              linewidth = 1) + 
  labs(y = 'ln(NOSA)',
       title='Chinook',
       x = 'Variance') +
  scale_color_manual(values = c("#c1a13c", # dam counts
                                # "#c772c5",
                                "#5b3c90", # mixed methods
                                # "#b85c37",
                                "#b94656", # peak spawner count
                                # "#b0457b",
                                "#729a43", # AUC Population
                                "#6d85db", # Redd counts
                                "#4dc48f" # Weir counts
  )) +
  geom_errorbarh(data=points_chin, 
                 aes(xmin=ifelse(mean_r - 1.96*sd_r < 0, 0, mean_r - 1.96*sd_r), 
                     xmax=(mean_r + 1.96*sd_r), 
                     y = mean_lnnosa), 
                 linewidth = 1) +
  geom_errorbar(data=points_chin, 
                aes(ymin=(mean_lnnosa - 1.96*sd_lnnosa), 
                    ymax=(mean_lnnosa + 1.96*sd_lnnosa), 
                    x = mean_r),
                width = 0.01, 
                linewidth = 1) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 28),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y= element_text(size = 20),
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y= element_text(size = 18, color = "black"),
    panel.grid = element_blank(),
    legend.position = "right"
  )
chin_splot

points_chinTRUNC <- points_chin %>%
  filter(mean_r <= 0.5)
chin_splotTRUNC <- ggplot(data=points_chinTRUNC, aes(x = mean_r, y = mean_lnnosa, color = Group)) +
  geom_point(size = 3) +
  geom_smooth(aes(group = 1), 
              method = "lm", 
              color = "black", 
              se = TRUE, 
              linetype = "dashed", 
              linewidth = 1) + 
  labs(y = 'ln(NOSA)',
       title='Chinook - Method 3 omitted',
       subtitle = 'Method 3 = Area Under Curve: Population (Total live spawners)',
       x = 'Variance') +
  scale_color_manual(values = c("#c1a13c", # dam counts
                                # "#c772c5",
                                "#5b3c90", # mixed methods
                                # "#b85c37",
                                "#b94656", # peak spawner count
                                # "#b0457b",
                                # "#729a43", # AUC Population
                                "#6d85db", # Redd counts
                                "#4dc48f" # Weir counts
  )) +
  geom_errorbarh(data=points_chinTRUNC, 
                 aes(xmin=ifelse(mean_r - 1.96*sd_r < 0, 0, mean_r - 1.96*sd_r), 
                     xmax=(mean_r + 1.96*sd_r), 
                     y = mean_lnnosa), 
                 linewidth = 1) +
  geom_errorbar(data=points_chinTRUNC, 
                aes(ymin=(mean_lnnosa - 1.96*sd_lnnosa), 
                    ymax=(mean_lnnosa + 1.96*sd_lnnosa), 
                    x = mean_r),
                width = 0.01, 
                linewidth = 1) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 28),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y= element_text(size = 20),
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y= element_text(size = 18, color = "black"),
    panel.grid = element_blank(),
    legend.position = "right"
  )
chin_splotTRUNC

chin_splotTRUNCref <- ggplot(data=points_chinTRUNC, aes(x = mean_r, y = mean_lnnosa, color = Group)) +
  geom_point(size = 3) +
  geom_smooth(aes(group = 1), 
              method = "lm", 
              color = "black", 
              se = TRUE, 
              linetype = "dashed", 
              linewidth = 1) + 
  labs(y = 'ln(NOSA)',
       title='Chinook - Method 3 omitted',
       subtitle = 'Method 3 = Area Under Curve: Population (Total live spawners)',
       x = 'Variance') +
  scale_color_manual(values = c("#c1a13c", # dam counts
                                # "#c772c5",
                                "#5b3c90", # mixed methods
                                # "#b85c37",
                                "#b94656", # peak spawner count
                                # "#b0457b",
                                # "#729a43", # AUC Population
                                "#6d85db", # Redd counts
                                "#4dc48f" # Weir counts
  )) +
  geom_errorbarh(data=points_chinTRUNC, 
                 aes(xmin=ifelse(mean_r - 1.96*sd_r < 0, 0, mean_r - 1.96*sd_r), 
                     xmax=(mean_r + 1.96*sd_r), 
                     y = mean_lnnosa), 
                 linewidth = 1) +
  geom_errorbar(data=points_chinTRUNC, 
                aes(ymin=(mean_lnnosa - 1.96*sd_lnnosa), 
                    ymax=(mean_lnnosa + 1.96*sd_lnnosa), 
                    x = mean_r),
                width = 0.01, 
                linewidth = 1) +
  geom_text_repel(
    aes(label = stringr::str_wrap(MethodName, width = 20)), # Wraps text at ~20 characters
    size = 4.5,                  
    color = "black",             
    box.padding = 0.9,           # Padding around the text box so they push away from lines
    point.padding = 0.4,         # Distance away from the plot point
    max.overlaps = Inf,          # Forces R to show ALL labels despite overlaps
    # segment.color = "grey50",    # Color of the line connecting label to the point
    # segment.alpha = 0.6,         # Transparency of the indicator line
    show.legend = FALSE        # Prevents text letters from showing up in your color legend
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 28),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y= element_text(size = 20),
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y= element_text(size = 18, color = "black"),
    panel.grid = element_blank(),
    legend.position = "right"
  )
chin_splotTRUNCref

coho_splot <- ggplot(data=points_coho, aes(x = mean_r, y = mean_lnnosa, color = Group)) +
  geom_point(size = 3) +
  geom_smooth(aes(group = 1), method = "lm", color = "black", se = TRUE, linetype = "dashed", linewidth = 1) + 
  labs(y = 'ln(NOSA)',
       title='Coho',
       x = 'Variance') +
  scale_color_manual(values = c("#c1a13c", # dam counts
                                "#c772c5", # AUC monitoring
                                # "#5b3c90", # mixed methods
                                # "#b85c37",
                                "#b94656", # peak spawner count
                                # "#b0457b",
                                "#729a43" # AUC Population
                                # "#6d85db", # Redd counts
                                # "#4dc48f" # Weir counts
  )) +
  geom_errorbarh(data=points_coho, 
                 aes(xmin=ifelse(mean_r - 1.96*sd_r < 0, 0, mean_r - 1.96*sd_r), 
                     xmax=(mean_r + 1.96*sd_r), 
                     y = mean_lnnosa), 
                 linewidth = 1) +
  geom_errorbar(data=points_coho, 
                aes(ymin=(mean_lnnosa - 1.96*sd_lnnosa), 
                    ymax=(mean_lnnosa + 1.96*sd_lnnosa), 
                    x = mean_r),
                width = 0.01, 
                linewidth = 1) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 28),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y= element_text(size = 20),
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y= element_text(size = 18, color = "black"),
    panel.grid = element_blank(),
    legend.position = "right"
  )
coho_splot

points_cohoTRUNC <- points_coho %>%
  filter(mean_r <= 1)
coho_splotTRUNC <- ggplot(data=points_cohoTRUNC, aes(x = mean_r, y = mean_lnnosa, color = Group)) +
  geom_point(size = 3) +
  geom_smooth(aes(group = 1), method = "lm", color = "black", se = TRUE, linetype = "dashed", linewidth = 1) + 
  labs(y = 'ln(NOSA)',
       title='Coho - Method 11 omitted',
       subtitle = 'Method 11 = Dam counts + Expansion below dam',
       x = 'Variance') +
  scale_color_manual(values = c("#c1a13c", # dam counts
                                "#c772c5", # AUC monitoring
                                # "#5b3c90", # mixed methods
                                # "#b85c37",
                                "#b94656", # peak spawner count
                                # "#b0457b",
                                "#729a43" # AUC Population
                                # "#6d85db", # Redd counts
                                # "#4dc48f" # Weir counts
  )) +
  geom_errorbarh(data=points_cohoTRUNC, 
                 aes(xmin=ifelse(mean_r - 1.96*sd_r < 0, 0, mean_r - 1.96*sd_r), 
                     xmax=(mean_r + 1.96*sd_r), 
                     y = mean_lnnosa), 
                 linewidth = 1) +
  geom_errorbar(data=points_cohoTRUNC, 
                aes(ymin=(mean_lnnosa - 1.96*sd_lnnosa), 
                    ymax=(mean_lnnosa + 1.96*sd_lnnosa), 
                    x = mean_r),
                width = 0.01, 
                linewidth = 1) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 28),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y= element_text(size = 20),
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y= element_text(size = 18, color = "black"),
    panel.grid = element_blank(),
    legend.position = "right"
  )
coho_splotTRUNC

coho_splotTRUNCref <- ggplot(data=points_cohoTRUNC, aes(x = mean_r, y = mean_lnnosa, color = Group)) +
  geom_point(size = 3) +
  geom_smooth(aes(group = 1), method = "lm", color = "black", se = TRUE, linetype = "dashed", linewidth = 1) + 
  labs(y = 'ln(NOSA)',
       title='Coho - Method 11 omitted',
       subtitle = 'Method 11 = Dam counts + Expansion below dam',
       x = 'Variance') +
  scale_color_manual(values = c("#c1a13c", # dam counts
                                "#c772c5", # AUC monitoring
                                # "#5b3c90", # mixed methods
                                # "#b85c37",
                                "#b94656", # peak spawner count
                                # "#b0457b",
                                "#729a43" # AUC Population
                                # "#6d85db", # Redd counts
                                # "#4dc48f" # Weir counts
  )) +
  geom_errorbarh(data=points_cohoTRUNC, 
                 aes(xmin=ifelse(mean_r - 1.96*sd_r < 0, 0, mean_r - 1.96*sd_r), 
                     xmax=(mean_r + 1.96*sd_r), 
                     y = mean_lnnosa), 
                 linewidth = 1) +
  geom_errorbar(data=points_cohoTRUNC, 
                aes(ymin=(mean_lnnosa - 1.96*sd_lnnosa), 
                    ymax=(mean_lnnosa + 1.96*sd_lnnosa), 
                    x = mean_r),
                width = 0.01, 
                linewidth = 1) +
  geom_text_repel(
    aes(label = stringr::str_wrap(MethodName, width = 20)), # Wraps text at ~20 characters
    size = 4.5,                  
    color = "black",             
    box.padding = 0.9,           # Padding around the text box so they push away from lines
    point.padding = 0.4,         # Distance away from the plot point
    max.overlaps = Inf,          # Forces R to show ALL labels despite overlaps
    # segment.color = "grey50",    # Color of the line connecting label to the point
    # segment.alpha = 0.6,         # Transparency of the indicator line
    show.legend = FALSE        # Prevents text letters from showing up in your color legend
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 28),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y= element_text(size = 20),
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y= element_text(size = 18, color = "black"),
    panel.grid = element_blank(),
    legend.position = "right"
  )
coho_splotTRUNCref

stel_splot <- ggplot(data=points_stel, aes(x = mean_r, y = mean_lnnosa, color = Group)) +
  geom_point(size = 3) +
  geom_smooth(aes(group = 1), method = "lm", color = "black", se = TRUE, linetype = "dashed", linewidth = 1) + 
  labs(y = 'ln(NOSA)',
       title='Steelhead',
       x = 'Variance') +
  scale_color_manual(values = c("#c1a13c", # dam counts
                                # "#c772c5", # AUC monitoring
                                # "#5b3c90", # mixed methods
                                # "#b85c37",
                                # "#b94656", # peak spawner count
                                # "#b0457b",
                                # "#729a43", # AUC Population
                                "#6d85db", # Redd counts
                                "#4dc48f" # Weir counts
  )) +
  geom_errorbarh(data=points_stel, 
                 aes(xmin=ifelse(mean_r - 1.96*sd_r < 0, 0, mean_r - 1.96*sd_r), 
                     xmax=(mean_r + 1.96*sd_r), 
                     y = mean_lnnosa), 
                 linewidth = 1) +
  geom_errorbar(data=points_stel, 
                aes(ymin=(mean_lnnosa - 1.96*sd_lnnosa), 
                    ymax=(mean_lnnosa + 1.96*sd_lnnosa), 
                    x = mean_r),
                width = 0.01, 
                linewidth = 1) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 28),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y= element_text(size = 20),
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y= element_text(size = 18, color = "black"),
    panel.grid = element_blank(),
    legend.position = "right"
  )
stel_splot

points_stelTRUNC <- points_stel %>%
  filter(mean_r <= 0.5)
stel_splotTRUNC <- ggplot(data=points_stelTRUNC, aes(x = mean_r, y = mean_lnnosa, color = Group)) +
  geom_point(size = 3) +
  geom_smooth(aes(group = 1), method = "lm", color = "black", se = TRUE, linetype = "dashed", linewidth = 1) + 
  labs(y = 'ln(NOSA)',
       title ="Steelhead - 'Total redd counts * Fish per redd estimate' omitted",
       # subtitle = 'Method 22 = Total redd counts * Fish per redd estimate',
       x = 'Variance') +
  scale_color_manual(values = c("#c1a13c", # dam counts
                                # "#c772c5", # AUC monitoring
                                # "#5b3c90", # mixed methods
                                # "#b85c37",
                                # "#b94656", # peak spawner count
                                # "#b0457b",
                                # "#729a43", # AUC Population
                                "#6d85db", # Redd counts
                                "#4dc48f" # Weir counts
  )) +
  geom_errorbarh(data=points_stelTRUNC, 
                 aes(xmin=ifelse(mean_r - 1.96*sd_r < 0, 0, mean_r - 1.96*sd_r), 
                     xmax=(mean_r + 1.96*sd_r), 
                     y = mean_lnnosa), 
                 linewidth = 1) +
  geom_errorbar(data=points_stelTRUNC, 
                aes(ymin=(mean_lnnosa - 1.96*sd_lnnosa), 
                    ymax=(mean_lnnosa + 1.96*sd_lnnosa), 
                    x = mean_r),
                width = 0.01, 
                linewidth = 1) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 28),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y= element_text(size = 20),
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y= element_text(size = 18, color = "black"),
    panel.grid = element_blank(),
    legend.position = "right"
  )
stel_splotTRUNC

stel_splotTRUNCref <- ggplot(data=points_stelTRUNC, aes(x = mean_r, y = mean_lnnosa, color = Group)) +
  geom_point(size = 3) +
  geom_smooth(aes(group = 1), method = "lm", color = "black", se = TRUE, linetype = "dashed", linewidth = 1) + 
  labs(y = 'ln(NOSA)',
       title ="Steelhead - 'Total redd counts * Fish per redd estimate' omitted",
       # subtitle = 'Method 22 = Total redd counts * Fish per redd estimate',
       x = 'Variance') +
  scale_color_manual(values = c("#c1a13c", # dam counts
                                # "#c772c5", # AUC monitoring
                                # "#5b3c90", # mixed methods
                                # "#b85c37",
                                # "#b94656", # peak spawner count
                                # "#b0457b",
                                # "#729a43", # AUC Population
                                "#6d85db", # Redd counts
                                "#4dc48f" # Weir counts
  )) +
  geom_errorbarh(data=points_stelTRUNC, 
                 aes(xmin=ifelse(mean_r - 1.96*sd_r < 0, 0, mean_r - 1.96*sd_r), 
                     xmax=(mean_r + 1.96*sd_r), 
                     y = mean_lnnosa), 
                 linewidth = 1) +
  geom_errorbar(data=points_stelTRUNC, 
                aes(ymin=(mean_lnnosa - 1.96*sd_lnnosa), 
                    ymax=(mean_lnnosa + 1.96*sd_lnnosa), 
                    x = mean_r),
                width = 0.01, 
                linewidth = 1) +
  geom_text_repel(
    aes(label = stringr::str_wrap(MethodName, width = 20)), # Wraps text at ~20 characters
    size = 4.5,                  
    color = "black",             
    box.padding = 0.9,           # Padding around the text box so they push away from lines
    point.padding = 0.4,         # Distance away from the plot point
    max.overlaps = Inf,          # Forces R to show ALL labels despite overlaps
    # segment.color = "grey50",    # Color of the line connecting label to the point
    # segment.alpha = 0.6,         # Transparency of the indicator line
    show.legend = FALSE        # Prevents text letters from showing up in your color legend
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 28),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y= element_text(size = 20),
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y= element_text(size = 18, color = "black"),
    panel.grid = element_blank(),
    legend.position = "right"
  )
stel_splotTRUNCref
