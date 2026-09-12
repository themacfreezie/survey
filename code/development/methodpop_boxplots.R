## SET WORKING DIR & PACKAGES
library(here)
library(MARSS)
library(panelr)
library(tidyverse)

here::i_am("code/development/methodpop_boxplots.R")
options(max.print=2000)

# pull in data
load(here("data", "clean", "nosa_codes.Rda"))
nosa <- merge

# natural log of counts
nosa$lnnosa <- log(nosa$NOSA + 1)
nosa <- nosa[-c(1,2, 5:7, 9)]

# different species
nosa_chin <- nosa %>% filter(CommonName=="Chinook Salmon")
nosa_coho <- nosa %>% filter(CommonName=="Coho Salmon")
nosa_stel <- nosa %>% filter(CommonName=="Steelhead")

# plots
ggplot(data = nosa_chin, aes(x = MethodName, y = lnnosa)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  geom_hline(aes(yintercept = mean(lnnosa, na.rm = TRUE)), 
             color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Chinook",
    x = "",
    y = "ln(NOSA)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust = 1))

ggplot(data = nosa_coho, aes(x = MethodName, y = lnnosa)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  geom_hline(aes(yintercept = mean(lnnosa, na.rm = TRUE)), 
             color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Coho",
    x = "",
    y = "ln(NOSA)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust = 1))

ggplot(data = nosa_chin, aes(x = MethodName, y = lnnosa)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  geom_hline(aes(yintercept = mean(lnnosa, na.rm = TRUE)), 
             color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Steelhead",
    x = "",
    y = "ln(NOSA)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust = 1))
