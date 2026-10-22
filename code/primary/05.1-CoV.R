## SET WORKING DIR & PACKAGES
library(here)
library(tidyverse)

here::i_am("code/primary/05.1-CoV.R")
options(max.print=2000)

# load data
load(here("data", "clean", "merged_ARdata.rda"))
load(here("data", "clean", "mtds.Rda"))

Rdata <- merge(merged_ARdata, mtds, by = c("MethodNameID"))
Rdata <- Rdata[-c(4, 7)]
Rdata$SD <- sqrt(Rdata$R)

# different species
Rdata_chin <- Rdata %>% filter(CommonName=="Chinook Salmon")
Rdata_coho <- Rdata %>% filter(CommonName=="Coho Salmon")
Rdata_stel <- Rdata %>% filter(CommonName=="Steelhead")

# save unique methods and SD combinations
chinMTDS_list <- Rdata_chin |> 
  distinct(MethodNameID, SD)
cohoMTDS_list <- Rdata_coho |> 
  distinct(MethodNameID, SD)
stelMTDS_list <- Rdata_stel |> 
  distinct(MethodNameID, SD)

# split data by survey method
Rdata_chinLIST <- split(Rdata_chin, Rdata_chin$MethodNameID)
Rdata_cohoLIST <- split(Rdata_coho, Rdata_coho$MethodNameID)
Rdata_stelLIST <- split(Rdata_stel, Rdata_stel$MethodNameID)

# get method level abundance averages
chin_avgNOSA <- sapply(Rdata_chinLIST, function(df) {
  mean(df$lnnosa, na.rm = TRUE)
})
chin_avgNOSA <- data.frame(
  Group = names(chin_avgNOSA),
  Mean_Value = as.numeric(chin_avgNOSA),
  row.names = NULL
)
names(chin_avgNOSA) <- c("MethodNameID", "avgNOSA")
chin_avgNOSA

coho_avgNOSA <- sapply(Rdata_cohoLIST, function(df) {
  mean(df$lnnosa, na.rm = TRUE)
})
coho_avgNOSA <- data.frame(
  Group = names(coho_avgNOSA),
  Mean_Value = as.numeric(coho_avgNOSA),
  row.names = NULL
)
names(coho_avgNOSA) <- c("MethodNameID", "avgNOSA")
coho_avgNOSA

stel_avgNOSA <- sapply(Rdata_stelLIST, function(df) {
  mean(df$lnnosa, na.rm = TRUE)
})
stel_avgNOSA <- data.frame(
  Group = names(stel_avgNOSA),
  Mean_Value = as.numeric(stel_avgNOSA),
  row.names = NULL
)
names(stel_avgNOSA) <- c("MethodNameID", "avgNOSA")
stel_avgNOSA

# merge averages and SDs
chinCoV <- merge(chin_avgNOSA, chinMTDS_list, by = c("MethodNameID"))
cohoCoV <- merge(coho_avgNOSA, cohoMTDS_list, by = c("MethodNameID"))
stelCoV <- merge(stel_avgNOSA, stelMTDS_list, by = c("MethodNameID"))

# construct CoV
chinCoV$CoV <- (chinCoV$SD/chinCoV$avgNOSA)
cohoCoV$CoV <- (cohoCoV$SD/cohoCoV$avgNOSA)
stelCoV$CoV <- (stelCoV$SD/stelCoV$avgNOSA)

save(chinCoV, file=here::here("data", "clean", "chinCoV.rda"))
save(cohoCoV, file=here::here("data", "clean", "cohoCoV.rda"))
save(stelCoV, file=here::here("data", "clean", "stelCoV.rda"))

chinCoV$PopID <- 1000
cohoCoV$PopID <- 1000
stelCoV$PopID <- 1000

# what about by population as well
chin_avg_list <- lapply(Rdata_chinLIST, function(df) {
  df |> 
    group_by(MethodNameID, PopID) |> 
    summarize(avgNOSA = mean(lnnosa, na.rm = TRUE), .groups = "drop")
})
chin_avgNOSApop <- bind_rows(chin_avg_list)
print(chin_avgNOSApop)

coho_avg_list <- lapply(Rdata_cohoLIST, function(df) {
  df |> 
    group_by(MethodNameID, PopID) |> 
    summarize(avgNOSA = mean(lnnosa, na.rm = TRUE), .groups = "drop")
})
coho_avgNOSApop <- bind_rows(coho_avg_list)
print(coho_avgNOSApop)

stel_avg_list <- lapply(Rdata_stelLIST, function(df) {
  df |> 
    group_by(MethodNameID, PopID) |> 
    summarize(avgNOSA = mean(lnnosa, na.rm = TRUE), .groups = "drop")
})
stel_avgNOSApop <- bind_rows(stel_avg_list)
print(stel_avgNOSApop)

# merge averages and SDs
chinCoVpop <- merge(chin_avgNOSApop, chinMTDS_list, by = c("MethodNameID"))
cohoCoVpop <- merge(coho_avgNOSApop, cohoMTDS_list, by = c("MethodNameID"))
stelCoVpop <- merge(stel_avgNOSApop, stelMTDS_list, by = c("MethodNameID"))

# construct CoV
chinCoVpop$CoV <- (chinCoVpop$SD/chinCoVpop$avgNOSA)
cohoCoVpop$CoV <- (cohoCoVpop$SD/cohoCoVpop$avgNOSA)
stelCoVpop$CoV <- (stelCoVpop$SD/stelCoVpop$avgNOSA)

# bring in the universal CoV
chinCoV <- rbind(chinCoV, chinCoVpop)
cohoCoV <- rbind(cohoCoV, cohoCoVpop)
stelCoV <- rbind(stelCoV, stelCoVpop)

chinCoV <- merge(chinCoV, mtds, by = c("MethodNameID"))
cohoCoV <- merge(cohoCoV, mtds, by = c("MethodNameID"))
stelCoV <- merge(stelCoV, mtds, by = c("MethodNameID"))

# plot
chinCoV <- chinCoV |> 
  mutate(Highlight = ifelse(PopID == 1000, "Universal", "Population"))
ggplot() +
  # geom_jitter(width = 0.2, height = 0, alpha = 1, size = 2) +
  geom_point(
    data = filter(chinCoV, PopID == 1000),
    aes(x = MethodName, y = CoV),
    size = 3, color = "firebrick"
  ) +
  geom_point(data = filter(chinCoV, PopID != 1000), 
             aes(x = MethodName, y = CoV),
             alpha = 0.6, size = 2, color = "steelblue") +
  labs(
    x = "",
    title = 'Chinook survey method coefficients of variation',
    y = "Coefficient of Variation"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 28),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y= element_text(size = 18, color = "black"),
    axis.title.y = element_text(size = 18),
    panel.grid = element_blank(),
    legend.position = "right",
    plot.margin = margin(t = 10, r = 175, b = 10, l = 10, unit = "pt")
  ) +
  theme(axis.text.x = element_text(angle = 330, hjust = 0, vjust = 0.9)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35))

cohoCoV <- cohoCoV |> 
  mutate(Highlight = ifelse(PopID == 1000, "Universal", "Population"))
ggplot() +
  geom_point(
    data = filter(cohoCoV, PopID == 1000),
    aes(x = MethodName, y = CoV),
    size = 3, color = "firebrick"
  ) +
  geom_point(data = filter(cohoCoV, PopID != 1000), 
             aes(x = MethodName, y = CoV),
             alpha = 0.6, size = 2, color = "steelblue") +
  labs(
    x = "",
    title = 'Coho survey method coefficients of variation',
    y = "Coefficient of Variation"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 28),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y= element_text(size = 18, color = "black"),
    axis.title.y = element_text(size = 18),
    panel.grid = element_blank(),
    legend.position = "right",
    plot.margin = margin(t = 10, r = 175, b = 10, l = 10, unit = "pt")
  ) +
  theme(axis.text.x = element_text(angle = 330, hjust = 0, vjust = 0.9)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 36))

stelCoV <- stelCoV |> 
  mutate(Highlight = ifelse(PopID == 1000, "Universal", "Population"))
ggplot() +
  geom_point(
    data = filter(stelCoV, PopID == 1000),
    aes(x = MethodName, y = CoV),
    size = 3, color = "firebrick"
  ) +
  geom_point(data = filter(stelCoV, PopID != 1000), 
             aes(x = MethodName, y = CoV),
             alpha = 0.6, size = 2, color = "steelblue") +
  labs(
    x = "",
    title = 'Steelhead survey method coefficients of variation',
    y = "Coefficient of Variation"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 28),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y= element_text(size = 18, color = "black"),
    axis.title.y = element_text(size = 18),
    panel.grid = element_blank(),
    legend.position = "right",
    plot.margin = margin(t = 10, r = 150, b = 10, l = 10, unit = "pt")
  ) +
  theme(axis.text.x = element_text(angle = 330, hjust = 0, vjust = 0.9)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30))
