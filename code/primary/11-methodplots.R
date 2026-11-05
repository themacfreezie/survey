library(data.table)
library(dplyr)
library(gghighlight)
library(here) 
library(readxl) 
library(tidyverse)

here::i_am("code/primary/11-methodplots.R")
options(max.print=2000)

# get data
methods <- read_excel(here("data", "raw", "METHODS_ODFW_Recompiled2_and_figs.xlsx"), sheet = "data-wide", col_names = TRUE)
methodsL <- melt(setDT(methods), id.vars=c("popid"), variable.name = "year")

methodsL$year <- as.character(methodsL$year)
methodsL$year <- substr(methodsL$year, 3, 6)
methodsL$year <- as.numeric(methodsL$year)

methodsL[methodsL == 0] <- NA
methodsL[[1]] <- NULL

methods_ct <- methodsL %>% 
  group_by(value, year) %>% 
  summarize("freq" = n(), .groups = "drop")

methods_ct$value <- as.factor(methods_ct$value)
methods_ct <- na.omit(methods_ct)

legend <- read_excel(here("550", "data", "method_key.xlsx"), col_names = TRUE)
legend$value <- legend$Method

methods_ct <- merge(methods_ct, legend, by=c("value"))

# group by year and method category
methods_ctGROUP <- methods_ct %>%
  group_by(year, Group) %>%
  summarise(freq = sum(freq), .groups = "drop")

# calculate percentages AND find the maximum annual total
methods_pctGROUP <- methods_ctGROUP %>%
  group_by(year) %>%
  mutate(
    annual_total = sum(freq),
    percentage = (freq / annual_total) * 100
  ) %>%
  ungroup()

# find max total surveys across all years
max_total <- max(methods_pctGROUP$annual_total)
# a scaling multiplier - map max raw count exactly to 100%
scale_factor <- 100 / max_total 

# generate dual axis stacked bar chart
dual_axis_chart <- ggplot(data = methods_pctGROUP) +
  # 100% stacked bars
  geom_col(
    aes(x = year, y = percentage, fill = Group),
    position = "stack",
    alpha = 0.75,
    width = 0.8
  ) +
  geom_line(
    aes(x = year, y = annual_total * scale_factor),
    color = "#2b2b2b",
    linewidth = 2,
    group = 1
  ) +
  labs(
    x = "",
    y = "Percentage of Surveys (%)",
    title = "Salmonid Survey Methods in Oregon (1980–2024)",
    # subtitle = "Bars: Proportional breakdown | Line: Total annual surveys conducted",
    fill = "Method Group"
  ) +
  scale_x_continuous(
    breaks = seq(1980, 2020, by = 10),
    expand = expansion(add = c(1, 1)) 
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 20),
    labels = function(x) paste0(x, "%"),
    expand = c(0, 0),
    sec.axis = sec_axis(
      trans = ~ . / scale_factor, 
      name = "Total Number of Surveys Conducted"
    )
  ) +
  scale_fill_manual(
    values = c(
      "#c1a13c", "#c772c5", "#5b3c90",
      "#b85c37", "#b94656", "#008080",
      "#729a43", "#6d85db", "#4dc48f"
    ) 
  ) +
  #   values = c(
  #     "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"
  #     )
  # ) +
  coord_cartesian(clip = "off") + 
  theme_classic() +
  theme(
    legend.position = "right",
    axis.title.y.right = element_text(color = "#2b2b2b", margin = margin(l = 10))
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 26),
    # plot.subtitle = element_text(size = 24, color = "black"),
    legend.title = element_blank(),
    legend.text = element_text(size = 24),
    axis.text.x = element_text(size = 22, color = "black"),
    axis.text.y = element_text(size = 22, color = "black"),
    axis.title.y = element_text(size = 24)
  )
dual_axis_chart <- dual_axis_chart + guides(fill = guide_legend(title = "Method Group"))
dual_axis_chart