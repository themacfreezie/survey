library(data.table)
library(dplyr)
library(gghighlight)
library(here) # set workind directory
library(readxl) # read excel sheets
library(tidyverse)

# grab data
methods <- read_excel(here("550", "data", "METHODS_ODFW_Recompiled2_and_figs.xlsx"), sheet = "data-wide", col_names = TRUE)
methodsL <- melt(setDT(methods), id.vars=c("popid"), variable.name = "year")
  # set methods data long
head(methodsL)
methodsL$year <- as.character(methodsL$year)
methodsL$year <- substr(methodsL$year, 3, 6)
methodsL$year <- as.numeric(methodsL$year)
head(methodsL)
  # set year as numeric
methodsL[methodsL == 0] <- NA
  # zeroes to NA
methodsL[[1]] <- NULL
  # drop pop-ids
methods_ct <- methodsL %>% 
  group_by(value, year) %>% 
  summarize("freq" = n())
  # make table of frequency of methods by year
head(methods_ct)
methods_ct$value <- as.factor(methods_ct$value)
  # set method as factor
methods_ct <- na.omit(methods_ct)
  # drop NAs
legend <- read_excel(here("550", "data", "method_key.xlsx"), col_names = TRUE)
legend$value <- legend$Method
  # pull in legend
methods_ct <- merge(methods_ct, legend, by=c("value"))
  # merge in legend

# methods large groupings
methods_ctGROUP <- methods_ct %>%
  group_by(year, Group) %>%
  summarise(freq = sum(freq))

# build dat for percentages
methods_pct <- methods_ct %>%
  group_by(year, value) %>%
  summarise(n = sum(freq)) %>%
  mutate(percentage = n / sum(n))

# plot
lines <- ggplot(data=methods_ct) + aes(x=year, y=Method, group=value, color=Group) + 
  geom_line(size = 1) +
  theme(axis.title.x =element_blank()) +
  labs(x = "",
       y='Survey Method',
       title='Use of Salmonid Survey Methods in Oregon',
       subtitle='1980-2022') +
  scale_x_continuous(expand = c(0, 0), 
                     breaks = c(1980, 1990, 2000, 2010, 2020)) +
  scale_fill_discrete(name = "Method Group") +
  scale_color_manual(values = c("#c1a13c",
                               "#c772c5",
                               "#5b3c90",
                               "#b85c37",
                               "#b94656",
                               "#b0457b",
                               "#729a43",
                               "#6d85db",
                               "#4dc48f")) +
  theme_classic() +
  theme(axis.text.y = element_blank())
lines <- lines + guides(color=guide_legend(title="Method Group"))
lines

survey <- ggplot(data=methods_ctGROUP) + aes(x=year, y=freq, fill=Group) + 
  geom_area(alpha=0.6 , size=.1, colour=NA) +
  theme(axis.title.x =element_blank()) +
  labs(x = "",
      y='Number of Surveys Conducted',
       title='Frequency of Salmonid Survey Methods in Oregon',
       subtitle='1980-2022') +
  scale_x_continuous(expand = c(0, 0), 
                     breaks = c(1980, 1990, 2000, 2010, 2020)) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) + 
  scale_fill_discrete(name = "Method Group") +
  scale_fill_manual(values = c("#c1a13c",
                               "#c772c5",
                               "#5b3c90",
                               "#b85c37",
                               "#b94656",
                               "#b0457b",
                               "#729a43",
                               "#6d85db",
                               "#4dc48f")) +
  theme_classic()
survey <- survey + guides(fill=guide_legend(title="Method Group"))
survey

methods_pctGROUP <- methods_ctGROUP %>%
  group_by(year) %>%
    mutate(percentage = freq / sum(freq) * 100) %>%
      ungroup()

surveyPCT <- ggplot(
  data = methods_pctGROUP,
  aes(x = year, y = percentage, fill = Group)
) +
  geom_area(
    alpha = 0.6,
    linewidth = 0.1,
    colour = NA
  ) +
  labs(
    x = "",
    y = "Percentage of Surveys (%)",
    title = "Proportion of Salmonid Survey Methods in Oregon",
    subtitle = "1980–2022",
    fill = "Method Group"
  ) +
  scale_x_continuous(
    breaks = c(1980, 1990, 2000, 2010, 2020),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20),
    labels = function(x) paste0(x, "%"),
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    values = c(
      "#c1a13c", 
      "#c772c5", 
      "#5b3c90",
      "#b85c37", 
      "#b94656", 
      "#b0457b",
      "#729a43", 
      "#6d85db", 
      "#4dc48f"
    )
  ) +
  theme_classic()
surveyPCT <- surveyPCT + guides(fill=guide_legend(title="Method Group"))
surveyPCT

surveyLINE <- ggplot(
  data = methods_ctGROUP,
  aes(x = year, y = freq, colour = Group, group = Group)
) +
  geom_line(linewidth = 2) +
  labs(
    x = "",
    y = "Number of Surveys Conducted",
    # title = "Frequency of Salmonid Survey Methods in Oregon",
    title = "",
    subtitle = "1980–2022",
    colour = "Method Group"
  ) +
  scale_x_continuous(
    breaks = c(1980, 1990, 2000, 2010, 2020),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_colour_manual(
    values = c(
      "#c1a13c", 
      "#c772c5", 
      "#5b3c90",
      "#b85c37", 
      "#b94656", 
      "#b0457b",
      "#729a43", 
      "#6d85db", 
      "#4dc48f"
    )
  ) +
  theme_classic() +
  theme(
    # plot.title = element_text(face = "bold", size = 26),
    plot.subtitle = element_text(size = 24, color = "black"),
    legend.title = element_blank(),
    legend.text = element_text(size = 24),
    axis.text.x = element_text(size = 22, color = "black"),
    axis.text.y = element_text(size = 22, color = "black"),
    axis.title.y = element_text(size = 24)
  )
surveyLINE <- surveyLINE + guides(fill=guide_legend(title="Method Group"))
surveyLINE

ggsave(here("550", "output", "lines.png"), plot=lines, device="png", dpi=300)
ggsave(here("550", "output", "survey.png"), plot=survey, device="png", dpi=300)

# scale_color_manual(values = c("salmon", "orange3", "gold3", "darkolivegreen3", "green4","cyan4", "darkblue", "darkorchid", "darkslateblue")) +





library(data.table)
library(dplyr)
library(gghighlight)
library(here) 
library(readxl) 
library(tidyverse)

# 1. Grab data
methods <- read_excel(here("550", "data", "METHODS_ODFW_Recompiled2_and_figs.xlsx"), sheet = "data-wide", col_names = TRUE)
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

# 2. Group by year and method category
methods_ctGROUP <- methods_ct %>%
  group_by(year, Group) %>%
  summarise(freq = sum(freq), .groups = "drop")

# 3. Calculate percentages AND find the maximum annual total to dynamically scale the line
methods_pctGROUP <- methods_ctGROUP %>%
  group_by(year) %>%
  mutate(
    annual_total = sum(freq),
    percentage = (freq / annual_total) * 100
  ) %>%
  ungroup()

# Find max total surveys across all years to define our secondary axis scale factor
max_total <- max(methods_pctGROUP$annual_total)
# A scaling multiplier that maps the maximum raw count exactly to 100% on the left axis
scale_factor <- 100 / max_total 

# 4. Generate the Dual-Axis 100% Stacked Bar Chart
dual_axis_chart <- ggplot(data = methods_pctGROUP) +
  # 100% Stacked Bars (Using a width of 0.8 leaves a clean gap between years)
  geom_col(
    aes(x = year, y = percentage, fill = Group),
    position = "stack",
    alpha = 0.75,
    width = 0.8
  ) +
  # Total Count Line Overlay (Scales absolute counts into the 0-100 percentage range)
  geom_line(
    aes(x = year, y = annual_total * scale_factor),
    color = "#2b2b2b",
    linewidth = 2,
    group = 1
  ) +
  # Adding clear point indicators for total count values per year
  # geom_point(
  #   aes(x = year, y = annual_total * scale_factor),
  #   color = "#2b2b2b",
  #   size = 4
  # ) +
  labs(
    x = "",
    y = "Percentage of Surveys (%)",
    title = "Salmonid Survey Methods in Oregon (1980–2022)",
    subtitle = "Bars: Proportional breakdown | Line: Total annual surveys conducted",
    fill = "Method Group"
  ) +
  # scale_x_continuous(
  #   breaks = seq(1980, 2020, by = 10)
  # ) +
  scale_x_continuous(
    breaks = seq(1980, 2020, by = 10),
    expand = expansion(add = c(1, 1)) # Adds 1 year of visual padding to the left and right
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
  # Configuring Dual Y-Axes
  # scale_y_continuous(
  #   limits = c(0, 100),
  #   breaks = seq(0, 100, by = 20),
  #   labels = function(x) paste0(x, "%"),
  #   expand = expansion(mult = c(0, 0.05)),
  #   # Sec_axis reverses the math multiplication to label the right axis accurately
  #   sec.axis = sec_axis(
  #     trans = ~ . / scale_factor, 
  #     name = "Total Number of Surveys Conducted"
  #   )
  # ) +
  scale_fill_manual(
    values = c(
      "#c1a13c", "#c772c5", "#5b3c90",
      "#b85c37", "#b94656", "#b0457b",
      "#729a43", "#6d85db", "#4dc48f"
    )
  ) +
  theme_classic() +
  theme(
    legend.position = "right",
    # Ensure secondary axis title is readable alongside the ticks
    axis.title.y.right = element_text(color = "#2b2b2b", margin = margin(l = 10))
  )

dual_axis_chart <- dual_axis_chart + guides(fill = guide_legend(title = "Method Group"))
dual_axis_chart