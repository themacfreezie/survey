library(data.table)
library(dplyr)
library(gghighlight)
library(here) # set workind directory
library(readxl) # read excel sheets
library(tidyverse)

# set loc
here::i_am("code/554survey.R")

# grab data
methods <- read_excel(here("data", "METHODS_ODFW_Recompiled2_and_figs.xlsx"), sheet = "data-wide", col_names = TRUE)
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
legend <- read_excel(here("data", "method_key.xlsx"), col_names = TRUE)
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
  scale_fill_discrete(name = "Survey Method") +
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
survey <- survey + guides(fill=guide_legend(title="Survey Method"))
survey

ggsave(here("output", "survey.png"), plot=survey, device="png", dpi=300)

# scale_color_manual(values = c("salmon", "orange3", "gold3", "darkolivegreen3", "green4","cyan4", "darkblue", "darkorchid", "darkslateblue")) +