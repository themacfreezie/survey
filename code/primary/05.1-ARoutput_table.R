## SET WORKING DIR & PACKAGES
library(gt)
library(here)
library(readxl)

here::i_am("code/primary/05.1-ARoutput_table.R")
options(max.print=2000)

# import table from excel
df <- read_excel(here("data", "clean", "AR_methods.xlsx"), sheet = "Final")

# create table
neat_table <- df %>%
  head(30) %>%        
  gt() %>%
  sub_missing(missing_text = "") %>%
  fmt_number(
    columns = where(is.numeric),
    decimals = 2
  ) %>%
  tab_header(
    title = "Survey method bias and variance",
    subtitle = "Bias measured relative to 'Dam Counts' method"
  ) %>%
  opt_align_table_header(align = "left") 
neat_table