## SET WORKING DIR & PACKAGES
library(gt)
library(here)
library(readxl)

here::i_am("code/primary/05.2-ARoutput_table.R")
options(max.print=2000)

# import table from excel
AR <- read_excel(here("data", "clean", "AR_methods.xlsx"), sheet = "Final")
CV <- read_excel(here("data", "clean", "CoV_methods.xlsx"))

# create table
neat_tableAR <- AR %>%
  head(30) %>%        
  gt() %>%
  sub_missing(missing_text = "") %>%
  fmt_number(
    columns = where(is.numeric),
    decimals = 2
  ) %>%
  tab_header(
    title = "Survey method bias and precision",
    subtitle = "Values reported are log-transformed"
  ) %>%
  opt_align_table_header(align = "left")
neat_tableAR

neat_tableCV <- CV %>%
  head(30) %>%        
  gt() %>%
  sub_missing(missing_text = "") %>%
  fmt_number(
    columns = where(is.numeric),
    decimals = 2
  ) %>%
  tab_header(
    title = "Coefficients of variation across survey methods",
    subtitle = "Values reported are log-transformed"
  ) %>%
  opt_align_table_header(align = "left")
neat_tableCV

