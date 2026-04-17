## SET WORKING DIR & PACKAGES
library(here)
library(tidyverse)

here::i_am("code/primary/02-method_labels.R")
options(max.print=2000)

# pull in data
load(here("data", "clean", "nosa_dat.Rda"))
nosa <- nosa_mod.dat
load(here("data", "clean", "methods_list.Rda"))
mtds <- method_list

# cleaning methods look-up table
mtds <- mtds %>%
  # fix capitalization for ID 20
  mutate(MethodName = ifelse(MethodNameID == 20, "PIT-tag expansion estimate", MethodName)) %>%
  # keep only one row per ID
  distinct(MethodNameID, .keep_all = TRUE)

# combine methods with #.# structure (i.e. 21.0, 21.1, and 21.2)
mtds <- mtds %>%
  mutate(
    # if the ID starts with 21 (e.g., 21.1, 21.2), make it exactly 21
    MethodNameID = ifelse(floor(MethodNameID) == 21, 21, MethodNameID),
    # update the name for all ID 21 rows
    MethodName = ifelse(MethodNameID == 21, 
                        "Index redd count expansion * Fish per redd estimate", 
                        MethodName)
  ) %>% 
  distinct(MethodNameID, .keep_all = TRUE)

# same catch for method 26
mtds <- mtds %>%
  # fix capitalization for ID 20
  mutate(MethodName = ifelse(MethodNameID == 26, "In-river weir counts + Index redd count expansion * Fish per redd estimate", MethodName)) %>%
  # keep only one row per ID
  distinct(MethodNameID, .keep_all = TRUE)

mtds <- mtds %>%
  mutate(
    # if the ID starts with 21 (e.g., 21.1, 21.2), make it exactly 21
    MethodNameID = ifelse(floor(MethodNameID) == 17, 17, MethodNameID),
    # update the name for all ID 21 rows
    MethodName = ifelse(MethodNameID == 17, 
                        "Peak redd counts * Fish per redd estimate", 
                        MethodName)
  ) %>% 
  distinct(MethodNameID, .keep_all = TRUE)

## pure ai here - trying to lock down unique elements
# 1. Get all unique individual elements from the MethodName column
all_elements <- mtds %>%
  separate_rows(MethodName, sep = "\\s*\\+\\s*") %>% # Split by '+' and trim whitespace
  pull(MethodName) %>%
  unique() %>%
  sort()

# 2. Create a lookup table: each unique string gets a unique alphabetical code
# letters[1:26] provides a-z. If you have >26, you can use combinations.
codes <- setNames(paste0(letters[seq_along(all_elements)]), all_elements)

# 3. Define a function to translate names to codes
translate_to_code <- function(name) {
  parts <- str_split(name, "\\s*\\+\\s*")[[1]] # Split the string
  mapped_parts <- codes[parts]                  # Map to alphabetical letters
  paste(mapped_parts, collapse = " + ")         # Recombine with +
}

# 4. Apply to your dataframe
mtds <- mtds %>%
  rowwise() %>%
  mutate(MethodCode = translate_to_code(MethodName)) %>%
  ungroup()

## seems to work great!

# look at method names for nosa
nosa_methods <- nosa %>%
  distinct(MethodNameID)
  # need to truncate methods

nosa <- nosa %>%
  mutate(MethodNameID = floor(MethodNameID))
nosa <- nosa[-c(7)]

merge <- nosa %>%
  left_join(mtds, by = "MethodNameID")

save(merge, file=here("data", "clean", "nosa_codes.Rda"))