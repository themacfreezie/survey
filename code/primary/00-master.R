update.packages(ask = FALSE, checkBuilt = TRUE)

library(here)

# set loc
here::i_am("code/primary/00-master.R")

# cleaning and modeling
source(here("code", "primary", "01-dataclean.R"))
source(here("code", "primary", "02-method_labels.R"))
source(here("code", "primary", "03-model_build.R"))
# source(here("code", "primary", "03.1-model_allspecies.R"))
source(here("code", "primary", "03.2-ESUmodel_build.R"))
source(here("code", "primary", "04-bootstrap.R"))
source(here("code", "primary", "05-ARoutput.R"))
source(here("code", "primary", "05.1-ARoutput_table.R"))
source(here("code", "primary", "06-ARmap.R"))
source(here("code", "primary", "06.1-chinookARmap.R"))
source(here("code", "primary", "06.1.2-chinookARmapFIRSTLAST.R"))
source(here("code", "primary", "06.2-cohoARmap.R"))
source(here("code", "primary", "06.2.2-cohoARmapFIRSTLAST.R"))
source(here("code", "primary", "06.3-steelheadARmap.R"))
source(here("code", "primary", "06.3.2-steelheadARmapFIRSTLAST.R"))
source(here("code", "primary", "06.4-multipanelFIRSTLAST.R"))
source(here("code", "primary", "07-descriptive.R"))
source(here("code", "primary", "08-modelplots.R"))
source(here("code", "primary", "09-TScomparison.R"))
source(here("code", "primary", "09.1-TScomparisonESU.R"))
source(here("code", "primary", "10-TSmap.R"))
