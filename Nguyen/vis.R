library(dplyr)
library(tidyverse)
library(haven)

national_data = read_sas("data_SAS/analytic_data2025_v2.sas7bdat")

# ==============================================================================
# first vis: (map) top race and stay data of the county ------------------------
# or cluster
