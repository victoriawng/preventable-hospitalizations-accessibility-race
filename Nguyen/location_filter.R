library(dplyr)
library(tidyverse)
library(haven)
national_data = read_sas("data_SAS/analytic_data2025_v2.sas7bdat")

# =============================================================================
# for state only ==============================================================
states = national_data |>
  # no USA
  filter(countycode == "000", statecode != "00")
# view(state)

# =============================================================================
# USA only  ===================================================================
USA = national_data |>
  filter(statecode == "00")
# view(USA)

# =============================================================================
# no states (only counties, parishes, boroughs ================================
no_states = national_data |>
  filter(countycode != "000")
# view(no_states)