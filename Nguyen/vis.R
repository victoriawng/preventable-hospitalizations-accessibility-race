library(dplyr)
library(tidyverse)
library(haven)

national_data = read_sas("data_SAS/analytic_data2025_v2.sas7bdat")

# ==============================================================================
# first vis: (map) top race and stay data of the county ------------------------
# or cluster

# notes:
# cannot do states bc no state has majority non white over 70%
# so have to do county

race_largest_70plus
preventable_stays_county

# merge ========================================================================
# clinical_care_county_level_skim = skim(clinical_care_county_level) |>
#   select(skim_variable, n_missing, complete_rate) |>
#   left_join(data_dict_2025, by = c("skim_variable" = "variable")) |>
#   arrange(complete_rate)
# view(clinical_care_county_level_skim)
# ========================================================================

# clinical_care_county_level_skim = skim(clinical_care_county_level) |>
#   select(skim_variable, n_missing, complete_rate) |>
#   left_join(data_dict_2025, by = c("skim_variable" = "variable")) |>
#   arrange(complete_rate)
# view(clinical_care_county_level_skim)


view(preventable_stays_county_noNA)



# ---------------------------------------------------------


white_majority_70plus_statesonly = white_majority_70plus |>
  filter(countycode == "000", statecode != "00")
# view(white_majority_70plus_statesonly)

race_largest_noWhite_70plus_statesonly = race_largest_noWhite_70plus |>
  filter(countycode == "000", statecode != "00")
view(race_largest_noWhite_70plus_statesonly)

view(race_largest_noWhite_70plus)

view(race_rawvalue |>
  filter(countycode == "000", statecode != "00"))

view(race_largest |>
  filter(countycode == "000", statecode != "00"))

# white_majority_70plus
# -
# race_largest_noWhite_70plus ---
# black_majority_70plus
# native_majority_70plus
# hispanic_majority_70plus

view(black_majority_70plus)
