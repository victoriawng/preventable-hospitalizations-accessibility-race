# install.packages("haven")
library(readxl)
library(dplyr)
library(tidyverse)
library(haven)
library(visdat)
library(skimr)
library(leaflet)

national_data = read_sas("data/analytic_data2025_v2.sas7bdat")
trend_data = read_sas("data/chr_trends_sas_2025.sas7bdat")
data_dict_2025 = read_excel("data/DataDictionary_2025.xlsx")

# need preventable hospitalizations
# need to understand population

preventable_stays = national_data |>
  dplyr::select(statecode, countycode, fipscode, state, county,
                contains("v005"))
view(preventable_stays)

# ----------------------EDA_clinicalcare_county_level.R-------------------------
# df of counties, parishes only---
# no states or country (USA)
no_states = national_data |>
  filter(countycode != "000")
# view(no_states)


clinical_care_county_level = no_states |>
  select(statecode, countycode, fipscode, state, county, county_clustered,
         contains(c("v003", "v004", "v005", "v050", "v062", "v085", "v122", "v088", "v131", "v155", "v166")))
# view(clinical_care_county_level)

clinical_care_no_ci = clinical_care_all |>
  select(!contains(c("cilow", "cihigh")))
# view(clinical_care_no_ci)

clinical_care_rawvalue_num_denom = clinical_care_no_ci |>
  select(!contains(c("race")))
# view(clinical_care_rawvalue_num_denom)

# ----------------------EDA_clinicalcare_state_level.R--------------------------
state = national_data |>
  filter(countycode == "000")

clinical_care_state_level = state |>
  select(statecode, countycode, fipscode, state, county, county_clustered,
         contains(c("v003", "v004", "v005", "v050", "v062", "v085", "v122", "v088", "v131", "v155", "v166")))

# need 
