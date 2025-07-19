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


# to be able to view these df, run EDA_preventable_stays.R AND EDA_race.R
view(race_largest_70plus)
view(preventable_stays_county)

race_preventablestays_county = race_largest_70plus |>
  left_join(preventable_stays_county, by = c("fipscode","statecode","countycode","state","county"))
view(race_preventablestays_county)
# |>
#   arrange(complete_rate)
# view(clinical_care_county_level_skim)

# map now ======================================================================
# install.packages("sf")

library(sf)
library(dplyr)  
library(ggplot2)
library(tigris)

options(tigris_use_cache = TRUE)  # optional: cache downloaded shapefiles
counties_sf <- counties(cb = TRUE, year = 2020, class = "sf")

view(counties_sf)
plot_df = left_join(counties_sf, race_preventablestays_county, by = c("GEOID" = "fipscode"))
# view(plot_df)

# Static map
ggplot(race_preventablestays_county) +
  geom_sf(aes(fill = v005_rawvalue), color = NA) +
  scale_fill_viridis_c(option = "plasma", name = "Preventable Stays") +
  labs(title = "Counties where top race ≥ 70% of population",
       subtitle = "Colored by rate of preventable hospital stays") +
  theme_minimal()

# Confirm plot_df is still spatial
inherits(plot_df, "sf")         # Should return TRUE
names(plot_df)                  # Should include 'geometry'
st_geometry(plot_df)           # Should return the shape data





# view(preventable_stays_county_noNA)



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
