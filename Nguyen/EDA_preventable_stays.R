# 6 columns for preventable stays
# 5 of them pertain to race


# 2025 County Health Rankings Data - v3.xlsx") # has z scores of preventable hospital stays, 2nd sheet

# all counties, state, and even one row for USA
preventable_stays = national_data |>
  dplyr::select(statecode, countycode, fipscode, state, county, 
                contains("v005"))
preventable_stays = preventable_stays |>
  dplyr::select(statecode, countycode, fipscode, state, county, where(~!all(is.na(.))))
view(preventable_stays)


# for states only ---------------------------------------------------------------
preventable_stays_state = preventable_stays |>
  filter(countycode == "000")
# preventable_stays_state = state |>
#   dplyr::select(statecode, countycode, fipscode, state, county,
#                 contains("v005"))
view(preventable_stays_state)


preventable_stays_state = preventable_stays_state |>
  # removes columns with all NA values like num, denom, CIs
  select(statecode, countycode, fipscode, state, county, where(~!all(is.na(.)))) |>
  mutate(
    num_NA = rowSums(is.na(preventable_stays_state)),
    aian_raw_ratio = v005_race_aian/v005_rawvalue,
    asian_raw_ratio = v005_race_asian/v005_rawvalue,
    black_raw_ratio = v005_race_black/v005_rawvalue,
    hispanic_raw_ratio = v005_race_hispanic/v005_rawvalue,
    white_raw_ratio = v005_race_white/v005_rawvalue
    )|>
  arrange(num_NA)
# preventable_stays_state = preventable_stays_state |>
#   select(-AIAN)
view(preventable_stays_state)
vis_miss(preventable_stays_state)

# show which rows were empty
# 6 AIAN groups empty (6/51 empty)
# 1 Asian group empty
preventable_stays_state[!complete.cases(preventable_stays_state), ]

preventable_stays_state_ratio = preventable_stays_state |>
  select(statecode, countycode, fipscode, state, county, (contains(c("rawvalue","ratio"))))

view(preventable_stays_state_ratio)

summary(preventable_stays_state_ratio)

# for county only --------------------------------------------------------------
preventable_stays_county = no_states |>
  dplyr::select(statecode, countycode, fipscode, state, county,
                contains("v005"))


preventable_stays_county = preventable_stays_county |>
  # remove columns with all empty
  select(statecode, countycode, fipscode, state, county, where(~!all(is.na(.)))) |>
  mutate(num_NA = rowSums(is.na(preventable_stays_county)))|>
  arrange(num_NA)
view(preventable_stays_county)
# lots of missing preventable stays for race besides white
vis_miss(preventable_stays_county)
skim(preventable_stays)

# show which rows were NOT empty
preventable_stays_county_noNA = preventable_stays_county[complete.cases(preventable_stays_county), ]
view(preventable_stays_county_noNA)
# or
# preventable_stays_county_noNA = preventable_stays_county |>
#   filter(num_NA == 0)
# view(preventable_stays_county_noNA)

preventable_stays_county_ratio = preventable_stays_county_noNA |>
  select(statecode, countycode, fipscode, state, county, (contains(c("rawvalue","ratio")))) |>
  mutate(
  aian_raw_ratio = v005_race_aian/v005_rawvalue,
  asian_raw_ratio = v005_race_asian/v005_rawvalue,
  black_raw_ratio = v005_race_black/v005_rawvalue,
  hispanic_raw_ratio = v005_race_hispanic/v005_rawvalue,
  white_raw_ratio = v005_race_white/v005_rawvalue)

# making map
install.packages(c("sf", "USAboundaries"))
library(leaflet)
library(sf)
library(USAboundaries)  # for U.S. state shapes
library(dplyr)


us_states()



preventable_stays |>
  ggplot(aes(x = num_NA)) +
  geom_bar() + 
  coord_flip()



view(preventable_stays)

vis_miss(preventable_stays)

skim(preventable_stays)

view(rowSums(is.na(preventable_stays)))


# Return rows where the column is not NA
view(preventable_stays |>
       filter(!is.na(v005_race_aian)))
vis_miss((preventable_stays |>
            filter(!is.na(v005_race_aian))))