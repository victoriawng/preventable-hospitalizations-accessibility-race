library(tidyverse)
library(naniar)
library(janitor)
library(VIM)
library(stats)
library(stringr)
library(scales)
library(leaflet)
library(tigris)

national_data <- read_csv("../data/analytic_data2025_v2.csv")

clean_names_national_data <- national_data |> 
  clean_names()

clean_names_national_data_subset <- clean_names_national_data |> 
  select(
    state_fips_code,
    state_abbreviation,
    name,
    county_fips_code,
    flu_vaccinations_raw_value,
    primary_care_physicians_raw_value,
    ratio_of_population_to_primary_care_physicians,
    mental_health_providers_raw_value,
    ratio_of_population_to_mental_health_providers,
    dentists_raw_value,
    ratio_of_population_to_dentists,
    preventable_hospital_stays_raw_value,
    mammography_screening_raw_value,
    uninsured_raw_value,
    uninsured_adults_raw_value,
    severe_housing_problems_raw_value,
    percentage_of_households_with_overcrowding,
    percentage_of_households_with_high_housing_costs,
    broadband_access_raw_value,
    library_access_raw_value,
    some_college_raw_value,
    high_school_completion_raw_value,
    children_in_poverty_raw_value,
    child_care_cost_burden_raw_value,
    child_care_centers_raw_value,
    frequent_physical_distress_raw_value,
    frequent_mental_distress_raw_value,
    limited_access_to_healthy_foods_raw_value,
    food_insecurity_raw_value,
    insufficient_sleep_raw_value,
    teen_births_raw_value,
    excessive_drinking_raw_value,
    adult_smoking_raw_value,
    adult_obesity_raw_value,
    physical_inactivity_raw_value,
    uninsured_children_raw_value,
    other_primary_care_providers_raw_value,
    ratio_of_population_to_primary_care_providers_other_than_physicians,
    traffic_volume_raw_value,
    homeownership_raw_value,
    severe_housing_cost_burden_raw_value,
    severe_housing_problems_raw_value,
    access_to_parks_raw_value,
    census_participation_raw_value,
    high_school_graduation_raw_value,
    reading_scores_raw_value,
    math_scores_raw_value,
    living_wage_raw_value,
    residential_segregation_black_white_raw_value,
    disconnected_youth_raw_value,
    lack_of_social_and_emotional_support_raw_value,
    population_raw_value,
    children_in_single_parent_households_raw_value,
    percent_below_18_years_of_age_raw_value,
    percent_65_and_older_raw_value,
    percent_female_raw_value,
    percent_american_indian_or_alaska_native_raw_value,
    percent_asian_raw_value,
    percent_hispanic_raw_value,
    percent_native_hawaiian_or_other_pacific_islander_raw_value,
    percent_non_hispanic_black_raw_value,
    percent_non_hispanic_white_raw_value,
    percent_disability_functional_limitations_raw_value,
    percent_not_proficient_in_english_raw_value,
    percent_rural_raw_value,
    children_eligible_for_free_or_reduced_price_lunch_raw_value
  )

clean_names_national_subset_counties <- clean_names_national_data_subset |> 
  filter(str_detect(name, "County|Municipality|Census|Borough|Region|District|Parish|city"))

char_cols <- c("state_abbreviation", "name")


cols_to_convert <- setdiff(names(clean_names_national_subset_counties), char_cols)


clean_names_national_subset_counties[cols_to_convert] <- lapply(
  clean_names_national_subset_counties[cols_to_convert],
  function(x) {
    x <- as.character(x)
    x[x %in% c("NA", "", "N/A", "*", "Suppressed")] <- NA
    as.numeric(x)
  }
)


options(tigris_use_cache = TRUE)

counties_sf <- counties(cb = TRUE, year = 2023, class = "sf")

clean_names_national_subset_counties <- clean_names_national_subset_counties |> 
  mutate(
    state_fips_code = str_pad(state_fips_code, 2, pad = "0"),
    county_fips_code = str_pad(county_fips_code, 3, pad = "0"),
    fips = paste0(state_fips_code, county_fips_code)
  )

map_data <- counties_sf |> 
  left_join(clean_names_national_subset_counties, by = c("GEOID" = "fips"))

pal <- colorNumeric(
  palette = "magma",
  domain = map_data$preventable_hospital_stays_raw_value,
  na.color = "gray90"
)

m <- leaflet(map_data) |> 
  addProviderTiles("CartoDB.Positron") |>  
  addPolygons(
    fillColor = ~pal(preventable_hospital_stays_raw_value),
    weight = 0.2,
    opacity = 1,
    color = "white",
    fillOpacity = 0.8,
    label = ~paste0(NAME, ": ", 
                    ifelse(is.na(preventable_hospital_stays_raw_value), 
                           "No data", 
                           comma(preventable_hospital_stays_raw_value))),
    highlightOptions = highlightOptions(
      weight = 1.5,
      color = "#666",
      fillOpacity = 1,
      bringToFront = TRUE
    )
  ) |> 
  addLegend(
    pal = pal,
    values = ~preventable_hospital_stays_raw_value,
    title = "Preventable Hospital Stays",
    position = "bottomright"
  )

