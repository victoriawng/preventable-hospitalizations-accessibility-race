library(tidyverse)
library(naniar)
library(janitor)
library(VIM)
library(stats)
library(stringr)
library(patchwork)
source("EDA_import.R")

clean_names_national_data <- national_data |> 
  clean_names()

clean_names_national_data_subset <- clean_names_national_data |> 
  select(
    state_fips_code,
    state_abbreviation,
    name,
    county_fips_code,
    flu_vaccinations_raw_value,
    flu_vaccinations_aian,
    flu_vaccinations_asian_pacific_islander,
    flu_vaccinations_black,
    flu_vaccinations_hispanic,
    flu_vaccinations_white,
    primary_care_physicians_raw_value,
    ratio_of_population_to_primary_care_physicians,
    mental_health_providers_raw_value,
    ratio_of_population_to_mental_health_providers,
    dentists_raw_value,
    ratio_of_population_to_dentists,
    preventable_hospital_stays_raw_value,
    preventable_hospital_stays_aian,
    preventable_hospital_stays_asian_pacific_islander,
    preventable_hospital_stays_black,
    preventable_hospital_stays_hispanic,
    preventable_hospital_stays_white,
    mammography_screening_raw_value,
    mammography_screening_aian,
    mammography_screening_asian_pacific_islander,
    mammography_screening_black,
    mammography_screening_hispanic,
    mammography_screening_white,
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
    children_in_poverty_aian,
    children_in_poverty_asian_pacific_islander,
    children_in_poverty_black,
    children_in_poverty_hispanic,
    children_in_poverty_white,
    child_care_cost_burden_raw_value,
    child_care_centers_raw_value,
    frequent_physical_distress_raw_value,
    frequent_mental_distress_raw_value,
    limited_access_to_healthy_foods_raw_value,
    food_insecurity_raw_value,
    insufficient_sleep_raw_value,
    teen_births_raw_value,
    teen_births_aian,
    teen_births_asian,
    teen_births_black,
    teen_births_hispanic,
    teen_births_white,
    teen_births_nhopi,
    teen_births_two_or_more_races,
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
    reading_scores_aian,
    reading_scores_asian_pacific_islander,
    reading_scores_black,
    reading_scores_hispanic,
    reading_scores_white,
    math_scores_raw_value,
    math_scores_aian,
    math_scores_asian_pacific_islander,
    math_scores_black,
    math_scores_hispanic,
    math_scores_white,
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

############### MODELING
p1 <- ggplot(clean_names_national_subset_counties, 
             aes(x = log(preventable_hospital_stays_raw_value + 1))) +
  geom_histogram(fill = "blue", color = "white") +
  labs(title = "Log-Transformed",
       x = "Log(Preventable Hospital Stays)",
       y = "Count of Counties") +
  theme_minimal()
p2 <- ggplot(clean_names_national_subset_counties, 
             aes(x = preventable_hospital_stays_raw_value)) +
  geom_histogram(fill = "red", color = "white") +
  labs(title = "Raw Distribution",
       x = "Preventable Hospital Stays",
       y = "Count of Counties") +
  theme_minimal()
p1 + p2 + plot_annotation()

var(clean_names_national_subset_counties$preventable_hospital_stays_raw_value)
mean(clean_names_national_subset_counties$preventable_hospital_stays_raw_value)
