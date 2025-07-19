library(tidyverse)
library(naniar)
library(janitor)
library(VIM)
library(stats)
library(stringr)
library(scales)
library(leaflet)
library(tigris)
library(glmmTMB)
library(broom)
library(glmmTMB)
library(car)
library(sf)
library(randomForest)

county_data <- read_csv("data/analytic_data2025_v2.csv") #FOR RUNNING ARTURO MODELS

cn_national_data <- national_data |> 
  clean_names()

cn_national_data_subset <- cn_national_data |> 
  select(
    state_fips_code,
    state_abbreviation,
    name,
    population_raw_value,
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
    children_eligible_for_free_or_reduced_price_lunch_raw_value,
    median_household_income_raw_value,
    income_inequality_raw_value
  )

cn_counties_national_subset <- cn_national_data_subset |> 
  filter(str_detect(name, "County|Municipality|Census|Borough|Region|District|Parish|city"))

char_cols <- c("state_abbreviation", "name")

cols_to_convert <- setdiff(names(cn_counties_national_subset), char_cols)

cn_counties_national_subset[cols_to_convert] <- lapply(
  cn_counties_national_subset[cols_to_convert],
  function(x) {
    x <- as.character(x)
    x[x %in% c("NA", "", "N/A", "*", "Suppressed")] <- NA
    as.numeric(x)
  }
)

race_cols <- c( #RUN THIS
  "percent_american_indian_or_alaska_native_raw_value",
  "percent_asian_raw_value",
  "percent_hispanic_raw_value",
  "percent_native_hawaiian_or_other_pacific_islander_raw_value",
  "percent_non_hispanic_black_raw_value",
  "percent_non_hispanic_white_raw_value"
)

racial_makeup <- cn_counties_national_subset |> #RUN THIS
  pivot_longer(
    cols = all_of(race_cols),
    names_to = "race",
    values_to = "percent"
  ) |> 
  group_by(state_fips_code, county_fips_code) |> 
  arrange(state_fips_code, county_fips_code, desc(percent)) |> 
  slice_head(n = 2) |> 
  summarise(
    racial_makeup = paste(
      gsub(
        "percent_|_raw_value", 
        "", 
        race
      ),
      collapse = ", "
    ),
    .groups = "drop"
  )

cn_counties_national_subset <- cn_counties_national_subset |>  #RUN THIS
  left_join(racial_makeup, by = c("state_fips_code", "county_fips_code"))

#You should have racial makeup variable in dataset. 


#SUBSET COUNTIES BY RACIAL MAKEUP
wh_counties <- your_subset |> 
  filter(racial_makeup == "non_hispanic_white, hispanic")

wb_counties <- your_subset |> 
  filter(racial_makeup == "non_hispanic_white, non_hispanic_black")

waian_counties <- your_subset |> 
  filter(racial_makeup == "non_hispanic_white, american_indian_or_alaska_native")

hw_counties <- your_subset |> 
  filter(racial_makeup == "hispanic, non_hispanic_white")

bw_counties <- your_subset |> 
  filter(racial_makeup == "non_hispanic_black, non_hispanic_white")



rf_subset_wh <- your_subset |> 
  select(primary_care_physicians_raw_value,
         uninsured_raw_value, mental_health_providers_raw_value, dentists_raw_value,
         broadband_access_raw_value, other_primary_care_providers_raw_value, mammography_screening_raw_value,
         severe_housing_problems_raw_value, high_school_completion_raw_value,
         percent_disability_functional_limitations_raw_value, percent_not_proficient_in_english_raw_value,
         percent_rural_raw_value, income_inequality_raw_value, median_household_income_raw_value,
         preventable_hospital_stays_raw_value)

rf_subset_wh <- na.omit(rf_subset_wh)

rf_subset_wb <- your_subset |> 
  select(primary_care_physicians_raw_value,
         uninsured_raw_value, mental_health_providers_raw_value, dentists_raw_value,
         broadband_access_raw_value, other_primary_care_providers_raw_value, mammography_screening_raw_value,
         severe_housing_problems_raw_value, high_school_completion_raw_value,
         percent_disability_functional_limitations_raw_value, percent_not_proficient_in_english_raw_value,
         percent_rural_raw_value, income_inequality_raw_value, median_household_income_raw_value,
         preventable_hospital_stays_raw_value)

rf_subset_wb <- na.omit(rf_subset_wb)

rf_subset_waian <- your_subset |> 
  select(primary_care_physicians_raw_value,
         uninsured_raw_value, mental_health_providers_raw_value, dentists_raw_value,
         broadband_access_raw_value, other_primary_care_providers_raw_value, mammography_screening_raw_value,
         severe_housing_problems_raw_value, high_school_completion_raw_value,
         percent_disability_functional_limitations_raw_value, percent_not_proficient_in_english_raw_value,
         percent_rural_raw_value, income_inequality_raw_value, median_household_income_raw_value,
         preventable_hospital_stays_raw_value)

rf_subset_waian <- na.omit(rf_subset_waian)

rf_subset_hw <- your_subset |> 
  select(primary_care_physicians_raw_value,
         uninsured_raw_value, mental_health_providers_raw_value, dentists_raw_value,
         broadband_access_raw_value, other_primary_care_providers_raw_value, mammography_screening_raw_value,
         severe_housing_problems_raw_value, high_school_completion_raw_value,
         percent_disability_functional_limitations_raw_value, percent_not_proficient_in_english_raw_value,
         percent_rural_raw_value, income_inequality_raw_value, median_household_income_raw_value,
         preventable_hospital_stays_raw_value)

rf_subset_hw <- na.omit(rf_subset_hw)

rf_subset_bw <- your_subset |> 
  select(primary_care_physicians_raw_value,
         uninsured_raw_value, mental_health_providers_raw_value, dentists_raw_value,
         broadband_access_raw_value, other_primary_care_providers_raw_value, mammography_screening_raw_value,
         severe_housing_problems_raw_value, high_school_completion_raw_value,
         percent_disability_functional_limitations_raw_value, percent_not_proficient_in_english_raw_value,
         percent_rural_raw_value, income_inequality_raw_value, median_household_income_raw_value,
         preventable_hospital_stays_raw_value)

rf_subset_bw <- na.omit(rf_subset_bw)


#Replace rf_model_blank and rf_subset_blank with each model/subset you want to run
#e.g. rf_model_wh and rf_subset_wh 'White majority and hispanic'

set.seed(42)
rf_model_ <- randomForest(
  preventable_hospital_stays_raw_value ~ .,
  data = rf_subset_wh,
  importance = TRUE,
  ntree = 500
)

#Once you run all the models, plot each of them for variable importance
#and/or use importance function for tabular format
#PLOT
varImpPlot(rf_model_wh)

#TABLE
importance(rf_model_bw)






