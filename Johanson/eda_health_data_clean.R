#Run at start
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

county_data <- read_csv("data/analytic_data2025_v2.csv") #FOR RUNNING ARTURO MODELS

national_data <- read_csv("data/analytic_data2025_v2.csv") #FOR RUNNING THIS FILE

#Clean names
cn_national_data <- national_data |> 
  clean_names()

#Subset variables of interest
cn_national_data_subset <- cn_national_data |> 
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
    children_eligible_for_free_or_reduced_price_lunch_raw_value,
    median_household_income_raw_value,
    income_inequality_raw_value
  )

##CLEAN DATA SET
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


##TO VIEW MISSING VALUES (IGNORE)
vars <- colnames(cn_counties_national_subset)

var_groups <- split(vars, cut(seq_along(vars), 4, labels = FALSE))

for (i in 1:4) {
  
  
  data_subset <- cn_counties_national_subset[, var_groups[[i]]]
  
  
  print(gg_miss_var(data_subset) + ggtitle(paste("Missingness - Group", i)))
}


#Scatter plot variable against preventable hospital stays
predict_cn_subset_ratios |>
  ggplot(aes(x = ratio_of_population_to_mental_health_providers,
             y = preventable_hospital_stays_raw_value)) + 
  geom_point(alpha = 0.3, size = 1) +
  scale_y_log10() +
  scale_x_log10() +
  labs(y = "Preventable Hospital Stays (log scale)")

#Correlation between two variables
with(clean_names_national_subset_counties, 
     cor(ratio_of_population_to_primary_care_physicians,
         preventable_hospital_stays_raw_value))

#Map Stuff
options(tigris_use_cache = TRUE)

# Get shapefile for all US counties
counties_sf <- counties(cb = TRUE, year = 2023, class = "sf")

cn_counties_national_subset <- cn_counties_national_subset |> 
  mutate(
    state_fips_code = str_pad(state_fips_code, 2, pad = "0"),
    county_fips_code = str_pad(county_fips_code, 3, pad = "0"),
    fips = paste0(state_fips_code, county_fips_code)
  )

map_data <- counties_sf |> 
  left_join(cn_counties_national_subset, by = c("GEOID" = "fips"))

#Interactive US County Map

#Run first
pal <- colorNumeric(
  palette = "magma",
  domain = map_data$preventable_hospital_stays_raw_value,
  na.color = "gray90"
)

#Leaflet map
leaflet(map_data) |> 
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

#Early Modeling

poisson_model <- glm(
  preventable_hospital_stays_raw_value ~ uninsured_raw_value + ratio_of_population_to_primary_care_physicians
  + ratio_of_population_to_mental_health_providers + percent_disability_functional_limitations_raw_value
  + ratio_of_population_to_dentists + percent_rural_raw_value + broadband_access_raw_value
  + percent_65_and_older_raw_value + ratio_of_population_to_primary_care_providers_other_than_physicians,
  family = poisson(link = "log"),
  data = clean_names_national_subset_counties,
  na.action = na.exclude
)

tidy(poisson_model)


#Correlations

with(cn_counties_race_data, 
     cor(percent_asian_raw_value,
         preventable_hospital_stays_raw_value))

with(cn_counties_race_data, 
     cor(percent_non_hispanic_black_raw_value,
         preventable_hospital_stays_raw_value))

with(cn_counties_race_data, 
     cor(percent_non_hispanic_white_raw_value,
         preventable_hospital_stays_raw_value))

with(clean_names_national_subset_counties, 
     cor(high_school_completion_raw_value, preventable_hospital_stays_raw_value, use = "complete.obs"))

with(clean_names_national_subset_counties, 
     cor(severe_housing_cost_burden_raw_value, preventable_hospital_stays_raw_value, use = "complete.obs"))

?cor
#Race Data

cn_counties_race_data <- clean_names_national_subset_counties |>
  select(state_abbreviation, name, fips, percent_asian_raw_value, percent_american_indian_or_alaska_native_raw_value,
         percent_hispanic_raw_value, percent_non_hispanic_black_raw_value, percent_non_hispanic_white_raw_value,
         percent_native_hawaiian_or_other_pacific_islander_raw_value, preventable_hospital_stays_raw_value, percent_disability_functional_limitations_raw_value,
         high_school_completion_raw_value)

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

predict_cn_subset <- cn_counties_national_subset |> #DONT RUN
  select(state_abbreviation, name, preventable_hospital_stays_raw_value,
         uninsured_raw_value, dentists_raw_value, other_primary_care_providers_raw_value,
         broadband_access_raw_value, mental_health_providers_raw_value,
         primary_care_physicians_raw_value, mammography_screening_raw_value,
         severe_housing_problems_raw_value, high_school_completion_raw_value,
         percent_disability_functional_limitations_raw_value, percent_not_proficient_in_english_raw_value,
         percent_rural_raw_value, racial_makeup, income_inequality_raw_value, median_household_income_raw_value)

predict_cn_subset_ratios <- cn_counties_national_subset |> #RUN THIS
  select(state_abbreviation, name, ratio_of_population_to_primary_care_physicians, preventable_hospital_stays_raw_value,
         uninsured_raw_value, ratio_of_population_to_mental_health_providers, ratio_of_population_to_primary_care_providers_other_than_physicians,
         broadband_access_raw_value, ratio_of_population_to_dentists,
         primary_care_physicians_raw_value, mammography_screening_raw_value,
         severe_housing_problems_raw_value, high_school_completion_raw_value,
         percent_disability_functional_limitations_raw_value, percent_not_proficient_in_english_raw_value,
         percent_rural_raw_value, racial_makeup, income_inequality_raw_value, median_household_income_raw_value)

with(predict_cn_subset_ratios, 
     cor(uninsured_raw_value, preventable_hospital_stays_raw_value, use = "complete.obs"))

#Negative Binomial + Random Effects (DONT RUN)
nb_model <- glmmTMB(
  preventable_hospital_stays_raw_value ~ uninsured_raw_value + dentists_raw_value 
  + other_primary_care_providers_raw_value + broadband_access_raw_value
  + mental_health_providers_raw_value + primary_care_physicians_raw_value
  + mammography_screening_raw_value + severe_housing_problems_raw_value
  + high_school_completion_raw_value + percent_disability_functional_limitations_raw_value
  + percent_not_proficient_in_english_raw_value + percent_rural_raw_value + income_inequality_raw_value
  + (1 | racial_makeup),
  family = nbinom2,
  data = predict_cn_subset
)



summary(nb_model)

predict_cn_subset_ratios <- na.omit(predict_cn_subset_ratios) #RUN THIS


nb_model_ratios <- glmmTMB( #RUN THIS
  preventable_hospital_stays_raw_value ~ uninsured_raw_value + ratio_of_population_to_dentists 
  + ratio_of_population_to_primary_care_providers_other_than_physicians + broadband_access_raw_value
  + ratio_of_population_to_mental_health_providers + ratio_of_population_to_primary_care_physicians
  + mammography_screening_raw_value + severe_housing_problems_raw_value
  + high_school_completion_raw_value + percent_disability_functional_limitations_raw_value
  + percent_not_proficient_in_english_raw_value + percent_rural_raw_value + income_inequality_raw_value
  + (1 | racial_makeup),
  family = nbinom2,
  data = predict_cn_subset_ratios
)



summary(nb_model_ratios) #RUN THIS




#NB RATIOS MODEL IN TABLE
variables <- c(
  "(Intercept)",
  "uninsured_raw_value",
  "ratio_of_population_to_dentists",
  "ratio_of_population_to_primary_care_providers_other_than_physicians",
  "broadband_access_raw_value",
  "ratio_of_population_to_mental_health_providers",
  "ratio_of_population_to_primary_care_physicians",
  "mammography_screening_raw_value",
  "severe_housing_problems_raw_value",
  "high_school_completion_raw_value",
  "percent_disability_functional_limitations_raw_value",
  "percent_not_proficient_in_english_raw_value",
  "percent_rural_raw_value",
  "income_inequality_raw_value"
)

estimates <- c(
  8.789e+00,
  -5.897e-01,
  9.159e-06,
  -1.112e-05,
  1.127e-01,
  1.262e-01,
  -2.435e-01,
  -3.637e-01,
  -1.277e+00,
  3.114e-01,
  1.608e+00,
  -4.789e-01,
  -1.311e-01,
  2.644e-01
)

pvalues <- c(
  "< 2e-16",
  0.001502,
  0.004750,
  0.070087,
  0.481642,
  0.022216,
  0.430034,
  0.000413,
  9.79e-09,
  4.41e-07,
  1.90e-16,
  0.284340,
  4.70e-06,
  0.017167
)

exp_estimates <- exp(estimates)

results_df <- data.frame(
  Variable = variables,
  exp_Coefficient = round(exp_estimates, 4),
  p_value = pvalues
)

print(results_df)

