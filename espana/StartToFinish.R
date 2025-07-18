library(tidyverse)
library(janitor)
source("EDA_import.R")

# Cleaning variables names
data <- national_data |>
  clean_names()

# Selecting variables of interest
# clincial care and socialeconomic 
# creating variables
data_clean <- data |>
  dplyr::select(state_fips_code, county_fips_code, x5_digit_fips_code, 
         state_abbreviation, name, primary_care_physicians_raw_value, ratio_of_population_to_primary_care_physicians,
         mental_health_providers_raw_value, ratio_of_population_to_mental_health_providers,
         dentists_raw_value, ratio_of_population_to_dentists, preventable_hospital_stays_raw_value,
         preventable_hospital_stays_aian, preventable_hospital_stays_asian_pacific_islander, preventable_hospital_stays_black, 
         preventable_hospital_stays_hispanic, preventable_hospital_stays_white, mammography_screening_raw_value, 
         mammography_screening_aian, mammography_screening_asian_pacific_islander, mammography_screening_black, 
         mammography_screening_black, mammography_screening_hispanic, mammography_screening_white, 
         uninsured_raw_value, uninsured_adults_raw_value, uninsured_children_raw_value, high_school_completion_raw_value, 
         unemployment_raw_value, income_inequality_raw_value, other_primary_care_providers_raw_value, high_school_graduation_raw_value,
         percent_american_indian_or_alaska_native_raw_value, percent_asian_raw_value, percent_hispanic_raw_value,
         percent_native_hawaiian_or_other_pacific_islander_raw_value, percent_non_hispanic_black_raw_value, percent_non_hispanic_white_raw_value, percent_not_proficient_in_english_raw_value,
         percent_rural_raw_value, population_raw_value) |>
  mutate(across(
    .cols = -c(state_fips_code, county_fips_code, x5_digit_fips_code, state_abbreviation, name),
    .fns = ~ as.numeric(as.character(.x)) 
  )) |>
  mutate(
    pop_aian = (percent_american_indian_or_alaska_native_raw_value / 100) * population_raw_value,
    pop_asian_pacific = (percent_asian_raw_value / 100 + percent_native_hawaiian_or_other_pacific_islander_raw_value / 100) * population_raw_value,
    pop_black = (percent_non_hispanic_black_raw_value / 100) * population_raw_value,
    pop_hispanic = (percent_hispanic_raw_value / 100) * population_raw_value,
    pop_white = (percent_non_hispanic_white_raw_value / 100) * population_raw_value
  ) |>
  mutate(
    physician_supply_per_10k = 10000 / ratio_of_population_to_primary_care_physicians,
    mental_health_providers_per_10k = 10000 / ratio_of_population_to_mental_health_providers,
    dentists_per_10k = 10000 / ratio_of_population_to_dentists
  ) |>
  rename(
    income_inequality = income_inequality_raw_value,
    unemployment_rate = unemployment_raw_value
  ) 
data_clean <- data_clean[-1, ]
  
glimpse(data_clean)

# dealing with missing data
na_percent <- sapply(data_clean, function(x) { 
  round(mean(is.na(x)) * 100, 1)  
})
na_percent[na_percent > 0]

# combine all poc data due to high missingness
data_clean <- data_clean |>
  #dplyr::select(-high_school_graduation_raw_value) |>
  mutate(
    # Sum all POC hospital stays
    preventable_hospital_stays_poc = preventable_hospital_stays_raw_value - preventable_hospital_stays_white,
    
    # Weighted average for POC mammography screening
    mammography_screening_poc = case_when(
      pop_white < population_raw_value ~  # If POC population exists
        (mammography_screening_raw_value * population_raw_value - 
           mammography_screening_white * pop_white) / 
        (population_raw_value - pop_white),
      TRUE ~ NA_real_
    ),
    
    # Total POC population
    pop_poc = population_raw_value - pop_white
  ) |>
  # Drop original race-specific columns
  dplyr::select(-c(
    preventable_hospital_stays_aian,
    preventable_hospital_stays_asian_pacific_islander,
    preventable_hospital_stays_black,
    preventable_hospital_stays_hispanic,
    mammography_screening_aian,
    mammography_screening_asian_pacific_islander,
    mammography_screening_black,
    mammography_screening_hispanic,
    pop_aian,
    pop_asian_pacific,
    pop_black,
    pop_hispanic
  ))

# Now drop all remaining NA values
data_clean <- data_clean |>
  drop_na()
glimpse(data_clean)

# Create analysis-ready dataset
analysis_data <- data_clean %>%
  dplyr::select(
    # Identifiers
    state_fips_code, county_fips_code, name,
    # Outcomes
    preventable_hospital_stays_white, preventable_hospital_stays_poc,
    mammography_screening_white, mammography_screening_poc,
    # Healthcare access
    physician_supply_per_10k, mental_health_providers_per_10k, 
    dentists_per_10k, uninsured_raw_value,
    # Controls
    income_inequality, unemployment_rate, 
    percent_rural_raw_value, high_school_completion_raw_value,
    # Population offsets
    pop_white, pop_poc, population_raw_value
  ) %>%
  # Convert percentages to proportions
  mutate(across(c(uninsured_raw_value, percent_rural_raw_value, 
                  high_school_completion_raw_value),
                ~ ./100))


# eda
library(ggplot2)
# Healthcare access distribution
ggplot(analysis_data, aes(x = physician_supply_per_10k)) +
  geom_histogram(bins = 30) +
  labs(title = "Distribution of Primary Care Physicians per 10k")

# Outcome variables by race
analysis_data %>%
  dplyr::select(contains("preventable"), contains("mammography")) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = value, fill = name)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~name, scales = "free", ncol = 2) +
  theme(legend.position = "none")

# model

# multicollinaarity 
library(car)

# Build a linear model to test VIFs
vif_model <- lm(preventable_hospital_stays_white ~ 
                  physician_supply_per_10k + 
                  uninsured_raw_value +
                  mental_health_providers_per_10k +
                  dentists_per_10k +
                  income_inequality +
                  unemployment_rate +
                  percent_rural_raw_value +
                  high_school_completion_raw_value,
                data = analysis_data)

vif(vif_model)

library(MASS)

nb_white <- glm.nb(preventable_hospital_stays_white ~ 
                     physician_supply_per_10k +
                     uninsured_raw_value +
                     income_inequality +
                     unemployment_rate +
                     percentrural_raw_value +
                     high_school_completion_raw_value +
                     physician_supply_per_10k:uninsured_raw_value,  # interaction example
                   offset = log(pop_white),
                   data = analysis_data)

summary(nb_white)

# 
