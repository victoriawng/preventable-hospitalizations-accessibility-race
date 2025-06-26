# we want to do analysis on primary care physicians, uninsured rate, race demographics 
library(tidyverse)
library(readxl)

source("EDA_import.R")
view(trend_data)
view(national_data)
colnames(national_data)

clean_national_subset <- national_data |> 
  select(
    `State FIPS Code`,
    `State Abbreviation`,
    Name,
    `County FIPS Code`,
    `Flu Vaccinations raw value`,
    `Flu Vaccinations (AIAN)`,
    `Flu Vaccinations (Asian/Pacific Islander)`,
    `Flu Vaccinations (Black)`,
    `Flu Vaccinations (Hispanic)`,
    `Flu Vaccinations (White)`,
    `Primary Care Physicians raw value`,
    `Ratio of population to primary care physicians.`,
    `Mental Health Providers raw value`,
    `Ratio of population to mental health providers.`,
    `Dentists raw value`,
    `Ratio of population to dentists.`,
    `Preventable Hospital Stays raw value`,
    `Preventable Hospital Stays (AIAN)`,
    `Preventable Hospital Stays (Asian/Pacific Islander)`,
    `Preventable Hospital Stays (Black)`,
    `Preventable Hospital Stays (Hispanic)`,
    `Preventable Hospital Stays (White)`,
    `Mammography Screening raw value`,
    `Mammography Screening (AIAN)`,
    `Mammography Screening (Asian/Pacific Islander)`,
    `Mammography Screening (Black)`,
    `Mammography Screening (Hispanic)`,
    `Mammography Screening (White)`,
    `Uninsured raw value`,
    `Uninsured Adults raw value`,
    `Severe Housing Problems raw value`,
    `Percentage of households with overcrowding`,
    `Percentage of households with high housing costs`,
    `Broadband Access raw value`,
    `Library Access raw value`,
    `Some College raw value`,
    `High School Completion raw value`,
    `Unemployment raw value`,
    `Income Inequality raw value`,
    `Children in Poverty raw value`,
    `Children in Poverty (AIAN)`,
    `Children in Poverty (White)`,
    `Children in Poverty (Black)`,
    `Children in Poverty (Hispanic)`,
    `Children in Poverty (Asian/Pacific Islander)`,
    `Child Care Cost Burden raw value`,
    `Child Care Centers raw value`,
    `Frequent Physical Distress raw value`,
    `Frequent Mental Distress raw value`,
    `Limited Access to Healthy Foods raw value`,
    `Food Insecurity raw value`,
    `Insufficient Sleep raw value`,
    `Teen Births raw value`,
    `Teen Births (AIAN)`,
    `Teen Births (Black)`,
    `Teen Births (NHOPI)`,
    `Teen Births (White)`,
    `Teen Births (Asian)`,
    `Teen Births (Hispanic)`,
    `Teen Births (Two or more races)`,
    `Excessive Drinking raw value`,
    `Adult Smoking raw value`,
    `Adult Obesity raw value`,
    `Physical Inactivity raw value`,
    `Uninsured Children raw value`,
    `Other Primary Care Providers raw value`,
    `Ratio of population to primary care providers other than physicians.`,
    `Traffic Volume raw value`,
    `Homeownership raw value`,
    `Severe Housing Cost Burden raw value`,
    `Access to Parks raw value`,
    `Census Participation raw value`,
    `High School Graduation raw value`,
    `Reading Scores raw value`,
    `Reading Scores (AIAN)`,
    `Reading Scores (White)`,
    `Reading Scores (Black)`,
    `Reading Scores (Hispanic)`,
    `Reading Scores (Asian/Pacific Islander)`,
    `Math Scores raw value`,
    `Math Scores (AIAN)`,
    `Math Scores (Black)`,
    `Math Scores (Asian/Pacific Islander)`,
    `Math Scores (Hispanic)`,
    `Math Scores (White)`,
    `School Segregation raw value`,
    `School Funding Adequacy raw value`,
    `Children Eligible for Free or Reduced Price Lunch raw value`,
    `Median Household Income raw value`,
    `Median Household Income (AIAN)`,
    `Median Household Income (Black)`,
    `Median Household Income (White)`,
    `Median household income (Asian)`,
    `Median Household Income (Hispanic)`,
    `Living Wage raw value`,
    `Residential Segregation - Black/White raw value`,
    `Disconnected Youth raw value`,
    `Lack of Social and Emotional Support raw value`,
    `Population raw value`,
    `Children in Single-Parent Households raw value`)


clean_national_subset <- clean_national_subset |> 
  filter(`State FIPS Code` != 'statecode')

colnames(clean_national_subset)

library(janitor)
library(skimr)

dim(clean_national_subset)
skim(clean_national_subset)

#lets look at varibles with low na values
library(dplyr)

column_complete_rate <- sapply(clean_national_subset, function(x) mean(!is.na(x)))

cols_to_keep <- names(column_complete_rate[column_complete_rate >= 0.90])

non_missing_data <- clean_national_subset %>%
  select(all_of(cols_to_keep))

view(non_missing_data)
colnames(non_missing_data)

# simple visualization with non na 

non_missing_data <- non_missing_data |>
  clean_names()

numeric_data <- non_missing_data %>%
  mutate(across(
    where(is.character) & !c(state_abbreviation, name), 
    readr::parse_number
  ))

colnames(numeric_data)
# plots 
ggplot(numeric_data, aes(x = ratio_of_population_to_primary_care_physicians, y = preventable_hospital_stays_raw_value)) +
  geom_point(color = "red", alpha = 0.3) +
  labs(
    title = "Ratio Physicians vs. Preventable Hospital Stays",
    x = "Ration",
    y = "Preventable Hospital Stays per 100,000"
  ) +
  theme_light()

ggplot(numeric_data, aes(x = uninsured_raw_value, y = preventable_hospital_stays_raw_value)) +
  geom_point(color = "blue", alpha = 0.3) +
  labs(
    title = "Uninsured Rate vs. Preventable Hospital Stays",
    x = "Uninsured Rate",
    y = "Preventable Hospital Stays per 100,000"
  ) +
  theme_light()

ggplot(numeric_data, aes(x =ratio_of_population_to_dentists, y = preventable_hospital_stays_raw_value)) +
  geom_point(color = "green", alpha = 0.3) +
  labs(
    title = "Dentist Count vs. Preventable Hospital Stays",
    x = "Denstists Count",
    y = "Preventable Hospital Stays per 100,000"
  ) +
  theme_light()
ggplot(numeric_data, aes(x = ratio_of_population_to_mental_health_providers, y = preventable_hospital_stays_raw_value)) +
  geom_point(color = "green", alpha = 0.3) +
  labs(
    title = "ratio_of_population_to_mental_health_providers vs. Preventable Hospital Stays",
    x = "ratio_of_population_to_mental_health_providers",
    y = "Preventable Hospital Stays per 100,000"
  ) +
  theme_light()
ggplot(numeric_data, aes(x = ratio_of_population_to_primary_care_providers_other_than_physicians, y = preventable_hospital_stays_raw_value)) +
  geom_point(color = "green", alpha = 0.3) +
  labs(
    title = "ratio_of_population_to_primary_care_providers_other_than_physicians vs. Preventable Hospital Stays",
    x = "Flu vax",
    y = "Preventable Hospital Stays per 100,000"
  ) +
  theme_light()

# these looks like the biggest corr at first glance 
#ratio_of_population_to_primary_care_providers_other_than_physicians
#ratio_of_population_to_mental_health_providers
#ratio_of_population_to_dentists
#uninsured_raw_value # maybe try to conver into ratio using the n/d
#ratio_of_population_to_primary_care_physicians
#telehealth (broadband)
 

ggplot(numeric_data, aes(x = broadband_access_raw_value, y = preventable_hospital_stays_raw_value)) +
  geom_point(color = "green", alpha = 0.3) +
  geom_smooth(method = "lm")
  labs(
    title = "broadband_access_raw_values vs. Preventable Hospital Stays",
    x = "broadband_access_raw_value",
    y = "Preventable Hospital Stays per 100,000"
  ) +
  theme_light()
  
ggplot(numeric_data, aes(x = high_school_completion_raw_value, y = preventable_hospital_stays_raw_value)) +
    geom_point(color = "green", alpha = 0.3) +
    geom_smooth(method = "lm")
  labs(
    title = "high_school_completion_raw_value vs. Preventable Hospital Stays",
    x = "high_school_completion_raw_value",
    y = "Preventable Hospital Stays per 100,000"
  ) +
    theme_light()
  
  
ggplot(numeric_data, aes(x = homeownership_raw_value, y = preventable_hospital_stays_raw_value)) +
    geom_point(color = "green", alpha = 0.3) +
    geom_smooth(method = "lm")
  labs(
    title = "high_school_completion_raw_value vs. Preventable Hospital Stays",
    x = "high_school_completion_raw_value",
    y = "Preventable Hospital Stays per 100,000"
  ) +
    theme_light()
  
ggplot(numeric_data, aes(x = insufficient_sleep_raw_value, y = preventable_hospital_stays_raw_value)) +
    geom_point(color = "green", alpha = 0.3) +
    geom_smooth(method = "lm")
  labs(
    title = "high_school_completion_raw_value vs. Preventable Hospital Stays",
    x = "high_school_completion_raw_value",
    y = "Preventable Hospital Stays per 100,000"
  ) +
    theme_light()
  
  
skim(non_missing_data)
  
  
