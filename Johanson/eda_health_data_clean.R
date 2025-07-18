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
library(sf)
library(randomForest)

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

#US County Map

#Run first
pal <- colorNumeric(
  palette = "magma",
  domain = map_data$preventable_hospital_stays_raw_value,
  na.color = "gray90"
)

# Load state shapefile
states_sf <- states(cb = TRUE, year = 2023, class = "sf")

# Exclude Alaska (02) and Hawaii (15)
map_data_contig <- map_data %>%
  filter(!STATEFP %in% c("02", "15"))

states_sf_contig <- states_sf %>%
  filter(!STATEFP %in% c("02", "15"))

#PLOT
ggplot() +
  geom_sf(data = map_data_contig, aes(fill = preventable_hospital_stays_raw_value), color = NA) +
  geom_sf(data = states_sf_contig, fill = NA, color = "white", size = 0.15) +
  scale_fill_viridis_c(option = "magma", na.value = "gray90", name = "Preventable\nHospital Stays") +
  coord_sf(
    crs = st_crs(2163),
    xlim = c(-2.4e6, 2.5e6),
    ylim = c(-2.2e6, 1.5e6),
    expand = FALSE
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(0, 0, 0, 0),
    legend.position = "right"
  ) +
  labs(
    caption = "Source: County Health Rankings, 2023"
  )


#Race Data



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
  select(state_abbreviation, name, population_raw_value, preventable_hospital_stays_raw_value,
         uninsured_raw_value, dentists_raw_value, other_primary_care_providers_raw_value,
         broadband_access_raw_value, mental_health_providers_raw_value,
         primary_care_physicians_raw_value, mammography_screening_raw_value,
         severe_housing_problems_raw_value, high_school_completion_raw_value,
         percent_disability_functional_limitations_raw_value, percent_not_proficient_in_english_raw_value,
         percent_rural_raw_value, racial_makeup, income_inequality_raw_value, median_household_income_raw_value,
         percent_american_indian_or_alaska_native_raw_value,
         percent_asian_raw_value, percent_hispanic_raw_value, percent_native_hawaiian_or_other_pacific_islander_raw_value,
         percent_non_hispanic_black_raw_value, percent_non_hispanic_white_raw_value)

predict_cn_subset <- predict_cn_subset |> 
  mutate(uninsured_raw_value_pct = uninsured_raw_value * 100)


predict_cn_subset_ratios <- cn_counties_national_subset |> #RUN THIS
  select(state_abbreviation, name, population_raw_value, ratio_of_population_to_primary_care_physicians, preventable_hospital_stays_raw_value,
         uninsured_raw_value, ratio_of_population_to_mental_health_providers, ratio_of_population_to_primary_care_providers_other_than_physicians,
         broadband_access_raw_value, ratio_of_population_to_dentists,
         primary_care_physicians_raw_value, mammography_screening_raw_value,
         severe_housing_problems_raw_value, high_school_completion_raw_value,
         percent_disability_functional_limitations_raw_value, percent_not_proficient_in_english_raw_value,
         percent_rural_raw_value, racial_makeup, income_inequality_raw_value, median_household_income_raw_value,
         population_raw_value, preventable_hospital_stays_raw_value, percent_american_indian_or_alaska_native_raw_value,
         percent_asian_raw_value, percent_hispanic_raw_value, percent_native_hawaiian_or_other_pacific_islander_raw_value,
         percent_non_hispanic_black_raw_value, percent_non_hispanic_white_raw_value)

predict_cn_subset_ratios <- predict_cn_subset_ratios |> 
  mutate(uninsured_raw_value_pct = uninsured_raw_value * 100)

#Negative Binomial + Random Effects (DONT RUN)
nb_model <- glmmTMB(
  preventable_hospital_stays_raw_value ~ uninsured_raw_value_pct + dentists_raw_value 
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
  preventable_hospital_stays_raw_value ~ uninsured_raw_value_pct + ratio_of_population_to_dentists 
  + ratio_of_population_to_primary_care_providers_other_than_physicians
  + ratio_of_population_to_mental_health_providers + ratio_of_population_to_primary_care_physicians
  + mammography_screening_raw_value + severe_housing_problems_raw_value
  + high_school_completion_raw_value + percent_disability_functional_limitations_raw_value
  + percent_rural_raw_value + income_inequality_raw_value
  + (1 | racial_makeup),
  family = nbinom2,
  data = predict_cn_subset_ratios
)



summary(nb_model_ratios) #RUN THIS


nb_model_ratios_black <- glmmTMB(
  preventable_hospital_stays_raw_value ~
    uninsured_raw_value_pct * percent_non_hispanic_black_raw_value +        # interaction term example
    ratio_of_population_to_dentists * percent_non_hispanic_black_raw_value +
    ratio_of_population_to_primary_care_providers_other_than_physicians * percent_non_hispanic_black_raw_value +
    ratio_of_population_to_mental_health_providers * percent_non_hispanic_black_raw_value +
    ratio_of_population_to_primary_care_physicians * percent_non_hispanic_black_raw_value +
    mammography_screening_raw_value * percent_non_hispanic_black_raw_value +
    severe_housing_problems_raw_value * percent_non_hispanic_black_raw_value +
    high_school_completion_raw_value * percent_non_hispanic_black_raw_value +
    percent_disability_functional_limitations_raw_value * percent_non_hispanic_black_raw_value +
    percent_rural_raw_value * percent_non_hispanic_black_raw_value +
    income_inequality_raw_value * percent_non_hispanic_black_raw_value,
  family = nbinom2,
  data = predict_cn_subset_ratios
)

summary(nb_model_ratios_black)



#MORE RACE DATA

stays_by_race <- cn_counties_national_subset |> 
  select(state_abbreviation, name, population_raw_value, preventable_hospital_stays_raw_value, percent_american_indian_or_alaska_native_raw_value,
         percent_asian_raw_value, percent_hispanic_raw_value, percent_native_hawaiian_or_other_pacific_islander_raw_value,
         percent_non_hispanic_black_raw_value, percent_non_hispanic_white_raw_value)


library(ggplot2)

# Example: Hispanic
ggplot(predict_cn_subset_ratios, aes(x = percent_hispanic_raw_value, y = uninsured_raw_value*100)) +
  geom_point(alpha=0.5) +
  labs(x = "Percent Hispanic", y = "Uninsured Rate (%)", title = "Uninsured Rate vs Percent Hispanic by County") +
  geom_smooth(method="lm")




summary_by_race <- predict_cn_subset_ratios |> 
  group_by(racial_makeup) |> 
  summarise(
    mean_uninsured_rate = weighted.mean(uninsured_raw_value, population_raw_value, na.rm = TRUE),
    mean_preventable_hosp = weighted.mean(preventable_hospital_stays_raw_value, population_raw_value, na.rm = TRUE),
    n_counties = n()
  )

print(summary_by_race)



library(ggplot2)

#Uninsured
ggplot(predict_cn_subset_ratios, aes(
  x = uninsured_raw_value * 100, 
  y = preventable_hospital_stays_raw_value, 
  color = racial_makeup)) +
  geom_point(alpha = 0.2) +
  labs(
    x = "Uninsured Rate (%)", 
    y = "Preventable Hospital Stays (per 100,000)", 
    color = "Dominant Race",
    title = "Uninsured Rate vs Preventable Hospitalizations by Racial Group"
  ) +
  geom_smooth(method = "lm", se = FALSE)

#High school completion
ggplot(predict_cn_subset_ratios, aes(
  x = high_school_completion_raw_value * 100, 
  y = preventable_hospital_stays_raw_value, 
  color = racial_makeup)) +
  geom_point(alpha = 0.2) +
  labs(
    x = "High School Completion (%)", 
    y = "Preventable Hospital Stays (per 100,000)", 
    color = "Dominant Race",
    title = "High School Completion vs Preventable Hospitalizations by Racial Group"
  ) +
  geom_smooth(method = "lm", se = FALSE)

#RANDOM FOREST

# install.packages("randomForest")   # Run if not already installed
library(randomForest)

rf_subset <- predict_cn_subset |> 
  select(primary_care_physicians_raw_value,
         uninsured_raw_value, mental_health_providers_raw_value, dentists_raw_value,
         broadband_access_raw_value, other_primary_care_providers_raw_value, mammography_screening_raw_value,
         severe_housing_problems_raw_value, high_school_completion_raw_value,
         percent_disability_functional_limitations_raw_value, percent_not_proficient_in_english_raw_value,
         percent_rural_raw_value, racial_makeup, income_inequality_raw_value, median_household_income_raw_value,
         population_raw_value, preventable_hospital_stays_raw_value, percent_american_indian_or_alaska_native_raw_value,
         percent_asian_raw_value, percent_hispanic_raw_value, percent_native_hawaiian_or_other_pacific_islander_raw_value,
         percent_non_hispanic_black_raw_value, percent_non_hispanic_white_raw_value)

rf_subset <- na.omit(rf_subset)

rfnr_subset <- rf_subset |>
  select(
    preventable_hospital_stays_raw_value,
    primary_care_physicians_raw_value,
    uninsured_raw_value,
    mental_health_providers_raw_value,
    other_primary_care_providers_raw_value,
    broadband_access_raw_value,
    dentists_raw_value,
    mammography_screening_raw_value,
    severe_housing_problems_raw_value,
    high_school_completion_raw_value,
    percent_disability_functional_limitations_raw_value,
    percent_not_proficient_in_english_raw_value,
    percent_rural_raw_value
  )



set.seed(42)
# Replace 'your_outcome' and 'your_dataframe' with your actual variable/data set names:
rfnr_model <- randomForest(
  preventable_hospital_stays_raw_value ~ .,
  data = rfnr_subset,
  importance = TRUE,
  ntree = 500
)

# Plot variable importance
varImpPlot(rfnr_model)

# View variable importance as a table
importance(rf_model)


count_by_group <- predict_cn_subset |> 
  count(racial_makeup, name = "n_counties") |> 
  arrange(desc(n_counties))

print(count_by_group)


#SUBSET COUNTIES BY RACIAL MAKEUP
wh_counties <- predict_cn_subset |> 
  filter(racial_makeup == "non_hispanic_white, hispanic")

wb_counties <- predict_cn_subset |> 
  filter(racial_makeup == "non_hispanic_white, non_hispanic_black")

waian_counties <- predict_cn_subset |> 
  filter(racial_makeup == "non_hispanic_white, american_indian_or_alaska_native")

hw_counties <- predict_cn_subset |> 
  filter(racial_makeup == "hispanic, non_hispanic_white")

bw_counties <- predict_cn_subset |> 
  filter(racial_makeup == "non_hispanic_black, non_hispanic_white")



rf_subset_wh <- wh_counties |> 
  select(primary_care_physicians_raw_value,
         uninsured_raw_value, mental_health_providers_raw_value, dentists_raw_value,
         broadband_access_raw_value, other_primary_care_providers_raw_value, mammography_screening_raw_value,
         severe_housing_problems_raw_value, high_school_completion_raw_value,
         percent_disability_functional_limitations_raw_value, percent_not_proficient_in_english_raw_value,
         percent_rural_raw_value, income_inequality_raw_value, median_household_income_raw_value,
         preventable_hospital_stays_raw_value)

rf_subset_wh <- na.omit(rf_subset_wh)

rf_subset_wb <- wb_counties |> 
  select(primary_care_physicians_raw_value,
         uninsured_raw_value, mental_health_providers_raw_value, dentists_raw_value,
         broadband_access_raw_value, other_primary_care_providers_raw_value, mammography_screening_raw_value,
         severe_housing_problems_raw_value, high_school_completion_raw_value,
         percent_disability_functional_limitations_raw_value, percent_not_proficient_in_english_raw_value,
         percent_rural_raw_value, income_inequality_raw_value, median_household_income_raw_value,
         preventable_hospital_stays_raw_value)

rf_subset_wb <- na.omit(rf_subset_wb)

rf_subset_waian <- waian_counties |> 
  select(primary_care_physicians_raw_value,
         uninsured_raw_value, mental_health_providers_raw_value, dentists_raw_value,
         broadband_access_raw_value, other_primary_care_providers_raw_value, mammography_screening_raw_value,
         severe_housing_problems_raw_value, high_school_completion_raw_value,
         percent_disability_functional_limitations_raw_value, percent_not_proficient_in_english_raw_value,
         percent_rural_raw_value, income_inequality_raw_value, median_household_income_raw_value,
         preventable_hospital_stays_raw_value)

rf_subset_waian <- na.omit(rf_subset_waian)

rf_subset_hw <- hw_counties |> 
  select(primary_care_physicians_raw_value,
         uninsured_raw_value, mental_health_providers_raw_value, dentists_raw_value,
         broadband_access_raw_value, other_primary_care_providers_raw_value, mammography_screening_raw_value,
         severe_housing_problems_raw_value, high_school_completion_raw_value,
         percent_disability_functional_limitations_raw_value, percent_not_proficient_in_english_raw_value,
         percent_rural_raw_value, income_inequality_raw_value, median_household_income_raw_value,
         preventable_hospital_stays_raw_value)

rf_subset_hw <- na.omit(rf_subset_hw)

rf_subset_bw <- bw_counties |> 
  select(primary_care_physicians_raw_value,
         uninsured_raw_value, mental_health_providers_raw_value, dentists_raw_value,
         broadband_access_raw_value, other_primary_care_providers_raw_value, mammography_screening_raw_value,
         severe_housing_problems_raw_value, high_school_completion_raw_value,
         percent_disability_functional_limitations_raw_value, percent_not_proficient_in_english_raw_value,
         percent_rural_raw_value, income_inequality_raw_value, median_household_income_raw_value,
         preventable_hospital_stays_raw_value)

rf_subset_bw <- na.omit(rf_subset_bw)


set.seed(42)
rf_model_wh <- randomForest(
  preventable_hospital_stays_raw_value ~ .,
  data = rf_subset_wh,
  importance = TRUE,
  ntree = 500
)

#PLOT
varImpPlot(rf_model_wh)

#TABLE
importance(rf_model_bw)

