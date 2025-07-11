source("../EDA_import.R")


#Run at start
library(tidyverse)
library(naniar)
library(janitor)
library(VIM)
library(stats)
library(stringr)

national_data <- read_csv('/Users/chato/Desktop/cmu sure/capstone/preventable-hospitalizations-accessibility-racedata/analytic_data2025_v2.csv')

#Clean names
clean_names_national_data <- national_data |> 
  clean_names()

#Subset variables of interest
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

##CLEAN DATA SET
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

#Replace NA values with 0 (DON'T RUN IF WANT TO USE MISSING VALUE PLOTS)
clean_names_national_subset_counties[is.na(clean_names_national_subset_counties)] <- 0


##TO VIEW MISSING VALUES
vars <- colnames(clean_names_national_subset_counties)

var_groups <- split(vars, cut(seq_along(vars), 4, labels = FALSE))

for (i in 1:4) {
  
  
  data_subset <- clean_names_national_subset_counties[, var_groups[[i]]]
  
  
  print(gg_miss_var(data_subset) + ggtitle(paste("Missingness - Group", i)))
}


#Scatter plot variable against preventable hospital stays
clean_names_national_subset_counties |>
  ggplot(aes(x = uninsured_raw_value,
             y = preventable_hospital_stays_raw_value + 1)) + 
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
library(tigris)
counties_sf <- counties(cb = TRUE, year = 2023, class = "sf")

library(dplyr)
library(stringr)
library(ggplot2)
library(viridis)
clean_names_national_subset_counties <- clean_names_national_subset_counties |> 
  mutate(
    state_fips_code = str_pad(state_fips_code, 2, pad = "0"),
    county_fips_code = str_pad(county_fips_code, 3, pad = "0"),
    fips = paste0(state_fips_code, county_fips_code)
  )

map_data <- counties_sf |> 
  left_join(clean_names_national_subset_counties, by = c("GEOID" = "fips"))

ggplot(map_data) +
  geom_sf(aes(fill = percent_hispanic_raw_value), color = NA) +
  scale_fill_viridis(option = "magma", na.value = "grey90") +
  theme_minimal() +
  labs(title = "County-Level Map",
       fill = "My Variable")


#US County Map
ggplot(map_data) +
  geom_sf(aes(fill = ratio_of_population_to_mental_health_providers), color = NA) +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50)) +  # Approximate bounds of the lower 48
  scale_fill_viridis_c(option = "magma", na.value = "grey90") +
  theme_minimal()

ggplot(map_data) +
  geom_sf(aes(fill = percent_non_hispanic_white_raw_value), color = NA) +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50)) +  # Approximate bounds of the lower 48
  scale_fill_viridis_c(option = "magma", na.value = "grey90") +
  theme_minimal()


#Correlations
with(clean_names_national_subset_counties,
     cor(ratio_of_population_to_primary_care_providers_other_than_physicians,
         preventable_hospital_stays_raw_value))

library(dplyr)
library(purrr)

cor_with_outcome <- function(df, outcome_var) {
  df %>%
    select(where(is.numeric)) %>%
    select(-all_of(outcome_var)) %>%
    map_dbl(~ cor(.x, df[[outcome_var]], use = "complete.obs")) %>%
    sort(decreasing = TRUE)
}

# Run it on your dataset:
cor_results <- cor_with_outcome(clean_names_national_subset_counties, "preventable_hospital_stays_raw_value")

# View results
view(cor_results)

view(clean_names_national_subset_counties |> 
       select(name, state_abbreviation, state_fips_code, percent_asian_raw_value, percent_hispanic_raw_value, percent_american_indian_or_alaska_native_raw_value,
              percent_non_hispanic_black_raw_value, percent_non_hispanic_white_raw_value, percent_native_hawaiian_or_other_pacific_islander_raw_value))




# art hy graphs 
#1 
library(ggplot2)
library(scales)

race_scatter_data <- clean_names_national_subset_counties |> 
  select(name, state_abbreviation, uninsured_raw_value, ratio_of_population_to_dentists,
         ratio_of_population_to_primary_care_providers_other_than_physicians,
         ratio_of_population_to_mental_health_providers,
         ratio_of_population_to_primary_care_physicians,
         broadband_access_raw_value,
         preventable_hospital_stays_white,
         preventable_hospital_stays_black,
         preventable_hospital_stays_hispanic,
         preventable_hospital_stays_asian_pacific_islander,
         preventable_hospital_stays_aian) |> 
  pivot_longer(cols = starts_with("preventable_hospital_stays_"),
               names_to = "race",
               values_to = "preventable_hosp_stays") |> 
  mutate(
    race = str_replace(race, "preventable_hospital_stays_", ""),
    race = str_to_title(race)
  )

view(race_scatter_data)
ggplot(race_scatter_data, aes(x = ratio_of_population_to_dentists, y = preventable_hosp_stays)) +
  geom_point(alpha = 0.4, size = 0.5, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1) +
  facet_wrap(~race, scales = "fixed", ncol = 3) + 
  labs(
    title = "Relationship Between ratio_of_population_to_dentists and Preventable Hospital Stays",
    x = "ratio_of_population_to_dentists",
    y = "Preventable Hospital Stays (per 100,000)"
  ) +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  theme_light() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5, margin = margin(b = 10)),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.2, "lines")
  )

ggplot(race_scatter_data, aes(x = ratio_of_population_to_dentists, y = preventable_hosp_stays)) +
  geom_point(alpha = 0.4, size = 0.5, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1) +
  facet_wrap(~race, scales = "fixed", ncol = 3) + 
  labs(
    title = "Relationship Between uninsured_raw_value and Preventable Hospital Stays",
    x = "uninsured_raw_value",
    y = "Preventable Hospital Stays (per 100,000)"
  ) +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  theme_light() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5, margin = margin(b = 10)),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.2, "lines")
  )


ggplot(race_scatter_data, aes(x = ratio_of_population_to_dentists, y = preventable_hosp_stays)) +
  geom_point(alpha = 0.4, size = 0.5, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1) +
  facet_wrap(~race, scales = "fixed", ncol = 3) + 
  labs(
    title = "Relationship Between ratio_of_population_to_primary_care_providers_other_than_physicians and Preventable Hospital Stays",
    x = "ratio_of_population_to_primary_care_providers_other_than_physicians",
    y = "Preventable Hospital Stays (per 100,000)"
  ) +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  theme_light() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5, margin = margin(b = 10)),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.2, "lines")
  )



ggplot(race_scatter_data, aes(x = ratio_of_population_to_dentists, y = preventable_hosp_stays)) +
  geom_point(alpha = 0.4, size = 0.5, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1) +
  facet_wrap(~race, scales = "fixed", ncol = 3) + 
  labs(
    title = "Relationship Between ratio_of_population_to_mental_health_providers and Preventable Hospital Stays",
    x = "ratio_of_population_to_mental_health_providers",
    y = "Preventable Hospital Stays (per 100,000)"
  ) +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  theme_light() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5, margin = margin(b = 10)),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.2, "lines")
  )
#2
# Map for Black population preventable stays
ggplot(map_data) +
  geom_sf(aes(fill = preventable_hospital_stays_black), color = NA) +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50)) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  theme_minimal() +
  labs(title = "Preventable Hospital Stays (Black Population)", fill = "Rate")


view(clean_names_national_data)





################# white
white_data <- national_data |>
  slice(-1) |>
  rename(
    white_pct = `% Non-Hispanic White raw value`,
    ratio_dentists = `Ratio of population to dentists.`,
    preventable_hosp_stays = `Preventable Hospital Stays raw value`,
    ratio_physicians = `Ratio of population to primary care physicians.`
  ) |>
  mutate(
    white_pct = as.numeric(white_pct),
    ratio_dentists = as.numeric(ratio_dentists),
    preventable_hosp_stays = as.numeric(preventable_hosp_stays),
    ratio_physicians = as.numeric(ratio_physicians),
    white_group = if_else(white_pct >= 0.5, "White Majority", "White Minority")
  )

# Filter clean data for plotting
plot_data <- white_data |>
  filter(
    !is.na(ratio_dentists),
    !is.na(preventable_hosp_stays),
    !is.na(white_group),
    !is.na(ratio_physicians)
  )

# Create plot
ggplot(plot_data, aes(x = ratio_dentists, y = preventable_hosp_stays)) +
  geom_point(alpha = 0.4, size = 0.5, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1) +
  facet_wrap(~white_group, scales = "fixed") +
  labs(
    title = "Population-to-Dentist Ratio vs Preventable Hospital Stays",
    subtitle = "Comparing White-Majority and White-Minority Counties",
    x = "Population-to-Dentist Ratio",
    y = "Preventable Hospital Stays (per 100,000)"
  ) +
  scale_y_continuous(labels = comma_format()) +
  scale_x_continuous(labels = comma_format()) +
  theme_light() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5, margin = margin(b = 10)),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.2, "lines")
  )

ggplot(plot_data, aes(x = ratio_physicians, y = preventable_hosp_stays)) +
  geom_point(alpha = 0.4, size = 0.5, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1) +
  facet_wrap(~white_group, scales = "fixed") +
  labs(
    title = "ratio_physicians vs Preventable Hospital Stays",
    subtitle = "Comparing White-Majority and White-Minority Counties",
    x = "ratio_physicians",
    y = "Preventable Hospital Stays (per 100,000)"
  ) +
  scale_y_continuous(labels = comma_format()) +
  scale_x_continuous(labels = comma_format()) +
  theme_light() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5, margin = margin(b = 10)),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.2, "lines")
  )



### intervative: #Interactive US County Map
library(leaflet)
library(tidyverse)
library(viridis)
library(tigris)
source("EDA_import.R")


#Run at start
library(tidyverse)
library(naniar)
library(janitor)
library(VIM)
library(stats)
library(stringr)
library(scales)
library(leaflet)

national_data <- read_csv('/Users/chato/Desktop/cmu sure/capstone/preventable-hospitalizations-accessibility-racedata/analytic_data2025_v2.csv')

#Clean names
clean_names_national_data <- national_data |> 
  clean_names()

#Subset variables of interest
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

##CLEAN DATA SET
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

#Replace NA values with 0 (DON'T RUN IF WANT TO USE MISSING VALUE PLOTS)
clean_names_national_subset_counties[is.na(clean_names_national_subset_counties)] <- 0


##TO VIEW MISSING VALUES
vars <- colnames(clean_names_national_subset_counties)

var_groups <- split(vars, cut(seq_along(vars), 4, labels = FALSE))

for (i in 1:4) {
  
  
  data_subset <- clean_names_national_subset_counties[, var_groups[[i]]]
  
  
  print(gg_miss_var(data_subset) + ggtitle(paste("Missingness - Group", i)))
}


#Scatter plot variable against preventable hospital stays
clean_names_national_subset_counties |>
  ggplot(aes(x = uninsured_raw_value,
             y = preventable_hospital_stays_raw_value + 1)) + 
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


clean_names_national_subset_counties <- clean_names_national_subset_counties |> 
  mutate(
    state_fips_code = str_pad(state_fips_code, 2, pad = "0"),
    county_fips_code = str_pad(county_fips_code, 3, pad = "0"),
    fips = paste0(state_fips_code, county_fips_code)
  )

map_data <- counties_sf |> 
  left_join(clean_names_national_subset_counties, by = c("GEOID" = "fips"))



#US County Map
ggplot(map_data) +
  geom_sf(aes(fill = preventable_hospital_stays_raw_value), color = NA) +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50)) +  
  scale_fill_viridis_c(option = "magma", na.value = "grey90") +
  theme_minimal()

#Interactive US County Map

#Run first
pal <- colorNumeric(
  palette = "magma",
  domain = map_data$preventable_hospital_stays_raw_value,
  na.color = "gray90"
)

#Leaflet map
map_inter <-leaflet(map_data) |> 
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


library(mapview)
library(webshot2)

# Save the leaflet object
mapshot(
  leaflet_object,  # replace with the name of your leaflet map
  file = "figures/preventable_map.png"
)
##
# --- LOAD LIBRARIES ---
library(tidyverse)
library(janitor)
library(stringr)
library(scales)
library(leaflet)
library(mapview)
library(webshot2)
library(tigris)
library(sf)

options(tigris_use_cache = TRUE)

# --- LOAD AND CLEAN DATA ---
national_data <- read_csv("data/analytic_data2025_v2.csv")

# Clean column names
clean_data <- national_data |> 
  clean_names()

# Filter to only county-level observations
county_data <- clean_data |> 
  filter(str_detect(name, "County|Municipality|Census|Borough|Region|District|Parish|city"))

# Convert selected columns to numeric (handle suppression)
char_cols <- c("state_abbreviation", "name")
cols_to_convert <- setdiff(names(county_data), char_cols)

county_data[cols_to_convert] <- lapply(
  county_data[cols_to_convert],
  function(x) {
    x <- as.character(x)
    x[x %in% c("NA", "", "N/A", "*", "Suppressed")] <- NA
    as.numeric(x)
  }
)

# --- ADD FIPS CODES ---
county_data <- county_data |> 
  mutate(
    state_fips_code = str_pad(state_fips_code, 2, pad = "0"),
    county_fips_code = str_pad(county_fips_code, 3, pad = "0"),
    fips = paste0(state_fips_code, county_fips_code)
  )

# --- LOAD SHAPEFILES FOR U.S. COUNTIES ---
counties_sf <- counties(cb = TRUE, year = 2023, class = "sf")

# --- JOIN SPATIAL DATA WITH HEALTH DATA ---
map_data <- counties_sf |> 
  left_join(county_data, by = c("GEOID" = "fips"))

# --- CREATE COLOR SCALE FOR PREVENTABLE STAYS ---
pal <- colorNumeric(
  palette = "magma",
  domain = map_data$preventable_hospital_stays_raw_value,
  na.color = "gray90"
)

# --- BUILD THE INTERACTIVE MAP ---
leaflet_map <- leaflet(map_data) |> 
  addProviderTiles("CartoDB.Positron") |>  
  addPolygons(
    fillColor = ~pal(preventable_hospital_stays_raw_value),
    weight = 0.2,
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

# --- SAVE MAP AS PNG ---
dir.create("figures", showWarnings = FALSE)

mapshot(leaflet_map, file = "figures/preventable_map.png")

