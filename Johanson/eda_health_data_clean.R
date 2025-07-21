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
library(biscale)
library(cowplot)

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
    income_inequality_raw_value,
    unemployment_raw_value
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



predict_cn_subset <- cn_counties_national_subset |> # RUN
  select(state_abbreviation, name, population_raw_value, preventable_hospital_stays_raw_value,
         uninsured_raw_value, dentists_raw_value, other_primary_care_providers_raw_value,
         broadband_access_raw_value, mental_health_providers_raw_value,
         primary_care_physicians_raw_value, mammography_screening_raw_value,
         severe_housing_problems_raw_value, high_school_completion_raw_value,
         unemployment_raw_value,
         percent_disability_functional_limitations_raw_value, percent_not_proficient_in_english_raw_value,
         percent_rural_raw_value, racial_makeup, income_inequality_raw_value, median_household_income_raw_value,
         percent_american_indian_or_alaska_native_raw_value,
         percent_asian_raw_value, percent_hispanic_raw_value, percent_native_hawaiian_or_other_pacific_islander_raw_value,
         percent_non_hispanic_black_raw_value, percent_non_hispanic_white_raw_value)

predict_cn_subset <- predict_cn_subset |> 
  mutate(uninsured_raw_value_pct = uninsured_raw_value * 100)



#RANDOM FOREST

rf_subset <- predict_cn_subset |> 
  select(primary_care_physicians_raw_value,
         uninsured_raw_value, mental_health_providers_raw_value, dentists_raw_value, 
         other_primary_care_providers_raw_value, mammography_screening_raw_value,
         severe_housing_problems_raw_value, high_school_completion_raw_value,
         percent_disability_functional_limitations_raw_value, percent_not_proficient_in_english_raw_value,
         percent_rural_raw_value, income_inequality_raw_value, unemployment_raw_value,
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
    dentists_raw_value,
    mammography_screening_raw_value,
    severe_housing_problems_raw_value,
    high_school_completion_raw_value,
    percent_disability_functional_limitations_raw_value,
    percent_not_proficient_in_english_raw_value,
    percent_rural_raw_value,
    unemployment_raw_value
  )

rfnr_subset <- na.omit(rfnr_subset)

#Random Forest for entire subset
set.seed(42)
rf_model <- randomForest(
  preventable_hospital_stays_raw_value ~ .,
  data = rf_subset,
  importance = TRUE,
  ntree = 500
)

varImpPlot(rf_model)

importance(rf_model)

imp <- importance(rf_model)
imp_df <- data.frame(Variable = rownames(imp), Importance = imp[,"%IncMSE"])

ggplot(imp_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col() +
  coord_flip() +
  labs(title = "Variable Importance (%IncMSE)", x = "Variable", y = "%IncMSE")

#Random Forest for entire subset (without race variables)
set.seed(42)
rfnr_model <- randomForest(
  preventable_hospital_stays_raw_value ~ .,
  data = rfnr_subset,
  importance = TRUE,
  ntree = 500
)

varImpPlot(rfnr_model)

importance(rfnr_model)

imp_nr <- importance(rfnr_model)
imp_df_nr <- data.frame(Variable = rownames(imp_nr), Importance = imp_nr[,"%IncMSE"])

ggplot(imp_df_nr, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col() +
  coord_flip() +
  labs(title = "Variable Importance (%IncMSE)", x = "Variable", y = "%IncMSE")

#Racial Makeup count of counties
count_by_group <- predict_cn_subset |> 
  count(racial_makeup, name = "n_counties") |> 
  arrange(desc(n_counties))

print(count_by_group)


#SUBSET COUNTIES BY RACIAL MAKEUP
w_counties <- predict_cn_subset |> 
  filter(racial_makeup == "non_hispanic_white")

h_counties <- predict_cn_subset |> 
  filter(racial_makeup == "hispanic")

b_counties <- predict_cn_subset |> 
  filter(racial_makeup == "non_hispanic_black")

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

aianw_counties <- predict_cn_subset |> 
  filter(racial_makeup == "american_indian_or_alaska_native, non_hispanic_white")

wa_counties <- predict_cn_subset |> 
  filter(racial_makeup == "non_hispanic_white, asian")

#White Majority
rf_subset_w <- w_counties |> 
  select(primary_care_physicians_raw_value,
         uninsured_raw_value, mental_health_providers_raw_value, dentists_raw_value, 
         other_primary_care_providers_raw_value, mammography_screening_raw_value,
         severe_housing_problems_raw_value, high_school_completion_raw_value,
         percent_disability_functional_limitations_raw_value, percent_not_proficient_in_english_raw_value,
         percent_rural_raw_value, income_inequality_raw_value,
         preventable_hospital_stays_raw_value, unemployment_raw_value)

rf_subset_w <- na.omit(rf_subset_w)

#Hispanic Majority
rf_subset_h <- h_counties |> 
  select(primary_care_physicians_raw_value,
         uninsured_raw_value, mental_health_providers_raw_value, dentists_raw_value, 
         other_primary_care_providers_raw_value, mammography_screening_raw_value,
         severe_housing_problems_raw_value, high_school_completion_raw_value,
         percent_disability_functional_limitations_raw_value, percent_not_proficient_in_english_raw_value,
         percent_rural_raw_value, income_inequality_raw_value,
         preventable_hospital_stays_raw_value, unemployment_raw_value)

rf_subset_h <- na.omit(rf_subset_h)

#Black Majority
rf_subset_b <- b_counties |> 
  select(primary_care_physicians_raw_value,
         uninsured_raw_value, mental_health_providers_raw_value, dentists_raw_value, 
         other_primary_care_providers_raw_value, mammography_screening_raw_value,
         severe_housing_problems_raw_value, high_school_completion_raw_value,
         percent_disability_functional_limitations_raw_value, percent_not_proficient_in_english_raw_value,
         percent_rural_raw_value, income_inequality_raw_value,
         preventable_hospital_stays_raw_value, unemployment_raw_value)

rf_subset_b <- na.omit(rf_subset_b)


#White and Hispanic Majority
rf_subset_wh <- wh_counties |> 
  select(primary_care_physicians_raw_value,
         uninsured_raw_value, mental_health_providers_raw_value, dentists_raw_value, 
         other_primary_care_providers_raw_value, mammography_screening_raw_value,
         severe_housing_problems_raw_value, high_school_completion_raw_value,
         percent_disability_functional_limitations_raw_value, percent_not_proficient_in_english_raw_value,
         percent_rural_raw_value, income_inequality_raw_value,
         preventable_hospital_stays_raw_value, unemployment_raw_value)

rf_subset_wh <- na.omit(rf_subset_wh)

#White and Black Majority
rf_subset_wb <- wb_counties |> 
  select(primary_care_physicians_raw_value,
         uninsured_raw_value, mental_health_providers_raw_value, dentists_raw_value, 
         other_primary_care_providers_raw_value, mammography_screening_raw_value,
         severe_housing_problems_raw_value, high_school_completion_raw_value,
         percent_disability_functional_limitations_raw_value, percent_not_proficient_in_english_raw_value,
         percent_rural_raw_value, income_inequality_raw_value,
         preventable_hospital_stays_raw_value, unemployment_raw_value)

rf_subset_wb <- na.omit(rf_subset_wb)

#White and AIAN Majority
rf_subset_waian <- waian_counties |> 
  select(primary_care_physicians_raw_value,
         uninsured_raw_value, mental_health_providers_raw_value, dentists_raw_value, 
         other_primary_care_providers_raw_value, mammography_screening_raw_value,
         severe_housing_problems_raw_value, high_school_completion_raw_value,
         percent_disability_functional_limitations_raw_value, percent_not_proficient_in_english_raw_value,
         percent_rural_raw_value, income_inequality_raw_value,
         preventable_hospital_stays_raw_value, unemployment_raw_value)

rf_subset_waian <- na.omit(rf_subset_waian)

#Hispanic and White Majority
rf_subset_hw <- hw_counties |> 
  select(primary_care_physicians_raw_value,
         uninsured_raw_value, mental_health_providers_raw_value, dentists_raw_value, 
         other_primary_care_providers_raw_value, mammography_screening_raw_value,
         severe_housing_problems_raw_value, high_school_completion_raw_value,
         percent_disability_functional_limitations_raw_value, percent_not_proficient_in_english_raw_value,
         percent_rural_raw_value, income_inequality_raw_value,
         preventable_hospital_stays_raw_value, unemployment_raw_value)

rf_subset_hw <- na.omit(rf_subset_hw)

#Black and White Majority
rf_subset_bw <- bw_counties |> 
  select(primary_care_physicians_raw_value,
         uninsured_raw_value, mental_health_providers_raw_value, dentists_raw_value, 
         other_primary_care_providers_raw_value, mammography_screening_raw_value,
         severe_housing_problems_raw_value, high_school_completion_raw_value,
         percent_disability_functional_limitations_raw_value, percent_not_proficient_in_english_raw_value,
         percent_rural_raw_value, income_inequality_raw_value,
         preventable_hospital_stays_raw_value, unemployment_raw_value)

rf_subset_bw <- na.omit(rf_subset_bw)

#AIAN and White Majority
rf_subset_aianw <- aianw_counties |> 
  select(primary_care_physicians_raw_value,
         uninsured_raw_value, mental_health_providers_raw_value, dentists_raw_value, 
         other_primary_care_providers_raw_value, mammography_screening_raw_value,
         severe_housing_problems_raw_value, high_school_completion_raw_value,
         percent_disability_functional_limitations_raw_value, percent_not_proficient_in_english_raw_value,
         percent_rural_raw_value, income_inequality_raw_value,
         preventable_hospital_stays_raw_value, unemployment_raw_value)

rf_subset_aianw <- na.omit(rf_subset_aianw)

#White and Asian Majority
rf_subset_wa <- wa_counties |> 
  select(primary_care_physicians_raw_value,
         uninsured_raw_value, mental_health_providers_raw_value, dentists_raw_value, 
         other_primary_care_providers_raw_value, mammography_screening_raw_value,
         severe_housing_problems_raw_value, high_school_completion_raw_value,
         percent_disability_functional_limitations_raw_value, percent_not_proficient_in_english_raw_value,
         percent_rural_raw_value, income_inequality_raw_value,
         preventable_hospital_stays_raw_value, unemployment_raw_value)

rf_subset_wa <- na.omit(rf_subset_wa)

#READABLE VARIABLE NAMES
# Variable name mapping
var_name_lookup <- c(
  "primary_care_physicians_raw_value" = "Primary Care Physicians",
  "uninsured_raw_value" = "Uninsured Rate",
  "mental_health_providers_raw_value" = "Mental Health Providers",
  "dentists_raw_value" = "Dentists",
  "other_primary_care_providers_raw_value" = "Other Primary Care Providers",
  "mammography_screening_raw_value" = "Mammography Screening",
  "severe_housing_problems_raw_value" = "Severe Housing Problems",
  "high_school_completion_raw_value" = "HS Completion Rate",
  "percent_disability_functional_limitations_raw_value" = "Disability Rate",
  "percent_not_proficient_in_english_raw_value" = "Limited English Proficiency",
  "percent_rural_raw_value" = "Rural Population (%)",
  "income_inequality_raw_value" = "Income Inequality",
  "preventable_hospital_stays_raw_value" = "Preventable Hospital Stays",
  "unemployment_raw_value" = "Unemployment Rate"
)

#Random Forest for White Counties
set.seed(42)
rf_model_w <- randomForest(
  preventable_hospital_stays_raw_value ~ .,
  data = rf_subset_w,
  importance = TRUE,
  ntree = 500
)

varImpPlot(rf_model_w)

importance(rf_model_w)

imp_w <- importance(rf_model_w)
imp_df_w <- data.frame(
  Variable = rownames(imp_w),
  Importance = imp_w[,"%IncMSE"]
) |> 
  mutate(Variable = str_replace_all(Variable, var_name_lookup)) |> 
  arrange(desc(Importance)) |> 
  slice_head(n = 5)

ggplot(imp_df_w, aes(x = reorder(Variable, Importance), y = Importance, fill = Variable)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Most Important Variables (White Majority Counties)",
    x = "Predictor",
    y = "%IncMSE"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14)+
  theme(panel.background = element_rect(fill = "white", color = NA), 
        plot.background = element_rect(fill = "white", color = NA))

#Random Forest for Hispanic Counties
set.seed(42)
rf_model_h <- randomForest(
  preventable_hospital_stays_raw_value ~ .,
  data = rf_subset_h,
  importance = TRUE,
  ntree = 500
)

varImpPlot(rf_model_h)

importance(rf_model_h)

imp_h <- importance(rf_model_h)
imp_df_h <- data.frame(
  Variable = rownames(imp_h),
  Importance = imp_h[,"%IncMSE"]
) |> 
  mutate(Variable = str_replace_all(Variable, var_name_lookup)) |> 
  arrange(desc(Importance)) |> 
  slice_head(n = 5)

ggplot(imp_df_h, aes(x = reorder(Variable, Importance), y = Importance, fill = Variable)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Most Important Variables (Hispanic Majority Counties)",
    x = "Predictor",
    y = "%IncMSE"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14)+
  theme(panel.background = element_rect(fill = "white", color = NA), 
        plot.background = element_rect(fill = "white", color = NA))

#Random Forest for Black Counties
set.seed(42)
rf_model_b <- randomForest(
  preventable_hospital_stays_raw_value ~ .,
  data = rf_subset_b,
  importance = TRUE,
  ntree = 500
)

varImpPlot(rf_model_b)

importance(rf_model_b)

imp_b <- importance(rf_model_b)
imp_df_b <- data.frame(
  Variable = rownames(imp_b),
  Importance = imp_b[,"%IncMSE"]
) |> 
  mutate(Variable = str_replace_all(Variable, var_name_lookup)) |> 
  arrange(desc(Importance)) |> 
  slice_head(n = 5)

ggplot(imp_df_b, aes(x = reorder(Variable, Importance), y = Importance, fill = Variable)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Most Important Variables (Black Majority Counties)",
    x = "Predictor",
    y = "%IncMSE"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14)+
  theme(panel.background = element_rect(fill = "white", color = NA), 
        plot.background = element_rect(fill = "white", color = NA))

#Random Forest for White, Hispanic Counties
set.seed(42)
rf_model_wh <- randomForest(
  preventable_hospital_stays_raw_value ~ .,
  data = rf_subset_wh,
  importance = TRUE,
  ntree = 500
)

varImpPlot(rf_model_wh)

importance(rf_model_wh)

imp_wh <- importance(rf_model_wh)
imp_df_wh <- data.frame(
  Variable = rownames(imp_wh),
  Importance = imp_wh[,"%IncMSE"]
) |> 
  mutate(Variable = str_replace_all(Variable, var_name_lookup)) |> 
  arrange(desc(Importance)) |> 
  slice_head(n = 5)

ggplot(imp_df_wh, aes(x = reorder(Variable, Importance), y = Importance, fill = Variable)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Most Important Variables (White-Hispanic Majority Counties)",
    x = "Predictor",
    y = "%IncMSE"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14)+
  theme(panel.background = element_rect(fill = "white", color = NA), 
        plot.background = element_rect(fill = "white", color = NA))

#Random Forest for White, Black Counties
set.seed(42)
rf_model_wb <- randomForest(
  preventable_hospital_stays_raw_value ~ .,
  data = rf_subset_wb,
  importance = TRUE,
  ntree = 500
)

varImpPlot(rf_model_wb)

importance(rf_model_wb)

imp_wb <- importance(rf_model_wb)
imp_df_wb <- data.frame(
  Variable = rownames(imp_wb),
  Importance = imp_wb[,"%IncMSE"]
) |> 
  mutate(Variable = str_replace_all(Variable, var_name_lookup)) |> 
  arrange(desc(Importance)) |> 
  slice_head(n = 5)

ggplot(imp_df_wb, aes(x = reorder(Variable, Importance), y = Importance, fill = Variable)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Most Important Variables (White-Black Majority Counties)",
    x = "Predictor",
    y = "%IncMSE"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14)+
  theme(panel.background = element_rect(fill = "white", color = NA), 
        plot.background = element_rect(fill = "white", color = NA))

#Random Forest for White, AIAN Counties
set.seed(42)
rf_model_waian <- randomForest(
  preventable_hospital_stays_raw_value ~ .,
  data = rf_subset_waian,
  importance = TRUE,
  ntree = 500
)

varImpPlot(rf_model_waian)

importance(rf_model_waian)

imp_waian <- importance(rf_model_waian)
imp_df_waian <- data.frame(
  Variable = rownames(imp_waian),
  Importance = imp_waian[,"%IncMSE"]
) |> 
  mutate(Variable = str_replace_all(Variable, var_name_lookup)) |> 
  arrange(desc(Importance)) |> 
  slice_head(n = 5)

ggplot(imp_df_waian, aes(x = reorder(Variable, Importance), y = Importance, fill = Variable)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Most Important Variables (White-AIAN Majority Counties)",
    x = "Predictor",
    y = "%IncMSE"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14)+
  theme(panel.background = element_rect(fill = "white", color = NA), 
        plot.background = element_rect(fill = "white", color = NA))

#Random Forest for Hispanic, White Counties
set.seed(42)
rf_model_hw <- randomForest(
  preventable_hospital_stays_raw_value ~ .,
  data = rf_subset_hw,
  importance = TRUE,
  ntree = 500
)

varImpPlot(rf_model_hw)

importance(rf_model_hw)

imp_hw <- importance(rf_model_hw)
imp_df_hw <- data.frame(
  Variable = rownames(imp_hw),
  Importance = imp_hw[,"%IncMSE"]
) |> 
  mutate(Variable = str_replace_all(Variable, var_name_lookup)) |> 
  arrange(desc(Importance)) |> 
  slice_head(n = 5)

ggplot(imp_df_hw, aes(x = reorder(Variable, Importance), y = Importance, fill = Variable)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Most Important Variables (Hispanic-White Majority Counties)",
    x = "Predictor",
    y = "%IncMSE"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14)+
  theme(panel.background = element_rect(fill = "white", color = NA), 
        plot.background = element_rect(fill = "white", color = NA))

#Random Forest for Black, White Counties
set.seed(42)
rf_model_bw <- randomForest(
  preventable_hospital_stays_raw_value ~ .,
  data = rf_subset_bw,
  importance = TRUE,
  ntree = 500
)

varImpPlot(rf_model_bw)

importance(rf_model_bw)

imp_bw <- importance(rf_model_bw)
imp_df_bw <- data.frame(
  Variable = rownames(imp_bw),
  Importance = imp_bw[,"%IncMSE"]
) |> 
  mutate(Variable = str_replace_all(Variable, var_name_lookup)) |> 
  arrange(desc(Importance)) |> 
  slice_head(n = 5)

ggplot(imp_df_bw, aes(x = reorder(Variable, Importance), y = Importance, fill = Variable)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Most Important Variables (Black-White Majority Counties)",
    x = "Predictor",
    y = "%IncMSE"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14)+
  theme(panel.background = element_rect(fill = "white", color = NA), 
        plot.background = element_rect(fill = "white", color = NA))

#Random Forest for AIAN, White Counties
set.seed(42)
rf_model_aianw <- randomForest(
  preventable_hospital_stays_raw_value ~ .,
  data = rf_subset_aianw,
  importance = TRUE,
  ntree = 500
)

varImpPlot(rf_model_aianw)

importance(rf_model_aianw)

imp_aianw <- importance(rf_model_aianw)
imp_df_aianw <- data.frame(
  Variable = rownames(imp_aianw),
  Importance = imp_aianw[,"%IncMSE"]
) |> 
  mutate(Variable = str_replace_all(Variable, var_name_lookup)) |> 
  arrange(desc(Importance)) |> 
  slice_head(n = 5)

ggplot(imp_df_aianw, aes(x = reorder(Variable, Importance), y = Importance, fill = Variable)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Most Important Variables (AIAN-White Majority Counties)",
    x = "Predictor",
    y = "%IncMSE"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14)+
  theme(panel.background = element_rect(fill = "white", color = NA), 
        plot.background = element_rect(fill = "white", color = NA))

#Random Forest for White, Asian Counties
set.seed(42)
rf_model_wa <- randomForest(
  preventable_hospital_stays_raw_value ~ .,
  data = rf_subset_wa,
  importance = TRUE,
  ntree = 500
)

varImpPlot(rf_model_wa)

importance(rf_model_wa)

imp_wa <- importance(rf_model_wa)
imp_df_wa <- data.frame(
  Variable = rownames(imp_wa),
  Importance = imp_wa[,"%IncMSE"]
) |> 
  mutate(Variable = str_replace_all(Variable, var_name_lookup)) |> 
  arrange(desc(Importance)) |> 
  slice_head(n = 5)

ggplot(imp_df_wa, aes(x = reorder(Variable, Importance), y = Importance, fill = Variable)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Most Important Variables (White-Asian Majority Counties)",
    x = "Predictor",
    y = "%IncMSE"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14)+
  theme(panel.background = element_rect(fill = "white", color = NA), 
        plot.background = element_rect(fill = "white", color = NA))

#Scatterplots with regression lines

predict_cn_subset <- na.omit(predict_cn_subset)

ggplot(data = predict_cn_subset, 
       aes(x = high_school_completion_raw_value, y = preventable_hospital_stays_raw_value)) +
  geom_point(alpha = 0.6) +                                
  geom_smooth(method = "lm", color = "red", se = TRUE) +  
  labs(
    x = "High School Completion Rate (%)",
    y = "Preventable Hospital Stays per 100,000",
    title = "High School Completion vs. Preventable Hospital Stays",
    caption = "Each point is a county"
  ) +
  theme_minimal()

ggplot(data = predict_cn_subset, 
       aes(x = percent_disability_functional_limitations_raw_value, y = preventable_hospital_stays_raw_value)) +
  geom_point(alpha = 0.6) +                                
  geom_smooth(method = "lm", color = "red", se = TRUE) +  
  labs(
    x = "Disability Functional Limitations Rate (%)",
    y = "Preventable Hospital Stays per 100,000",
    title = "Disability Functional Limitations vs. Preventable Hospital Stays",
    caption = "Each point is a county"
  ) +
  theme_minimal()


#Stacked plots
rf_subset_w$group <- "w"
rf_subset_wb$group <- "wb"
rf_subset_wh$group <- "wh"
rf_subset_bw$group <- "bw"
rf_subset_hw$group <- "hw"

combined_rf_subset <- rbind(
  rf_subset_w,
  rf_subset_wb,
  rf_subset_wh,
  rf_subset_bw,
  rf_subset_hw
)




#RACIAL GROUP SUBSET
combined_rf_subset <- combined_rf_subset |> 
  mutate(group = dplyr::recode(as.character(group),
                               "w" = "White",
                               "wb" = "White-Black",
                               "wh" = "White-Hispanic",
                               "bw" = "Black-White",
                               "hw" = "Hispanic-White"
  ))


#HIGHSCHOOL COMPLETION
ggplot(
  combined_rf_subset,
  aes(
    x = high_school_completion_raw_value,
    y = preventable_hospital_stays_raw_value,
    color = group
  )
) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, size = 1.2) +
  facet_wrap(~ group, nrow = 1) +
  scale_color_manual(
    values = c(
      "White" = "red", 
      "White-Black" = "blue", 
      "White-Hispanic" = "forestgreen", 
      "Black-White" = "orange", 
      "Hispanic-White" = "purple"
    )
  ) +
  scale_x_continuous(limits = c(0.7, 1.0)) +
  labs(
    x = "High School Completion Rate (%)",
    y = "Preventable Hospital Stays per 100,000",
    title = "High School Completion vs. Preventable Hospital Stays by Group",
    color = "Group"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(color = "white", face = "bold", size = 14),
    strip.background = element_rect(fill = "gray40"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

#UNINSURED
ggplot(
  combined_rf_subset,
  aes(
    x = uninsured_raw_value,
    y = preventable_hospital_stays_raw_value,
    color = group
  )
) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE, size = 1.2) +
  facet_wrap(~ group, nrow = 1) +
  scale_color_manual(
    values = c(
      "White-Hispanic" = "red", 
      "White-Black" = "blue", 
      "White-AIAN" = "forestgreen", 
      "Hispanic-White" = "orange", 
      "Black-White" = "purple"
    )
  ) +
  scale_x_continuous(limits = c(0.01, 0.4)) +
  labs(
    x = "Uninsured Rate (%)",
    y = "Preventable Hospital Stays per 100,000",
    title = "Uninsured vs. Preventable Hospital Stays by Group",
    color = "Group"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(color = "white", face = "bold", size = 14),
    strip.background = element_rect(fill = "gray40"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )


range(combined_rf_subset$uninsured_raw_value, na.rm = TRUE)


set.seed(123)
map_data <- map_data |> 
  mutate(
    mental_health_providers_jit = jitter(mental_health_providers_raw_value, amount = 0.01)
  )


map_data_bi <- bi_class(
  map_data,
  x = preventable_hospital_stays_raw_value,
  y = mental_health_providers_jit,
  style = "quantile",                           
)



# Replace 'state' with the actual column name if it's different
map_data_bi_continental <- map_data_bi |> 
  filter(!STATE_NAME %in% c("Hawaii", "Alaska"))


bimap <- ggplot() +
  geom_sf(
    data = map_data_bi_continental,
    aes(fill = bi_class),
    color = "white", size = 0.1, show.legend = FALSE
  ) +
  bi_scale_fill(pal = "GrPink", dim = 3) +
  labs(
    title = "Preventable Hospital Stays & Mental Health Providers (US Counties)"
  ) +
  bi_theme() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50), expand = FALSE)

legend <- bi_legend(
  pal = "GrPink",
  dim = 3,
  xlab = "Preventable Hospital Stays",
  ylab = "# of Mental Health Providers",
  size = 6
)


ggdraw() +
  draw_plot(bimap, 0, 0, 1, 1) +
  draw_plot(legend, 0.75, 0.15, 0.2, 0.2) + 
  theme(text = element_text(size = 10))


ggsave("vip_wh.png", plot = last_plot(), width = 12, height = 8, dpi = 300)

imp_rfnr <- importance(rfnr_model)
imp_df_rfnr <- data.frame(
  Variable = rownames(imp_rfnr),
  Importance = imp_rfnr[,"%IncMSE"]
) |> 
  mutate(Variable = str_replace_all(Variable, var_name_lookup)) |> 
  arrange(desc(Importance)) |> 
  slice_head(n = 10)

ggplot(imp_df_rfnr, aes(x = reorder(Variable, Importance), y = Importance, fill = Variable)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Most Important Variables (All Counties)",
    x = "Predictor",
    y = "%IncMSE"
  ) +
  scale_fill_brewer(palette = "Spectral") +
  theme_minimal(base_size = 14)+
  theme(panel.background = element_rect(fill = "white", color = NA), 
        plot.background = element_rect(fill = "white", color = NA))

