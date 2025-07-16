library(tidyverse)
library(sf)           
library(spdep)        
library(mgcv)         
library(xgboost)      
library(SHAPforxgboost) 
library(mice)         
library(patchwork)    
library(ggdag)      
library(MASS)
library(broom)
library(dplyr)
library(janitor)
source("EDA_import.R")


## Data ##
data <- national_data
# County Health Disparities Analysis
# Comprehensive Modeling Approach

# Load required libraries
library(tidyverse)
library(MASS)       # For negative binomial regression
library(broom)      # For tidy model outputs
library(performance) # For model diagnostics
library(janitor)    # For data cleaning
library(patchwork)  # For plot arrangement
library(AER)        # For dispersion test

# 1. Data Preparation -----------------------------------------------------

# Load and clean primary dataset
analysis_data <- national_data %>%
  janitor::clean_names() %>%
  # Select key variables
  dplyr::select(
    # Geographic identifiers
    fips = x5_digit_fips_code,
    county = name,
    state = state_abbreviation,
    population = population_raw_value,
    
    # Outcome variables
    preventable_stays = preventable_hospital_stays_raw_value,
    preventable_black = preventable_hospital_stays_black,
    preventable_white = preventable_hospital_stays_white,
    
    # Healthcare access
    uninsured = uninsured_raw_value,
    primary_care = primary_care_physicians_raw_value,
    mental_health = mental_health_providers_raw_value,
    dentists = dentists_raw_value,
    other_providers = other_primary_care_providers_raw_value,
    flu_vaccinations = flu_vaccinations_raw_value,
    mammogram = mammography_screening_raw_value,
    broadband = broadband_access_raw_value,
    
    # Demographic variables
    pct_white = percent_non_hispanic_white_raw_value,
    pct_black = percent_non_hispanic_black_raw_value,
    pct_hispanic = percent_hispanic_raw_value,
    poverty = children_in_poverty_raw_value
  ) %>%
  # Convert to numeric and handle missing values
  mutate(across(-c(fips, county, state), ~ as.numeric(gsub(",", "", .)))) %>%
  filter(population > 0) %>%
  drop_na()

# 2. Feature Engineering -------------------------------------------------

# Create transformed variables
model_data <- analysis_data %>%
  mutate(
    # Outcome measures
    log_preventable = log(preventable_stays + 1),
    disparity_count = round((preventable_black - preventable_white) * population / 100000),
    
    # Transformed predictors
    log_uninsured = log(uninsured + 1),
    log_primary_care = log(primary_care + 1),
    log_mental_health = log(mental_health + 1),
    log_dentists = log(dentists + 1),
    log_other_providers = log(other_providers + 1),
    log_population = log(population),
    
    # Rescaled variables
    primary_care_per_100k = primary_care / population * 100000,
    mental_health_per_100k = mental_health / population * 100000
  )

# 3. Model Building ------------------------------------------------------

# Negative Binomial Model for Count Data
nb_model <- glm.nb(
  preventable_stays ~ 
    log_uninsured +
    primary_care_per_100k +
    mental_health_per_100k +
    poly(broadband, 2) +
    poverty +
    pct_black +
    offset(log_population),
  data = model_data
)

# 4. Model Diagnostics ---------------------------------------------------

# Proper dispersion test for negative binomial
dispersion_test <- function(model) {
  r <- residuals(model, type = "pearson")
  n <- length(r)
  p <- length(coef(model))
  disp <- sum(r^2) / (n - p)
  data.frame(
    statistic = disp,
    p.value = pchisq(disp * (n - p), df = n - p, lower.tail = FALSE),
    method = "Dispersion test for Negative Binomial"
  )
}

cat("\nModel Summary:\n")
print(summary(nb_model))

cat("\nDispersion Test:\n")
print(dispersion_test(nb_model))

# Check if theta (dispersion parameter) is significantly different from 0
cat("\nTheta (Dispersion Parameter):\n")
print(nb_model$theta)
print(paste("Std. Error:", nb_model$SE.theta))

# 5. Final Model Interpretation ------------------------------------------

# Calculate Incidence Rate Ratios with confidence intervals
final_results <- broom::tidy(nb_model, exponentiate = TRUE, conf.int = TRUE) %>% 
  mutate(across(where(is.numeric), ~ round(., 3)))

cat("\nFinal Model Results (IRRs):\n")
print(final_results)

# 6. Visualization of Key Results ----------------------------------------

# Create coefficient plot
coef_plot <- final_results %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high)) +
  geom_pointrange() +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_x_log10() +
  labs(title = "Predictors of Preventable Hospital Stays",
       x = "Incidence Rate Ratio (log scale)",
       y = "Predictor") +
  theme_minimal()

print(coef_plot)

# 7. Save Results --------------------------------------------------------

saveRDS(nb_model, "final_health_disparities_model.rds")
write_csv(model_data, "processed_health_data.csv")

cat("\nAnalysis complete. Results saved to:\n",
    "- final_health_disparities_model.rds\n",
    "- processed_health_data.csv\n")