library(tidyverse)
library(janitor)
library(ggplot2)
library(scales)
library(usmap)
library(dplyr)
library(tidyr)
library(scales)
library(broom)
library(MASS)
library(glmmTMB)
library(car)
library(tibble)
library(knitr)
library(DHARMa)
library(sandwich)
library(lmtest)
source("EDA_import.R")

# Cleaning variables names
data <- national_data |>
  clean_names()

# Selecting variables of interest
# clinical care and social economic 
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

# Clean and compute weighted POC values
no_na_data <- data_clean |>
  dplyr::select(-high_school_graduation_raw_value) |>
  mutate(
    # Total POC population
    pop_poc = pop_aian + pop_asian_pacific + pop_black + pop_hispanic,
    
    # Weighted preventable hospital stays for POC
    preventable_hospital_stays_poc = case_when(
      pop_poc > 0 ~ (
        preventable_hospital_stays_aian * pop_aian +
          preventable_hospital_stays_asian_pacific_islander * pop_asian_pacific +
          preventable_hospital_stays_black * pop_black +
          preventable_hospital_stays_hispanic * pop_hispanic
      ) / pop_poc,
      TRUE ~ NA_real_
    ),
    
    # Weighted mammography screening for POC
    mammography_screening_poc = case_when(
      pop_poc > 0 ~ (
        mammography_screening_aian * pop_aian +
          mammography_screening_asian_pacific_islander * pop_asian_pacific +
          mammography_screening_black * pop_black +
          mammography_screening_hispanic * pop_hispanic
      ) / pop_poc,
      TRUE ~ NA_real_
    )
  ) |>
  # Drop race-specific components
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

# Drop rows with any remaining NAs
done_data <- no_na_data |>
  drop_na()

# Final cleaned dataset
analysis_data <- done_data |>
  dplyr::select(
    # Identifiers
    state_fips_code, county_fips_code, name,state_abbreviation,
    # Outcomes
    preventable_hospital_stays_white, preventable_hospital_stays_poc,
    # Healthcare access
    physician_supply_per_10k, mental_health_providers_per_10k, 
    dentists_per_10k, uninsured_raw_value,
    mammography_screening_white, mammography_screening_poc,
    # Controls
    income_inequality, unemployment_rate, 
    percent_rural_raw_value, high_school_completion_raw_value,
    # Population offsets
    pop_white, pop_poc, population_raw_value
  ) |>
  # Convert percentages to proportions
  mutate(across(c(uninsured_raw_value, percent_rural_raw_value, 
                  high_school_completion_raw_value),
                ~ ./100))



######## EDA 

access_vars <- c(
  "physician_supply_per_10k",
  "mental_health_providers_per_10k",
  "dentists_per_10k",
  "uninsured_raw_value",
  "mammography_screening_white",
  "mammography_screening_poc"
)

# distribution
for (var in access_vars) {
  p <- ggplot(analysis_data, aes_string(x = var)) +
    geom_histogram(fill = "steelblue", color = "white", bins = 30) +
    labs(
      title = paste("Distribution of", var),
      x = var,
      y = "Number of Counties"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  print(p)
}

# mental_health_providers_per_10k and uninsured_raw_value are right skew lets transform them
# dealing with negative values
model_analysis_data <- analysis_data |>
  mutate(
    mental_health_providers_per_10k = ifelse(
      mental_health_providers_per_10k < 0, NA, mental_health_providers_per_10k
    )
  ) |>
  mutate(
    log_mental_health_per_10k = log1p(mental_health_providers_per_10k)
  ) |>
  drop_na(log_mental_health_per_10k)

# check dis. again
for (var in access_vars) {
  p <- ggplot(model_analysis_data, aes_string(x = var)) +
    geom_histogram(fill = "green", color = "white", bins = 30) +
    labs(
      title = paste("Distribution OF", var),
      x = var,
      y = "Number of Counties"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  print(p)
}

# uninsured rate vs preventable hospitalizations

# Build cleaned plot dataset
plot_data <- analysis_data |>
  mutate(
    uninsured_pct = uninsured_raw_value * 100,
    rate_white = preventable_hospital_stays_white / pop_white * 10000,
    rate_poc = preventable_hospital_stays_poc / pop_poc * 10000
  ) |>
  filter(
    pop_white > 1000, pop_poc > 1000,
    rate_white > 0, rate_white < 300,
    rate_poc > 0, rate_poc < 300
  )

# Plot
ggplot(plot_data) +
  geom_point(aes(
    x = uninsured_pct,
    y = rate_white,
    color = "White"
  ), alpha = 0.6) +
  geom_point(aes(
    x = uninsured_pct,
    y = rate_poc,
    color = "POC"
  ), alpha = 0.6) +
  geom_smooth(aes(
    x = uninsured_pct,
    y = rate_white,
    color = "White"
  ), method = "lm", se = FALSE) +
  geom_smooth(aes(
    x = uninsured_pct,
    y = rate_poc,
    color = "POC"
  ), method = "lm", se = FALSE) +
  labs(
    title = "Uninsured Rate vs. Preventable Hospitalizations",
    subtitle = "Per 10,000 population by racial group",
    x = "Uninsured Rate (%)",
    y = "Hospital Stays per 10,000",
    color = "Racial Group"
  ) +
  scale_color_manual(values = c("White" = "#377eb8", "POC" = "#e41a1c")) +
  theme_minimal()


# map visualization
library(usmap)
library(viridis)

# Step 1: Prepare state-level summary
state_map_data <- analysis_data |>
  group_by(state = state_abbreviation) |>
  summarize(
    white_rate = sum(preventable_hospital_stays_white, na.rm = TRUE) / 
      sum(pop_white, na.rm = TRUE) * 10000,
    poc_rate = sum(preventable_hospital_stays_poc, na.rm = TRUE) / 
      sum(pop_poc, na.rm = TRUE) * 10000,
    disparity_ratio = poc_rate / white_rate
  ) |>
  filter(
    is.finite(disparity_ratio),
    !is.na(state)
  )

# Step 2: Plot state-level disparity ratios
plot_usmap(data = state_map_data, values = "disparity_ratio", regions = "states") +
  scale_fill_viridis_c(
    name = "Disparity Ratio\n(POC / White)",
    option = "magma",
    trans = "log10",
    breaks = c(0.5, 1, 2),
    labels = c("0.5x", "1x", "2x")
  ) +
  labs(
    title = "State-Level Disparities in Preventable Hospital Stays",
    subtitle = "Ratio of POC to White hospitalization rates"
  ) +
  theme(legend.position = "right")


# correlation heatmap
# Select and compute correlation matrix
cor_matrix <- analysis_data |>
  dplyr::select(
    preventable_hospital_stays_white,
    preventable_hospital_stays_poc,
    physician_supply_per_10k,
    uninsured_raw_value,
    income_inequality,
    percent_rural_raw_value,
    mental_health_providers_per_10k, 
    dentists_per_10k,
    mammography_screening_white,
    mammography_screening_poc
  ) |>
  na.omit() |>
  cor()

# Convert to long format for ggplot
cor_long <- as.data.frame(cor_matrix) |>
  rownames_to_column(var = "Var1") |>
  pivot_longer(-Var1, names_to = "Var2", values_to = "Correlation")

# Plot using ggplot2
ggplot(cor_long, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "#d73027", high = "#1a9850", mid = "white", 
    midpoint = 0, limit = c(-1, 1), space = "Lab",
    name = "Correlation"
  ) +
  geom_text(aes(label = round(Correlation, 2)), color = "black", size = 3) +
  labs(
    title = "Key Variable Correlations",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    panel.grid = element_blank()
  )

# racial disaprities in outcome 
analysis_data |>
  pivot_longer(
    cols = c(preventable_hospital_stays_white, preventable_hospital_stays_poc),
    names_to = "group",
    values_to = "hospital_stays"
  ) |>
  mutate(
    group = ifelse(grepl("white", group), "White", "POC"),
    population = ifelse(group == "White", pop_white, pop_poc),
    rate = hospital_stays / population * 10000
  ) |>
  filter(is.finite(rate), population > 1000, rate > 0, rate < 300) %>%
  ggplot(aes(x = group, y = rate, fill = group, weight = population)) +
  geom_violin(
    alpha = 0.7, draw_quantiles = c(0.25, 0.5, 0.75),
    trim = FALSE
  ) +
  labs(
    title = "Preventable Hospitalization Rates by Race",
    y = "Stays per 10,000 population", x = NULL
  ) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )

# access factor disparities

access_vars <- c(
  "physician_supply_per_10k",
  "mental_health_providers_per_10k",
  "dentists_per_10k",
  "income_inequality",
  "high_school_completion_raw_value"
)

var_labels <- c(
  physician_supply_per_10k = "Physician Supply per 10k",
  mental_health_providers_per_10k = "Mental Health Providers per 10k",
  dentists_per_10k = "Dentists per 10k",
  income_inequality = "Income Inequality (Ratio)",
  high_school_completion_raw_value = "High School Completion (%)"
)

percent_vars <- c("high_school_completion_raw_value") 

analysis_data |>
  mutate(insured_group = ifelse(uninsured_raw_value > median(uninsured_raw_value, na.rm = TRUE), "Uninsured", "Insured")) |>
  pivot_longer(cols = all_of(access_vars), names_to = "factor", values_to = "value") |>
  group_by(factor, insured_group) |>
  summarize(mean_value = mean(value, na.rm = TRUE), .groups = "drop") |>
  mutate(
    factor_label = fct_rev(fct_reorder(var_labels[factor], mean_value)),
    mean_value_scaled = ifelse(factor %in% percent_vars, mean_value * 100, mean_value)
  ) |>
  ggplot(aes(x = mean_value_scaled, y = factor_label, fill = insured_group)) +
  geom_col(position = "dodge") +
  labs(
    title = "Access Factors by Insurance Status",
    x = "Mean Value",
    y = "",
    fill = "Insurance Status"
  ) +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(labels = label_number(accuracy = 1)) +
  theme_minimal()

# predictors affect on racial group (regression)
clean_data <- analysis_data |>
  mutate(
    rate_white = preventable_hospital_stays_white / pop_white * 10000,
    rate_poc   = preventable_hospital_stays_poc / pop_poc * 10000
  ) |>
  filter(
    is.finite(rate_white),
    is.finite(rate_poc)
  ) |>
  mutate(
    dplyr::across(
      c(physician_supply_per_10k, mental_health_providers_per_10k, 
        dentists_per_10k, uninsured_raw_value,
        mammography_screening_white, mammography_screening_poc,
        income_inequality, unemployment_rate, 
        percent_rural_raw_value, high_school_completion_raw_value),
      ~ scale(.)[,1],
      .names = "z_{.col}"
    )
  )

# Run models for each group
model_white <- lm(rate_white ~ z_physician_supply_per_10k + z_mental_health_providers_per_10k +
                    z_dentists_per_10k + z_uninsured_raw_value +
                    z_mammography_screening_white +
                    z_income_inequality + z_unemployment_rate + 
                    z_percent_rural_raw_value + z_high_school_completion_raw_value,
                  data = clean_data)

model_poc <- lm(rate_poc ~ z_physician_supply_per_10k + z_mental_health_providers_per_10k +
                  z_dentists_per_10k + z_uninsured_raw_value +
                  z_mammography_screening_poc +
                  z_income_inequality + z_unemployment_rate + 
                  z_percent_rural_raw_value + z_high_school_completion_raw_value,
                data = clean_data)

# Combine and label results
bind_rows(
  tidy(model_white) |> mutate(group = "White"),
  tidy(model_poc)   |> mutate(group = "POC")
) |>
  filter(term != "(Intercept)") |>
  mutate(
    term = dplyr::recode(term,
                         "z_physician_supply_per_10k" = "Physician Supply",
                         "z_mental_health_providers_per_10k" = "Mental Health Providers",
                         "z_dentists_per_10k" = "Dentists",
                         "z_uninsured_raw_value" = "Uninsured Rate",
                         "z_mammography_screening_white" = "Mammography Screening (White)",
                         "z_mammography_screening_poc" = "Mammography Screening (POC)",
                         "z_income_inequality" = "Income Inequality",
                         "z_unemployment_rate" = "Unemployment Rate",
                         "z_percent_rural_raw_value" = "Percent Rural",
                         "z_high_school_completion_raw_value" = "High School Completion"
    )
  ) |>
  ggplot(aes(x = estimate, y = term, color = group)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbarh(aes(xmin = estimate - std.error, xmax = estimate + std.error),
                 height = 0.2,
                 position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Standardized Predictors of Hospital Stays by Racial Group",
    x = "Effect Size (Standardized)",
    y = "",
    color = "Racial Group"
  ) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal()

########### Modeling


# Fit Model 
fit_nb_model <- function(data, outcome, offset_var, screening_var) {
  formula <- as.formula(paste(
    outcome, "~ physician_supply_per_10k + mental_health_providers_per_10k +",
    "dentists_per_10k + uninsured_raw_value +", screening_var, "+",
    "income_inequality + log_unemployment + percent_rural_raw_value +",
    "high_school_completion_raw_value + offset(", offset_var, ")"
  ))
  tryCatch({
    nb_model <- glm.nb(formula, data = data, control = glm.control(maxit = 100))
    message("✅ glm.nb succeeded for ", outcome)
    return(nb_model)
  }, error = function(e) {
    message("⚠️ glm.nb failed for ", outcome, " — using glmmTMB: ", e$message)
    return(glmmTMB(formula, family = nbinom2, data = data))
  })
}

prep_data <- function(data, outcome, pop_var, screening_var) {
  data |>
    mutate(
      !!sym(paste0("log_", pop_var)) := log(!!sym(pop_var)),
      log_unemployment = log(unemployment_rate + 1)
    ) |>
    dplyr::select(
      !!sym(outcome),
      !!sym(paste0("log_", pop_var)),
      physician_supply_per_10k,
      mental_health_providers_per_10k,
      dentists_per_10k,
      uninsured_raw_value,
      !!sym(screening_var),
      income_inequality,
      log_unemployment,
      percent_rural_raw_value,
      high_school_completion_raw_value
    ) |>
    mutate(across(
      -c(!!sym(outcome), !!sym(paste0("log_", pop_var))),
      ~ (. - min(.)) / (max(.) - min(.))
    )) |>
    filter(if_all(everything(), is.finite))
}

model_data_white <- prep_data(analysis_data, 
                              "preventable_hospital_stays_white", 
                              "pop_white", 
                              "mammography_screening_white")

model_data_poc <- prep_data(analysis_data, 
                            "preventable_hospital_stays_poc", 
                            "pop_poc", 
                            "mammography_screening_poc")

# Fit Models 
model_white <- fit_nb_model(model_data_white, 
                            "preventable_hospital_stays_white", 
                            "log_pop_white", 
                            "mammography_screening_white")

model_poc <- fit_nb_model(model_data_poc, 
                          "preventable_hospital_stays_poc", 
                          "log_pop_poc", 
                          "mammography_screening_poc")

# VIF Checks
cat("\nVariance Inflation Factors (White):\n")
print(vif(lm(
  preventable_hospital_stays_white ~ physician_supply_per_10k + 
    mental_health_providers_per_10k + dentists_per_10k + uninsured_raw_value +
    mammography_screening_white + income_inequality + log_unemployment +
    percent_rural_raw_value + high_school_completion_raw_value,
  data = model_data_white
)))

cat("\nVariance Inflation Factors (POC):\n")
print(vif(lm(
  preventable_hospital_stays_poc ~ physician_supply_per_10k + 
    mental_health_providers_per_10k + dentists_per_10k + uninsured_raw_value +
    mammography_screening_poc + income_inequality + log_unemployment +
    percent_rural_raw_value + high_school_completion_raw_value,
  data = model_data_poc
)))

# IRR Extraction 
summarize_model <- function(model, label) {
  if ("glmmTMB" %in% class(model)) {
    est <- fixef(model)$cond
    ci <- confint(model, method = "Wald")[names(est), ]
    irrs <- exp(cbind(IRR = est, Lower = ci[,1], Upper = ci[,2]))
  } else {
    est <- coef(model)
    ci <- confint(model)
    irrs <- exp(cbind(IRR = est, Lower = ci[,1], Upper = ci[,2]))
  }
  as.data.frame(irrs) |>
    rownames_to_column("Variable") |>
    filter(Variable != "(Intercept)") |>
    mutate(Race = label) |>
    mutate(across(where(is.numeric), \(x) round(x, 3)))
}

result_white <- summarize_model(model_white, "White")
result_poc <- summarize_model(model_poc, "POC")

bind_rows(result_white, result_poc) |>
  knitr::kable()

# Model Fit and Diagnostics 
get_fit_metrics <- function(model) {
  list(
    AIC = AIC(model),
    BIC = BIC(model),
    deviance = deviance(model)
  )
}

cat("\nModel Fit (White):\n")
print(get_fit_metrics(model_white))

cat("\nModel Fit (POC):\n")
print(get_fit_metrics(model_poc))

# Residual Diagnostics 
simulate_and_plot <- function(model, label) {
  if ("glmmTMB" %in% class(model)) {
    sim <- simulateResiduals(model)
    plot(sim, main = paste("DHARMa Residuals -", label))
  } else {
    cat("Skipping DHARMa for glm.nb model:", label, "\n")
  }
}

quartz()
simulate_and_plot(model_white, "White")

quartz()
simulate_and_plot(model_poc, "POC")


# Robust SE
robust_se <- function(model) {
  coeftest(model, vcov. = sandwich::vcovHC(model, type = "HC0"))
}

cat("\nRobust SEs (White):\n")
if (!"glmmTMB" %in% class(model_white)) print(robust_se(model_white))

cat("\nRobust SEs (POC):\n")
if (!"glmmTMB" %in% class(model_poc)) print(robust_se(model_poc))

# Influence and Leverage Checks 

# check top Pearson residuals
check_residuals <- function(model_data, model, outcome, label) {
  model_data <- model_data %>%
    mutate(
      resid = residuals(model, type = "pearson"),
      fitted = predict(model, type = "response")
    )
  
  cat("\nTop 5 Largest Absolute Pearson Residuals -", label, "\n")
  model_data %>%
    mutate(abs_resid = abs(resid)) |>
    arrange(desc(abs_resid)) |>
    dplyr::select(!!sym(outcome), resid, fitted, abs_resid) |>
    slice(1:5) |>
    print()
  
  # Plot Residuals vs Fitted
  ggplot(model_data, aes(x = fitted, y = resid)) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    labs(
      title = paste("Residuals vs Fitted -", label),
      x = "Fitted Values",
      y = "Pearson Residuals"
    ) +
    theme_minimal()
}

quartz()
check_residuals(model_data_white, model_white, "preventable_hospital_stays_white", "White")

quartz()
check_residuals(model_data_poc, model_poc, "preventable_hospital_stays_poc", "POC")

# Visualize IRRs
combined_irrs <- bind_rows(result_white, result_poc)

# Clean variable names
combined_irrs <- combined_irrs %>%
  mutate(Variable = case_when(
    Variable == "physician_supply_per_10k" ~ "Physician Supply",
    Variable == "mental_health_providers_per_10k" ~ "Mental Health Providers",
    Variable == "dentists_per_10k" ~ "Dentists",
    Variable == "uninsured_raw_value" ~ "Uninsured Rate",
    Variable == "mammography_screening_white" ~ "Mammography (White)",
    Variable == "mammography_screening_poc" ~ "Mammography (POC)",
    Variable == "income_inequality" ~ "Income Inequality",
    Variable == "log_unemployment" ~ "Log Unemployment",
    Variable == "percent_rural_raw_value" ~ "% Rural",
    Variable == "high_school_completion_raw_value" ~ "HS Completion",
    TRUE ~ Variable
  ))

# Plot
ggplot(combined_irrs, aes(x = IRR, y = fct_reorder(Variable, IRR), color = Race)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = Lower, xmax = Upper), width = 0.2, position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray40") +
  labs(
    title = "Incidence Rate Ratios (IRRs) for Preventable Hospital Stays",
    subtitle = "Comparing White vs POC Models",
    x = "IRR (Exponentiated Coefficient)",
    y = "Predictor",
    color = "Group"
  ) +
  scale_color_manual(values = c("White" = "#377eb8", "POC" = "#e41a1c")) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))



