# ----------------------------------------
# SURE 2025 – Project 2:
# Does healthcare access affect the number
# of preventable hospital stays by race?
# ----------------------------------------

# library(readxl)
library(tidyverse)

county_data = read_csv("data/analytic_data2025_v2.csv") 

library(tidyverse)
library(MASS)
library(patchwork)


# Step 1: Construct cleaned dataset from County Health Rankings
eda_data <- county_data %>%
  transmute(
    fips = `5-digit FIPS Code`,
    county = Name,
    state = `State Abbreviation`),
    
    # Outcome variables by race
    phs_white = as.numeric(`Preventable Hospital Stays (White)`),
    phs_black = as.numeric(`Preventable Hospital Stays (Black)`),
    phs_hispanic = as.numeric(`Preventable Hospital Stays (Hispanic)`),
    
    # Population and race composition
    pop = as.numeric(`Population raw value`),
    white_pct = as.numeric(`% Non-Hispanic White raw value`),
    black_pct = as.numeric(`% Non-Hispanic Black raw value`),
    
    # Core predictors of access
    pcp_per_100k = as.numeric(`Primary Care Physicians raw value`),
    pcp_ratio = as.numeric(`Ratio of population to primary care physicians.`),
    uninsured = as.numeric(`Uninsured raw value`),
    broadband = as.numeric(`Broadband Access raw value`),
    
    # Additional access & preventive care measures
    dentists = as.numeric(`Dentists raw value`),
    mental = as.numeric(`Mental Health Providers raw value`),
    other_providers = as.numeric(`Other Primary Care Providers raw value`),
    mammogram = as.numeric(`Mammography Screening raw value`)
  )

# Step 2: 
eda_data <- eda_data %>%
  mutate(
    log_uninsured = log(uninsured + 1),
    log_dentists = log(dentists + 1),
    log_mental = log(mental + 1),
    log_otherproviders = log(other_providers + 1),
    pcp_scaled = pcp_per_100k * 100000  # rescale for interpretability
  )

# Step 3: Exploratory plots – focus on outcome vs. key predictors
eda_data <- eda_data %>%
  mutate(log_phs_white = log(phs_white + 1))

p1 <- ggplot(eda_data, aes(x = pcp_per_100k, y = log_phs_white)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "PCP Access vs PHS (White)", x = "Primary Care Physicians per 100k", y = "log(PHS White)") +
  theme_minimal()

p2 <- ggplot(eda_data, aes(x = uninsured, y = log_phs_white)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Uninsured Rate vs PHS (White)", x = "Uninsured Rate", y = NULL) +
  theme_minimal()

p3 <- ggplot(eda_data, aes(x = broadband, y = log_phs_white)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", color = "darkgreen") +
  labs(title = "Broadband Access vs PHS (White)", x = "Broadband Access", y = NULL) +
  theme_minimal()

(p1 | p2 | p3)  # Side-by-side layout

# Step 4:  drop missing values in predictors/outcome
nb_data_extended <- eda_data %>%
  drop_na(
    phs_white,
    log_uninsured,
    log_dentists,
    log_otherproviders,
    broadband,
    log_mental,
    pcp_scaled,
    mammogram,
    black_pct,
    pop
  )

# Step 5: Fit negative binomial model for White residents
nb_model_extended <- glm.nb(
  phs_white ~ 
    log_uninsured +          
    log_dentists +           
    log_otherproviders +    
    poly(broadband, 2) +          
    log_mental +             
    pcp_scaled +                
    mammogram + 
    black_pct + 
    pop,
  data = nb_data_extended
)

# Fit negative binomial model for Black residents
nb_model_extendedB <- glm.nb(
  phs_black ~ 
    log_uninsured +          
    log_dentists +           
    log_otherproviders +    
    poly(broadband, 2) +          
    log_mental +             
    pcp_scaled +                
    mammogram + 
    black_pct + 
    pop,
  data = nb_data_extended
)
# Step 6: Summarize model fit
summary(nb_model_extended)
summary(nb_model_extendedB)

# Step 7: Compute and display IRRs with 95% CIs
exp_coef_ext <- exp(coef(nb_model_extended))
exp_ci_ext <- exp(confint(nb_model_extended)) 

model_results_extended <- round(data.frame(
  IRR = exp_coef_ext,
  `CI Lower` = exp_ci_ext[,1],
  `CI Upper` = exp_ci_ext[,2]
), 3)

model_results_extended

# Step 8: Diagnostic plots 
plot(residuals(nb_model_extended, type = "deviance"),
     main = "Deviance Residuals")

ggplot(nb_data_extended, aes(x = predict(nb_model_extended, type = "response"), y = phs_white)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(x = "Predicted PHS (White)", y = "Observed PHS (White)",
       title = "Model Fit: Predicted vs Observed") +
  theme_minimal()

#  EDA visualization: PCP access vs Preventable Hospital Stays
library(scales)
library(ggplot2)

phs_breaks <- c(0, 100, 500, 1000, 2500, 5000, 10000, 25000, 50000)

ggplot(eda_data, aes(x = pcp_per_100k, y = phs_white)) +
  geom_point(
    alpha = 0.5, 
    color = "#3366CC",
    size = 2.5  
  ) +
  geom_smooth(
    method = "lm", 
    formula = y ~ x, 
    se = TRUE, 
    color = "#003399", 
    fill = "#CCDDFF", 
    size = 1.2,
    alpha = 0.2 
  ) +
  scale_y_continuous(
    trans = "log1p",
    breaks = phs_breaks,
    labels = comma_format(accuracy = 1),
    limits = c(NA, max(eda_data$phs_white, na.rm = TRUE))  
  ) +
  scale_x_continuous(
    labels = comma,
    breaks = scales::pretty_breaks(n = 6)  
  ) +
  labs(
    x = "Primary Care Physicians per 100,000 Residents",
    y = "Preventable Hospital Stays (White Residents) Log Scale",
    
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 18,
      hjust = 0.5,
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      size = 12,
      hjust = 0.5,
      color = "gray40",
      margin = margin(b = 20)
    ),
    axis.title = element_text(face = "bold", size = 12),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 10)),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(
      size = 10,
      color = "gray50",
      hjust = 0,
      margin = margin(t = 15)
    ),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  ) +
  annotation_logticks(
    sides = "l",
    short = unit(0.05, "cm"),
    mid = unit(0.1, "cm"),
    long = unit(0.15, "cm")
  )




# Prepare long-form data for racial group comparison
library(tidyr)

eda_long <- eda_data %>%
  dplyr::select(uninsured, phs_white, phs_black) %>%
  pivot_longer(cols = starts_with("phs_"), names_to = "group", values_to = "phs") %>%
  mutate(
    group = recode(group,
                   "phs_white" = "White",
                   "phs_black" = "Black"
            
    )
  )

# Uninsured rate vs PHS by racial group
ggplot(eda_long, aes(x = uninsured, y = phs, color = group)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, size = 1.1) +
  scale_color_manual(values = c("White" = "#3366CC", "Black" = "#CC0000")) +
  scale_y_continuous(labels = scales::comma, trans = "log1p") +
  labs(
    x = "Uninsured Rate",
    y = "Preventable Hospital Stays (log scale)",
    color = "Racial Group"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 13),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom"
  )
