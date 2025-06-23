# we want to do analysis on primary care physicians, uninsured rate, race demographics 
library(tidyverse)
library(readxl)
national_data <- read_csv("data/analytic_data2025_v2.csv")
trend_data <- read_csv("data/chr_trends_csv_2025.csv")
view(national_data)
view(trend_data)

colnames(national_data)


filter_national_data <- national_data |>
  select(`State FIPS Code`, `County FIPS Code`, `5-digit FIPS Code`, `State Abbreviation`, `Name`, `Release Year`, `Premature Death raw value`, `Premature Death numerator`, `Premature Death denominator`)
filter_national_data
