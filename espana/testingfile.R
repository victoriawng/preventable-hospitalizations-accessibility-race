#comment testing

library(tidyverse)
library(readxl)
national_data <- read_csv("analytic_data2025_v2.csv")
trend_data <- read_csv("chr_trends_csv_2025.csv")
ranking_data <- read_excel("2025 County Health Rankings Data - v3.xlsx")
view(national_data)
view(trend_data)

colnames(national_data)


