# importing data

# data sets and dictionaries: https://www.countyhealthrankings.org/health-data/methodology-and-sources/data-documentation
# install.packages("readxl")


library(readxl)
library(tidyverse)

national_data = read_csv("data/analytic_data2025_v2.csv") 
trend_data = read_csv("data/chr_trends_csv_2025.csv")
data_dict_2025 = read_excel("data/DataDictionary_2025.xlsx")

