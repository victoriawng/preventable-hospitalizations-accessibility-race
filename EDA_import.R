# importing data

# install.packages("readxl")
library(tidyverse)
library(readxl)
national_data <- read_csv("data/analytic_data2025_v2.csv")
trend_data <- read_csv("data/chr_trends_csv_2025.csv")
data_dict_2025 = read_excel("data/DataDictionary_2025.xlsx")


# put this code into your script wherever you want to call it
# source("EDA_import.R")