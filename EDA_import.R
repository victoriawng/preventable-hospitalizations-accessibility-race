# importing data

# data sets and dictionaries: https://www.countyhealthrankings.org/health-data/methodology-and-sources/data-documentation
# install.packages("readxl")
# install.packages("haven") 

library(readxl)
library(haven)
# idk what a sas7bdat is but that's a later problem
national_data = read_sas("data/analytic_data2025_v2.sas7bdat") 
trend_data = read_sas("data/chr_trends_sas_2025.sas7bdat")
data_dict_2025 = read_excel("data/DataDictionary_2025.xlsx")


# put this code into your script wherever you want to call it
# source("EDA_import.R")

