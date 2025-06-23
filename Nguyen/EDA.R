# hello this is Victoria's EDA

# understand healthcare access
# preventable hospitalizations

# 

source("EDA_import.R")
library(tidyverse)

view(national_data)
view(trend_data)
view(data_dict_2025)


dim(national_data) #3205  796
dim(trend_data) #745088     15
dim(data_dict_2025) #796   3

# install.packages("visdat")
library(visdat)
# vis_miss(covid_hospitalizations, warn_large_data = FALSE) # raw df # NA 14.5% missing
