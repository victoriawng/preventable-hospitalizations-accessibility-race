# understand healthcare access
# preventable hospitalizations

source("EDA_import.R")
library(tidyverse)

view(national_data)
view(trend_data)
view(data_dict_2025)

dim(national_data) #3205  796
dim(trend_data) #745088     15
dim(data_dict_2025) #796   3


# missing values ---------------------------------------------------------------
library(visdat)
# install.packages("visdat")
vis_miss(national_data, warn_large_data = FALSE) +
  #  hides the column names
  theme(axis.text.x = element_blank())
  # removes the little tick marks too
        # ,axis.ticks.x = element_blank())

# ---
# https://www.geeksforgeeks.org/r-language/how-to-find-and-count-missing-values-in-r-dataframe/
# Dec 2023

# finds the location of missing values
# indice NOT row or column
# which(is.na(national_data))

# finds the count of missing values 
# sum(is.na(national_data))

# ouputs length, class, and mode
# summary(national_data[25]) # change number to change column summary


# ---
# https://www.geeksforgeeks.org/r-language/handling-missing-values-in-r-programming/
# April 2025

# display the number of NA values in each column
class(colSums(is.na(national_data)))
class(national_data)


class(is.na(national_data))


vis_miss(national_data, warn_large_data = FALSE)


