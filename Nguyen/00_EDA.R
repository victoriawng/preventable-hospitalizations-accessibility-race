# install and run packages -----------------------------------------------------

library(readxl)
library(tidyverse)
# install.packages("haven")
library(haven)
# install.packages("visdat")
library(visdat)

# to check which packages are loaded
# (.packages())

# notes ------------------------------------------------------------------------
# NA values
# log it then plus 1

# look over trend data

# importing data ---------------------------------------------------------------
# im importing SAS
# but team is using CSV
# idk what a sas7bdat is but that's a later problem
national_data = read_sas("data/analytic_data2025_v2.sas7bdat")
trend_data = read_sas("data/chr_trends_sas_2025.sas7bdat")
data_dict_2025 = read_excel("data/DataDictionary_2025.xlsx")


# agenda -----------------------------------------------------------------------
# Project 2: Healthcare access & preventable hospital stays (TAs: Princess, James)
# Does healthcare access (e.g., primary care physicians, uninsured rate, etc) 
# affect the number of preventable hospital stays of certain racial groups at the county level?

# ---
# understand healthcare access
# preventable hospitalizations (in terms of medical services)

# affordability?
# ---


# things to look at
# arturo: uninsured (adults or kids?), population provider ratio (there's also numerator and denominator)
# p: 
# ---


# as you go through this
# remember you can almost always use view() to see the output better


# ------------------------------------------------------------------------------
# exploring/understanding the data
# -----------------------

# understanding RAW data set

view(national_data)
view(trend_data)
view(data_dict_2025)


dim(national_data) #3204  796
dim(trend_data) #745088     15
dim(data_dict_2025) #796   3


# class(national_data) # "spec_tbl_df" "tbl_df"      "tbl"         "data.frame" 
# class(trend_data) # "spec_tbl_df" "tbl_df"      "tbl"         "data.frame" 
# class(data_dict_2025) # "tbl_df"     "tbl"        "data.frame"
# i learned that tbl_df remembers the data type when parsed
# while spec_tbl_df doesn't so that's why it defaults to chr
class(national_data) # "tbl_df"      "tbl"         "data.frame"
class(trend_data) # "tbl_df"      "tbl"         "data.frame"
class(data_dict_2025) # "tbl_df"     "tbl"        "data.frame"


# ways to show class or data types of each column
# https://www.geeksforgeeks.org/check-data-type-of-each-dataframe-column-in-r/
str(national_data)
sapply(national_data, class) 

# for csv
# notice how we will prob need to change chr to corresponding data type (exp: chr to int in order for us to plot)
# it is all chr bc we read it in that way
# if we wanted to, we can find if there's a way to import data so that the data type pertains or doesnt change
# which is to import with SAS

head(national_data)








# -----------------------
# descriptive statistics
# do by columns or consider different data types

# int is quantative 
# fct is categorical 

# for quantitative variables
# summary()

# for categorical variables 
# table()


