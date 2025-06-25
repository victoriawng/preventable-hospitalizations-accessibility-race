# understand healthcare access
# preventable hospitalizations

source("EDA_import.R")

# install.packages("visdat")
library(tidyverse)
library(skimr)
library(visdat)

# remember you can almost always use view() to see the output better
# ------------------------------------------------------------------------------
# exploring/understanding the data
# -----------------------
# understanding data set

view(national_data)
view(trend_data)
view(data_dict_2025)

dim(national_data) #3205  796
dim(trend_data) #745088     15
dim(data_dict_2025) #796   3

class(national_data) # "spec_tbl_df" "tbl_df"      "tbl"         "data.frame" 
class(trend_data) # "spec_tbl_df" "tbl_df"      "tbl"         "data.frame" 
class(data_dict_2025) # "tbl_df"     "tbl"        "data.frame"

# class(data.frame())          # "data.frame"
# class(tibble())              # "tbl_df" "tbl" "data.frame"
# class(data.table())          # "data.table" "data.frame"
# class(group_by(tibble(), x)) # "grouped_df" "tbl_df" "tbl" "data.frame"

# ways to show class or data types of each column
# https://www.geeksforgeeks.org/check-data-type-of-each-dataframe-column-in-r/
str(national_data) # 
sapply(national_data, class) 
# notice how we will prob need to change chr to corresponding data type (exp: chr to int in order for us to plot)
# it is all chr bc we read it in that way
# if we wanted to, we can find if there's a way to import data so that the data type pertains or doesnt change

class(national_data$)

head(national_data)

# -----------------------
# descriptive statistics
# do by columns or consider different data types

# int is quantative 
# fct is categorical 

# for quantitative variables
# summary()
summary()

# for categorical variables 
# table()




# missing values ---------------------------------------------------------------
# library(visdat)
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
colSums(is.na(national_data))

# na function
get_na_proportion <- function(df) {
  na_prop <- colSums(is.na(df)) / nrow(df)
  result <- data.frame(
    Column = names(na_prop),
    ProportionMissing = as.vector(na_prop),
    row.names = NULL
  )
  return(result)
}

view(get_na_proportion(national_data))

view(get_na_proportion(national_data) |>
  filter(ProportionMissing < 0.6))

view(get_na_proportion(national_data) |>
       filter(ProportionMissing > 0.6))



df_with_60_na <- df_cleaned_column[rowMeans(is.na(df_cleaned_column)) > .6, ]
# view(df_with_60_na)
dim(df_with_60_na) #7035
# view(count_NA(df_with_60_na))


class(colSums(is.na(national_data)))


class(national_data)


class(is.na(national_data))


get_na_proportion <- function(df) {
  na_prop <- colSums(is.na(df)) / nrow(df)
  data.frame(Column = names(na_prop), ProportionMissing = na_prop)
}



