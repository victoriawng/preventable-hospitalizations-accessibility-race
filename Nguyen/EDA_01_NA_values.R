library(skimr)
library(visdat)


# to understand basic stats about data set
national_skim = skim(national_data)
view(national_skim)

# view(glimpse(national_data)) # shows same thing as view(national_data)


# missing values ---------------------------------------------------------------

# total-------------------------------------------

# to visualize missing values in entire data set ---
vis_miss(national_data, warn_large_data = FALSE) +
  #  hides the column names
  theme(axis.text.x = element_blank())
# removes the little tick marks too
# ,axis.ticks.x = element_blank())
# 50.5% missing data
# 49.5% missing data



# sum count of missing data ---
# sum(is.na(national_data))



# column------------------------------------------

# to see percentage of missing values in each column ---
# look at complete rate
# or look at national_skim we just created
skim(national_data) |> head(40)


# create data set with only columns "variable" and "complete_rate" ---
na_perc = national_skim |>
  select(skim_variable, complete_rate)
view(na_perc)

missing_40plus = na_perc |>
  filter(complete_rate < .6) |>
  arrange((complete_rate))
view(missing_40plus)

missing_less40 = na_perc |>
  filter(complete_rate > .6) |>
  arrange(desc(complete_rate))
view(missing_less40)

class(missing_40plus) 


# --------------- missing 40% plus of data in a column
missing_40plus_description = merge(missing_40plus, data_dict_2025, by.x = "skim_variable", by.y = "Variable Name")
missing_40plus_description = arrange(missing_40plus_description, complete_rate)


no_ci_denom_num_missing40plus = missing_40plus_description |>
  filter(!grepl("cilow", skim_variable) & 
           !grepl("cihigh", skim_variable) &
           !grepl("numerator", skim_variable) &
           !grepl("denominator", skim_variable))
view(no_ci_denom_num_missing40plus)
*
  
  

# nrow(missing_40plus_description) #442
# nrow(no_ci_denom_num_missing40plus) #108
(nrow(no_ci_denom_num_missing40plus)/nrow(missing_40plus_description))*100 
# 24.43% of missing 40% plus is only applicable
# the rest are confidence interval, denom, or num


# --------------- missing 40% less of data in a column
missing_less40_description = merge(missing_less40, data_dict_2025, by.x = "skim_variable", by.y = "Variable Name")
missing_less40_description = arrange(missing_less40_description, complete_rate)

view(missing_less40_description)

no_ci_denom_num_missingless40 = missing_less40_description |>
  filter(!grepl("cilow", skim_variable) & 
           !grepl("cihigh", skim_variable) &
           !grepl("numerator", skim_variable) &
           !grepl("denominator", skim_variable))
view(no_ci_denom_num_missingless40)


# export------------------------------------------------------------------------
# write.csv(no_ci_denom_num_missingless40, "missingless40.csv") 




# ---
# Count the number of Missing Values of each column
view(colSums(is.na(national_data)))
# same output but different library
# sapply(national_data, function(x) sum(is.na(x)))

# row--------------------------------------------

