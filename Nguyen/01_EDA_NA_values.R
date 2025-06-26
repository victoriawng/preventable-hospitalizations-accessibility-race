library(skimr)
library(visdat)


# to understand basic stats about data set
national_skim = skim(national_data)
view(national_skim)



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
sum(is.na(national_data))



# column------------------------------------------

# to see percentage of missing values in each column ---
# look at complete rate
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

missing_40plus_description = merge(missing_40plus, data_dict_2025, by.x = "skim_variable", by.y = "Variable Name")

missing

view(missing_40plus_description)


merge1 = merge(missing_40plus, data_dict_2025)

view(merge1)



# ---
# Count the number of Missing Values of each column
view(colSums(is.na(national_data)))
# same output but different library
# sapply(national_data, function(x) sum(is.na(x)))

# row--------------------------------------------

