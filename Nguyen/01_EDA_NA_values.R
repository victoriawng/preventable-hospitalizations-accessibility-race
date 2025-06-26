library(skimr)
library(visdat)


skim(national_data) |> head(30)

national_skim = skim(national_data)
view(national_skim)

# missing values ---------------------------------------------------------------

# total-------------------------------------------
vis_miss(national_data, warn_large_data = FALSE) +
  #  hides the column names
  theme(axis.text.x = element_blank())
# removes the little tick marks too
# ,axis.ticks.x = element_blank())
# 50.5% missing data
# 49.5% missing data

# sum of missing data
sum(is.na(national_data))

# column------------------------------------------
# Count the number of Missing Values of each column
view(colSums(is.na(national_data)))
# same output but different library
# sapply(national_data, function(x) sum(is.na(x)))

na_perc = national_skim |>
  select(skim_variable, complete_rate)

missing_40plus = na_perc |>
  filter(complete_rate < .6)
view(missing_40plus)

missing_less40 = na_perc |>
  filter(complete_rate > .6)
view(missing_less40)

# row--------------------------------------------

