library(skimr)
library(visdat)

# also selecting columns with our picked features (clinic healthcare)
# plus its race parameters ofc
race = national_data |> 
  select(statecode, countycode, fipscode, state, county, county_clustered,
         contains(c("v051", "v054", "v055", "v056", "v080", "v081", "v126")))
race = race |>
  select(!contains(c("cilow", "cihigh", "v051_num", "v051_denom")))
# view(race)

# grabbing only rawvalues, dropping denom, numerator, etc
# omitted county_clustered
race_rawvalue = race |>
  select(statecode, countycode, fipscode, state, county,
    contains(c("rawvalue")))
# omitted population rawvalue
race_rawvalue = race_rawvalue |>
  select(statecode, countycode, fipscode, state, county,
         !contains(c("v051")))
# view(race_rawvalue)

# little to no missing values :D
# view(skim(race)|>
#        arrange(complete_rate))

# v051: Population - Resident population
# v054: % Non-Hispanic Black - Percentage of population identifying as non-Hispanic Black or African American.
# v055: % American Indian or Alaska Native - Percentage of population identifying as American Indian or Alaska Native.
# v056: % Hispanic - Percentage of population identifying as Hispanic.
# v080: % Native Hawaiian or Other Pacific Islander - Percentage of population identifying as Native Hawaiian or Other Pacific Islander.
# v081: % Asian - Percentage of population identifying as Asian.
# v126: % Non-Hispanic White - Percentage of population identifying as non-Hispanic white.
# -----------------------------------------------------------------------------------------------------------
# returning largest 

# library(dplyr)
# create chracter vector of only the race percentage/rawvalue columns
race_rawvalue_cols = grep("rawvalue", colnames(race_rawvalue), value = TRUE)
race_rawvalue_cols
# the following was accident and made df instead of character string
# race_rawvalue_cols  = race_rawvalue |>
#   select(contains(c("v051", "v054", "v055", "v056", "v080", "v081", "v126")))
# view(race_rawvalue_cols)


race_lookup <- c(
  "v126" = "Non-Hispanic White",
  "v054" = "Non-Hispanic Black",
  "v081" = "Asian",
  "v056" = "Hispanic ",
  "v055" = "American Indian or Alaska Native",
  "v080" = "Native Hawaiian or Other Pacific Islander"
)


# function to extract v### and map to name
get_race_label <- function(column_name) {
  code <- sub("^(v[0-9]+).*", "\\1", column_name)      # extract v###
  race_lookup[[code]] %||% NA_character_           # return mapped name or NA
}





# both rowwise() and ungroup() play key roles in row-by-row calculations in dplyr
# -
# rowwise() tells dplyr to treat each row as an independent group. 
# Normally, dplyr verbs like mutate() operate column-wise (vectorized across all rows). 
# when you need to do calculations within a row, rowwise() switches the behavior 
# so After rowwise(), your tibble is marked as a "rowwise grouped tibble". 
# This can cause unexpected behavior later if you run functions like summarise(), 
# group_by(), or mutate() — they’ll treat each row as a group.
# SUMMARY: Makes operations (like which.max) run per row
# -
# ungroup() clears that grouping and restores your data to a normal ungrouped tibble, 
# so downstream operations behave as expected.that operations are done per row.
# SUMMARY: Removes rowwise status to avoid side effects later

# adding columns of biggest and smallest race and its percentage 
race_biggest_smallest <- race_rawvalue %>%
  # Remove rows where all race_rawvalue columns are NA
  filter(rowSums(is.na(across(all_of(race_rawvalue_cols)))) != length(race_rawvalue_cols))  %>%
  rowwise() %>%
  mutate(
    largest_race   = race_rawvalue_cols[which.max(c_across(all_of(race_rawvalue_cols)))],
    largest_race_label = get_race_label(largest_race),
    largest_pct    = max(c_across(all_of(race_rawvalue_cols))),
    
    
    smallest_race  = race_rawvalue_cols[which.min(c_across(all_of(race_rawvalue_cols)))],
    smallest_race_label = get_race_label(smallest_race),
    smallest_pct   = min(c_across(all_of(race_rawvalue_cols)))
  ) %>%
  ungroup()
# view(race_biggest_smallest)
# race_biggest_smallest <- race_biggest_smallest %>%
#   mutate(race_name = race_lookup[as.character(largest_race, smallest_race)])
# view(race_biggest_smallest)


race_largest = race_biggest_smallest |>
  select(statecode, countycode, fipscode, state, county,
         contains(c("largest"))) |>
  arrange(desc(largest_pct))
# view(race_largest)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
race_largest_70plus = race_largest |>
  filter(largest_pct > .70)
view(race_largest_70plus)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

race_largest_noWhite = race_largest |>
  filter(largest_race_label != "Non-Hispanic White")
# view(race_largest_noWhite)

race_smallest = race_biggest_smallest |>
  select(statecode, countycode, fipscode, state, county,
         contains(c("smallest"))) |>
  arrange(desc(smallest_pct))
# view(race_smallest)


# ----
white_majority_70plus = (race_largest |>
  filter(largest_race_label == "Non-Hispanic White", largest_pct > .70))
# view(white_majority_70plus)
# ----
unique(race_largest_noWhite$largest_race_label)

race_largest_noWhite_70plus = race_largest_noWhite |>
  filter(largest_pct > .70)
# view(race_largest_noWhite_70plus)

# asian_majority_70plus = (race_largest_noWhite_70plus |>
#        filter(largest_race_label == "Asian"))
# view(asian_majority_70plus)

black_majority_70plus = (race_largest_noWhite_70plus |>
       filter(largest_race_label == "Non-Hispanic Black"))
# view(black_majority_70plus)

# hawaiian_majority_70plus = (race_largest_noWhite_70plus |>
#       filter(largest_race_label == "Native Hawaiian or Other Pacific Islander"))
# view(hawaiian_majority_70plus)

native_majority_70plus = (race_largest_noWhite_70plus |>
       filter(largest_race_label == "American Indian or Alaska Native"))
# view(native_majority_70plus)

hispanic_majority_70plus = race_largest_noWhite_70plus |>
  filter(largest_race_label == "Hispanic ")
# view(hispanic_majority_70plus)



# graphs -----------------------------------------------------------------------
# race_biggest_smallest |>
#   ggplot(aes(x = largest_race_label)) +
#   geom_bar() + 
#   coord_flip()
# 
# race_biggest_smallest |>
#   ggplot(aes(x = smallest_race_label)) +
#   geom_bar() + 
#   coord_flip()



            