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
view(race_biggest_smallest)

race_biggest_smallest <- race_biggest_smallest %>%
  mutate(race_name = race_lookup[as.character(largest_race, smallest_race)])

view(race_biggest_smallest)

race_biggest_smallest |>
  ggplot( aes(x = largest_race_label)) +
  geom_bar() + 
  coord_flip()



# -----------------------------------------------------------------------------------------------------------
# make sure to run EDA_clinicalcare.R for this code to work
# clinical care and race variables
clinical_care_race = clinical_care_no_ci |>
  select(!contains(c("rawvalue", "numerator", "denominator", "other")))
view(clinical_care_race)

# to see missing values
vis_miss(clinical_care_race)

# to see percentage of missing values in each column ---
# look at complete rate
# or create a variable to see better
skim(clinical_care_race)
clinical_care_race_skim = skim(clinical_care_race) |>
  arrange(complete_rate)

view(clinical_care_race_skim)

clinical_care_race_NA = clinical_care_race_skim |>
  select(skim_variable, n_missing, complete_rate)
view(clinical_care_race_NA)


