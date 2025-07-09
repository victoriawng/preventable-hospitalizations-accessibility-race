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


#rename column name
names(data_dict_2025)[names(data_dict_2025) == "Variable Name"] <- "variable"

clinical_care_race_NA_dict = clinical_care_race_NA |>
  left_join(data_dict_2025, by = c("skim_variable" = "variable"))

view(clinical_care_race_NA_dict)