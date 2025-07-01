clinical_care = national_data |>
  select(contains(c("v003", "v004", "v005", "v050", "v062", "v085", "v122", "v088", "v131", "v155", "v166")))
view(clinical_care)

# county_clustered equals 1 when the county was included in the Health Groups 
# and equals 0 if the county was not grouped. This variable is set to missing for state and national data.

clinical_care_ONLY = clinical_care |>
  select(!contains(c("race", "cilow", "cihigh")))
view(clinical_care_ONLY)  

race = national_data |>
  select()


no_ci_denom_num = national_data |>
filter(!grepl("cilow", skim_variable) & 
         !grepl("cihigh", skim_variable) &
         !grepl("numerator", skim_variable) &
         !grepl("denominator", skim_variable))