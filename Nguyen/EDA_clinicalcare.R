# state_county_labels = national_data |>
#   select(statecode, countycode, fipscode, state, county) # didn't include county_clustered
# view(state_county_labels)
# county_clustered # included or not in health groups, whatever that is
# county_clustered = 1 #
# county_clustered = 0 # 
# county_clustered = NA # this row is not a county (exp: data for entire state or country)


clinical_care_all = national_data |>
  select(statecode, countycode, fipscode, state, county, county_clustered,
         contains(c("v003", "v004", "v005", "v050", "v062", "v085", "v122", "v088", "v131", "v155", "v166")))
# view(clinical_care_all)
# v003: Uninsured Adults - Percentage of adults under age 65 without health insurance
# v004: Primary Care Physicians (Ratio) - Number of primary care physicians per 100,000 population 
# v005:  Preventable Hospital (Race) - Rate of hospital stays for ambulatory-care sensitive conditions per 100,000 Medicare enrollees.  
# v050: Mammography Screening (Race) - Percentage of female Medicare enrollees ages 65-74 who received an annual mammography screening
# v062: Mental Health Providers (Ratio) - Number of mental health care providers per 100,000 population
# v085: Uninsured - Percentage of population under age 65 without health insurance.
# v122: Uninsured Children - Percentage of children under age 19 without health insurance.
# v088: Dentists (Ratio) - Number of dentists per 100,000 population
# v131: Other Primary Care Providers (Ratio)  - Number of other primary care providers per 100,000 population
# v155: Flu Vaccinations (Race) - Percentage of fee-for-service (FFS) Medicare enrollees who had an annual flu vaccination.
# v166: Broadband Access raw value - Percentage of households with broadband internet connection.


# county_clustered equals 1 when the county was included in the Health Groups 
# and equals 0 if the county was not grouped. This variable is set to missing for state and national data.
clinical_care_no_ci = clinical_care_all |>
  select(!contains(c("cilow", "cihigh")))
# view(clinical_care_no_ci)


clinical_care_rawvalue_num_denom = clinical_care_no_ci |>
  select(!contains(c("race")))
# view(clinical_care_rawvalue_num_denom)




# no_ci_denom_num = national_data |>
# filter(!grepl("cilow", skim_variable) & 
#          !grepl("cihigh", skim_variable) &
#          !grepl("numerator", skim_variable) &
#          !grepl("denominator", skim_variable))
