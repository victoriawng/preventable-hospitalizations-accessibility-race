# preventable hospitalization stays
# is Rate of hospital stays for ambulatory-care sensitive conditions per 100,000 Medicare enrollees.  

# How often Medicare patients are hospitalized for conditions that should usually be 
# treatable or manageable with good outpatient care.

#   "Rate of hospital stays": the number of times people are admitted to the hospital.
# --
#   "Ambulatory-care sensitive conditions": health issues like asthma, diabetes, 
#    high blood pressure, or infections that shouldn't usually require hospitalization 
#    if patients get regular care in clinics or doctor's offices (i.e., “ambulatory care”).
# --
#   "Per 100,000 Medicare enrollees": the number of such hospital stays is calculated 
#    relative to a population of 100,000 people who are enrolled in Medicare, 
#    to make comparisons across different areas fair.


view(white_majority_70plus) #white
view(race_largest_noWhite_70plus) #nonwhite #65 rows

view(national_data)

class(national_data$countycode)


library(dplyr)

view(skim(national_data |>
dplyr::select(statecode, countycode, fipscode, state, county, 
              contains("v005"))))
# Return rows where the 'name' column is not NA
 %>%
  filter(!is.na(name))
print(df_filtered_dplyr)


vis_miss()# population
# v178_rawvalue
# v051_rawvalue

view(clinical_care_rawvalue_num_denom)


# clinical_care_all = national_data |>
#   select(statecode, countycode, fipscode, state, county, county_clustered,
#          contains(c("v003", "v004", "v005", "v050", "v062", "v085", "v122", "v088", "v131", "v155", "v166")))
# view(clinical_care_all)

