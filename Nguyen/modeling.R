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

library(dplyr)
  
preventable_stays = national_data |>
  dplyr::select(statecode, countycode, fipscode, state, county, contains("v005_raw"))



#

# 

view(preventable_stays)
vis_miss(preventable_stays)


dplyr::
  select(national_data, statecode, countycode)

colnames(national_data)

,contains("v003"))

class(national_data)

view(national_data)


clinical_care_all = national_data |>
  select(statecode, countycode, fipscode, state, county, county_clustered,
         contains(c("v003", "v004", "v005", "v050", "v062", "v085", "v122", "v088", "v131", "v155", "v166")))


