state = national_data |>
# no USA
  filter(countycode == "000", statecode != "00")
view(state)


clinical_care_state_level = state |>
  select(statecode, countycode, fipscode, state, county, county_clustered,
         contains(c("v003", "v004", "v005", "v050", "v062", "v085", "v122", "v088", "v131", "v155", "v166")))
view(clinical_care_state_level)
vis_miss(clinical_care_state_level)

# remove columns with all NA
clinical_care_state_level = clinical_care_state_level |>
  select(statecode, countycode, fipscode, state, county, where(~!all(is.na(.))))

# to see percentage of missing values in each column ---
# look at complete rate
# or create a variable to see better
skim(clinical_care_state_level)
clinical_care_state_level_skim = skim(clinical_care_state_level) |>
  arrange(complete_rate)
view(clinical_care_state_level_skim)


