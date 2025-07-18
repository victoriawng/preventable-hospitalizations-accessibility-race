# merge ========================================================================
# clinical_care_county_level_skim = skim(clinical_care_county_level) |>
#   select(skim_variable, n_missing, complete_rate) |>
#   left_join(data_dict_2025, by = c("skim_variable" = "variable")) |>
#   arrange(complete_rate)
# view(clinical_care_county_level_skim)