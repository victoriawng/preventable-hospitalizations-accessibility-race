states = national_data |>
  # no USA
  filter(countycode == "000", statecode != "00")
# view(state)

USA = national_data |>
  filter(statecode == "00")
# view(USA)

no_states = national_data |>
  filter(countycode != "000")
view(no_states)