state = national_data |>
  # no USA
  filter(countycode == "000", statecode != "00")
# view(state)

USA = national_data |>
  filter(statecode == "00")
# view(USA)