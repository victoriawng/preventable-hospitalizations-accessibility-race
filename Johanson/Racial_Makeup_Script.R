threshold_top1 <- 0.7
threshold_top2 <- 0.6

# Step 1: Long format and rank races by county
race_ranks <- cn_counties_national_subset |>
  pivot_longer(
    cols = all_of(race_cols),
    names_to = "race",
    values_to = "percent"
  ) |>
  group_by(state_fips_code, county_fips_code) |>
  arrange(desc(percent)) |>
  mutate(rank = row_number()) |>
  ungroup()

# Step 2: Get top 1 and top 2 races per county
top_races_summary <- race_ranks |>
  filter(rank <= 2) |>
  group_by(state_fips_code, county_fips_code) |>
  summarise(
    top_races = paste0(
      gsub("percent_|_raw_value", "", race),
      collapse = ", "
    ),
    top1_race = gsub("percent_|_raw_value", "", race[1]),
    top1_percent = percent[1],
    top2_total_percent = sum(percent, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    racial_makeup = case_when(
      top1_percent >= threshold_top1 ~ top1_race,
      top2_total_percent >= threshold_top2 ~ top_races,
      TRUE ~ "no clear racial majority"
    )
  ) |>
  select(state_fips_code, county_fips_code, racial_makeup)

# Step 3: Join back to main data
cn_counties_national_subset <- cn_counties_national_subset |>
  left_join(top_races_summary, by = c("state_fips_code", "county_fips_code"))
