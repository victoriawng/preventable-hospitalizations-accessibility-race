

analytic_data <- read_csv("analytic_data2025_v2.csv")

view(analytic_data)

trends_data <- read_csv("chr_trends_csv_2025.csv")

view(trends_data)

analytic_data_dt <- as.data.table(analytic_data)

trends_data_dt <- as.data.table(trends_data)

new_analytic_data <- analytic_data |> 
  select(`Income Inequality raw value`, Name, `State Abbreviation`)

view(new_analytic_data |> 
  arrange(`Income Inequality raw value`))

analytic_raw <- analytic_data |> 
  select(`State FIPS Code`, `State Abbreviation`, Name, `County FIPS Code`, `Release Year`, contains("raw value"))


