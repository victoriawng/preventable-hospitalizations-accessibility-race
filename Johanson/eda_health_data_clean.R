source("EDA_import.R")
view(national_data)

national_data_dt <- as.data.table(national_data)

trend_data_dt <- as.data.table(trend_data)

national_raw <- national_data |>
  select(`State FIPS Code`, `State Abbreviation`, Name, `County FIPS Code`, `Release Year`, contains("raw value"))



clean_national_subset <- national_data |> 
  select(
    `State FIPS Code`,
    `State Abbreviation`,
    Name,
    `County FIPS Code`,
    `Flu Vaccinations raw value`,
    `Flu Vaccinations (AIAN)`,
    `Flu Vaccinations (Asian/Pacific Islander)`,
    `Flu Vaccinations (Black)`,
    `Flu Vaccinations (Hispanic)`,
    `Flu Vaccinations (White)`,
    `Primary Care Physicians raw value`,
    `Ratio of population to primary care physicians.`,
    `Mental Health Providers raw value`,
    `Ratio of population to mental health providers.`,
    `Dentists raw value`,
    `Ratio of population to dentists.`,
    `Preventable Hospital Stays raw value`,
    `Preventable Hospital Stays (AIAN)`,
    `Preventable Hospital Stays (Asian/Pacific Islander)`,
    `Preventable Hospital Stays (Black)`,
    `Preventable Hospital Stays (Hispanic)`,
    `Preventable Hospital Stays (White)`,
    `Mammography Screening raw value`,
    `Mammography Screening (AIAN)`,
    `Mammography Screening (Asian/Pacific Islander)`,
    `Mammography Screening (Black)`,
    `Mammography Screening (Hispanic)`,
    `Mammography Screening (White)`,
    `Uninsured raw value`,
    `Uninsured Adults raw value`,
    `Severe Housing Problems raw value`,
    `Percentage of households with overcrowding`,
    `Percentage of households with high housing costs`,
    `Broadband Access raw value`,
    `Library Access raw value`,
    `Some College raw value`,
    `High School Completion raw value`,
    `Unemployment raw value`,
    `Income Inequality raw value`,
    `Children in Poverty raw value`,
    `Children in Poverty (AIAN)`,
    `Children in Poverty (White)`,
    `Children in Poverty (Black)`,
    `Children in Poverty (Hispanic)`,
    `Children in Poverty (Asian/Pacific Islander)`,
    `Child Care Cost Burden raw value`,
    `Child Care Centers raw value`,
    `Frequent Physical Distress raw value`,
    `Frequent Mental Distress raw value`,
    `Limited Access to Healthy Foods raw value`,
    `Food Insecurity raw value`,
    `Insufficient Sleep raw value`,
    `Teen Births raw value`,
    `Teen Births (AIAN)`,
    `Teen Births (Black)`,
    `Teen Births (NHOPI)`,
    `Teen Births (White)`,
    `Teen Births (Asian)`,
    `Teen Births (Hispanic)`,
    `Teen Births (Two or more races)`,
    `Excessive Drinking raw value`,
    `Adult Smoking raw value`,
    `Adult Obesity raw value`,
    `Physical Inactivity raw value`,
    `Uninsured Children raw value`,
    `Other Primary Care Providers raw value`,
    `Ratio of population to primary care providers other than physicians.`,
    `Traffic Volume raw value`,
    `Homeownership raw value`,
    `Severe Housing Cost Burden raw value`,
    `Access to Parks raw value`,
    `Census Participation raw value`,
    `High School Graduation raw value`,
    `Reading Scores raw value`,
    `Reading Scores (AIAN)`,
    `Reading Scores (White)`,
    `Reading Scores (Black)`,
    `Reading Scores (Hispanic)`,
    `Reading Scores (Asian/Pacific Islander)`,
    `Math Scores raw value`,
    `Math Scores (AIAN)`,
    `Math Scores (Black)`,
    `Math Scores (Asian/Pacific Islander)`,
    `Math Scores (Hispanic)`,
    `Math Scores (White)`,
    `School Segregation raw value`,
    `School Funding Adequacy raw value`,
    `Children Eligible for Free or Reduced Price Lunch raw value`,
    `Median Household Income raw value`,
    `Median Household Income (AIAN)`,
    `Median Household Income (Black)`,
    `Median Household Income (White)`,
    `Median household income (Asian)`,
    `Median Household Income (Hispanic)`,
    `Living Wage raw value`,
    `Residential Segregation - Black/White raw value`,
    `Disconnected Youth raw value`,
    `Lack of Social and Emotional Support raw value`,
    `Population raw value`,
    `Children in Single-Parent Households raw value`)

clean_national_subset <- clean_national_subset |> 
  filter(`State FIPS Code` != 'statecode')


library(stringr)



non_county_names <- clean_national_subset |> 
  filter(!str_detect(Name, "County")) |> 
  select(Name)  

  
#Data set excluding US and States
clean_national_subset_counties <- clean_national_subset |> 
  filter(str_detect(Name, "County|Municipality|Census|Borough|Region|District|Parish|city"))


