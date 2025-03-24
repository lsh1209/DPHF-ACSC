# Load necessary libraries
library(tidycensus)
library(dplyr)
library(tidyr)
library(purrr)

# Define variables including poverty rate components
variables <- c(
  total_population = "B01003_001", 
  median_income = "B19013_001", 
  non_hispanic_white = "B03002_003",       # Non-Hispanic White
  non_hispanic_black = "B03002_004",       # Non-Hispanic Black
  hispanic = "B03002_012",                 # Hispanic (any race)
  non_hispanic_asian = "B03002_006",       # Non-Hispanic Asian
  non_hispanic_native_american = "B03002_005", # Non-Hispanic Native American
  non_hispanic_pacific_islander = "B03002_007", # Non-Hispanic Pacific Islander
  non_hispanic_other = "B03002_008",       # Non-Hispanic Other Race
  non_hispanic_two_or_more = "B03002_009", # Non-Hispanic Two or More Races
  total_population_poverty = "B17001_001",  # Population for whom poverty is determined
  below_poverty = "B17001_002", # Individuals below poverty level
  age_18_19 = "B01001_007",             # 18-19 years
  age_20_24 = "B01001_008",             # 20-24 years
  age_25_29 = "B01001_009",             # 25-29 years
  age_30_34 = "B01001_010",             # 30-34 years
  age_35_39 = "B01001_011",             # 35-39 years
  age_40_44 = "B01001_012",             # 40-44 years
  age_45_49 = "B01001_013",             # 45-49 years
  age_50_54 = "B01001_014",             # 50-54 years
  age_55_59 = "B01001_015",             # 55-59 years
  age_60_61 = "B01001_016",             # 60-61 years
  age_62_64 = "B01001_017",             # 62-64 years
  age_65_66 = "B01001_018",             # 65-66 years
  age_67_69 = "B01001_019",             # 67-69 years
  age_70_74 = "B01001_020",             # 70-74 years
  age_75_79 = "B01001_021",             # 75-79 years
  age_80_84 = "B01001_022",             # 80-84 years
  age_85_plus = "B01001_023"
)

# Define years of interest
years <- 2010:2020
states <- c("AZ", "FL", "GA", "KY", "MA", "MN", "NE", "NJ", "NY", "NC", "OR", "WA", "WI")

# Function to fetch ACS data at both county and state levels
get_census_data <- function(year, geography) {
  get_acs(
    geography = geography,
    variables = variables,
    state = if (geography == "state") states else NULL,  # Only specify states for state-level data
    year = year,
    survey = "acs5"
  ) %>%
    mutate(year = year, level = geography)  # Add level column to differentiate county/state data
}

# Fetch both county and state data
census_data <- map_dfr(years, ~{
  bind_rows(
    get_census_data(.x, "county"),  # County-level data
    get_census_data(.x, "state")    # State-level data
  )
})

# Reshape data from long to wide format
census_data_wide <- census_data %>%
  select(GEOID, NAME, variable, estimate, year, level) %>%
  pivot_wider(names_from = variable, values_from = estimate)

# Calculate Poverty Rate
census_data_wide <- census_data_wide %>%
  mutate(Poverty_Rate = (below_poverty / total_population_poverty) * 100)

# Rename columns for clarity
census_data_wide <- census_data_wide %>%
  rename(FIPS_Code = GEOID, Name = NAME)

# View first few rows
head(census_data_wide)

write_xlsx(census_data_wide, "census_county_data.xlsx")
census_county_data<- read_xlsx("census_county_data.xlsx")

##FILTER FOR YEARS AND STATES
census_county_data1<-census_county_data%>% 
  separate(Name, into = c("County", "State"), sep = ", ")
census_county_data2 <-census_county_data1 %>%
  mutate(County = str_remove(County, " County$"))
census_county_filtered <- census_county_data2%>%
  filter(year >= 2016 & year <= 2020)
view(census_county_filtered)

states_to_keep <- c("Arizona", "Florida", "Georgia", "Kentucky", "Nebraska",
                    "North Carolina", "New Jersey", "New York", "Massachusetts",
                    "Minnesota", "Oregon", "Washington", "Wisconsin")

# Filter the data to keep only the rows for these states
census_county_filtered1 <- census_county_filtered %>%
  filter(State %in% states_to_keep)

write_xlsx(census_county_filtered1, "census_county_2016_2020.xlsx")
census_county_data_2016_2020<- read_xlsx("census_county_2016_2020.xlsx")


####2000 Census
variables_2000 <- c(
  total_population = "P001001", 
  non_hispanic_white = "P003001",       # Non-Hispanic White
  non_hispanic_black = "P003002",       # Non-Hispanic Black
  hispanic = "P003005",                 # Hispanic (any race)
  non_hispanic_asian = "P003003",       # Non-Hispanic Asian
  non_hispanic_native_american = "P003004", # Non-Hispanic Native American
  non_hispanic_pacific_islander = "P003006", # Non-Hispanic Pacific Islander
  non_hispanic_other = "P003007",       # Non-Hispanic Other Race
  non_hispanic_two_or_more = "P003008", # Non-Hispanic Two or More Races
  age_18_19 = "P012001",             # 18-19 years
  age_20_24 = "P012002",             # 20-24 years
  age_25_29 = "P012003",             # 25-29 years
  age_30_34 = "P012004",             # 30-34 years
  age_35_39 = "P012005",             # 35-39 years
  age_40_44 = "P012006",             # 40-44 years
  age_45_49 = "P012007",             # 45-49 years
  age_50_54 = "P012008",             # 50-54 years
  age_55_59 = "P012009",             # 55-59 years
  age_60_61 = "P012010",             # 60-61 years
  age_62_64 = "P012011",             # 62-64 years
  age_65_66 = "P012012",             # 65-66 years
  age_67_69 = "P012013",             # 67-69 years
  age_70_74 = "P012014",             # 70-74 years
  age_75_79 = "P012015",             # 75-79 years
  age_80_84 = "P012016",             # 80-84 years
  age_85_plus = "P012017"            # 85+ years
)

get_census_data_2000 <- 
  get_decennial(
    geography = "county",
    variables = variables_2000,
    state = c("AZ","FL","GA","KY","MA","MN","NE","NJ","NY","NC","OR","WA","WI"),  # Only specify states for state-level data
    year = 2000
  ) %>%
    mutate(year = 2000)  

# Reshape data from long to wide format
census_data_wide_2000 <- get_census_data_2000 %>%
  select(GEOID, NAME, variable, value, year) %>%
  pivot_wider(names_from = variable, values_from = value)

# Rename columns for clarity
census_data_wide_2000 <- census_data_wide_2000 %>%
  rename_with(~ paste0(.x, "_00"))

# View first few rows of 2000 data
head(census_data_wide_2000)

census_county_data_00<-census_data_wide_2000%>% 
  separate(NAME_00, into = c("County", "State"), sep = ", ")
census_county_data_002 <-census_county_data_00 %>%
  mutate(County = str_remove(County, " County$"))

# Export data to Excel
write_xlsx(census_county_data_002, "census_county_data_2000.xlsx")
census_county_data_2000<- read_xlsx("census_county_data_2000.xlsx")
colnames(census_county_data_2016_2020)
colnames(census_county_data_2000)
merged_census_county <- left_join(census_county_data_2000,census_county_data_2016_2020, by = c("GEOID_00"="FIPS_Code", "County"="County", "State"="State"))
view(merged_census_county)

write_xlsx(merged_census_county, "census_data_county_all.xlsx")