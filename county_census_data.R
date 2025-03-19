library(arrow)
library(tidycensus)
library(tidyverse)
library(dplyr)
library(tidyr)
library(purrr)
library(writexl)
library(readxl)
library(scales)
library(stringr)

#CENSUS COUNTY DATA
# Define variables of interest (modify as needed)
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
  age_85_plus = "B01001_023",            # 85+ years
  total_population_poverty = "B17001_001",  # Population for whom poverty is determined
  below_poverty = "B17001_002",# Individuals below poverty level
  male_pop= "B01001_002",
  female_pop="B01001_026"
)
# Define years of interest
years<-2016:2020

census_data_16_20 <- map_dfr(years, ~ {
  # Use ACS 5-year estimates for 2020 (1-year estimates are unavailable)
  get_acs(
    geography = "county",
    variables = variables, 
    state = c("AZ", "FL", "GA", "KY", "MA", "MN", "NE","NJ","NY", "NC", "OR", "WA", "WI"),
    year = .x,
    survey = "acs5"
  ) %>%      
    mutate(Year = .x)
})
view(census_data_16_20)

# Pivot the data to make it wider for better readability
county_data_16_20 <- census_data_16_20 %>%
  select(GEOID, NAME, Year, variable, estimate) %>%  # Select necessary columns
  pivot_wider(
    names_from = variable,    # Pivot to wide format by variable
    values_from = estimate    # Use the 'estimate' column as the values
  )
view(county_data_16_20)
county_data_16_20a <- county_data_16_20 %>% 
  separate(NAME, into = c("County", "State"), sep = ", ")
county_data_16_20b <-county_data_16_20a %>%
  mutate(County = str_remove(County, " County$"))
county_data_16_20c <- county_data_16_20b %>%
  mutate(Poverty_Rate = (below_poverty / total_population_poverty) * 100)

#Create Columns to Match SID Dataset
county_data_16_20d<- county_data_16_20c %>%
  mutate(
    other = non_hispanic_other+non_hispanic_two_or_more,
    non_hispanic_aspi=non_hispanic_asian+non_hispanic_pacific_islander)%>%
  select(
    - non_hispanic_asian,
    - non_hispanic_pacific_islander,
    - non_hispanic_other,
    - non_hispanic_two_or_more,
    - below_poverty,
    - total_population_poverty)
view(county_data_16_20d)

county_data_16_20e<- county_data_16_20d%>%
  mutate(
    age_18_29= age_18_19+age_20_24+age_25_29,
    age_30_44= age_30_34+age_35_39+age_40_44,
    age_45_66= age_45_49+age_50_54+age_55_59+age_60_61+age_62_64+age_65_66,
    age_67_74= age_67_69+age_70_74,
    age_75_plus= age_75_79+age_80_84+age_85_plus)%>%
  select(
    -age_18_19,
    - age_20_24,
    - age_25_29,
    - age_30_34,
    - age_35_39,
    - age_40_44,
    - age_45_49,
    - age_50_54,
    - age_55_59,
    - age_60_61,
    - age_62_64,
    - age_65_66,
    - age_67_69,
    - age_70_74,
    - age_75_79,
    - age_80_84,
    - age_85_plus)
view(county_data_16_20e)
#Save
write_xlsx(county_data_16_20e,"county_data_16_20final.xlsx")
county_data_16_20final<-read_xlsx("county_data_16_20final.xlsx")

census_data_16_20_State<- map_dfr(years, ~ {
  # Use ACS 5-year estimates for 2020 (1-year estimates are unavailable)
  get_acs(
    geography = "state",
    variables = variables, 
    state = c("AZ", "FL", "GA", "KY", "MA", "MN", "NE","NJ","NY", "NC", "OR", "WA", "WI"),
    year = .x,
    survey = "acs5"
  ) %>%      
    mutate(Year = .x)
})
view(census_data_16_20_State)

# Pivot the data to make it wider for better readability
state_data_16_20 <- census_data_16_20_State%>%
  select(GEOID, NAME, Year, variable, estimate) %>%  # Select necessary columns
  pivot_wider(
    names_from = variable,    # Pivot to wide format by variable
    values_from = estimate    # Use the 'estimate' column as the values
  )
view(state_data_16_20)
state_data_16_20a <- state_data_16_20 %>% 
  rename(
    "State"=NAME)
state_data_16_20b <- state_data_16_20a %>%
  mutate(Poverty_Rate= (below_poverty / total_population_poverty) * 100)

#Create Columns to Match SID Dataset
state_data_16_20c<- state_data_16_20b %>%
  mutate(
    other = non_hispanic_other+non_hispanic_two_or_more,
    non_hispanic_aspi=non_hispanic_asian+non_hispanic_pacific_islander)%>%
  select(
    - non_hispanic_asian,
    - non_hispanic_pacific_islander,
    - non_hispanic_other,
    - non_hispanic_two_or_more,
    - below_poverty,
    - total_population_poverty)
view(state_data_16_20c)

state_data_16_20d<- state_data_16_20c%>%
  mutate(
    age_18_29= age_18_19+age_20_24+age_25_29,
    age_30_44= age_30_34+age_35_39+age_40_44,
    age_45_66= age_45_49+age_50_54+age_55_59+age_60_61+age_62_64+age_65_66,
    age_67_74= age_67_69+age_70_74,
    age_75_plus= age_75_79+age_80_84+age_85_plus)%>%
  select(
    -age_18_19,
    - age_20_24,
    - age_25_29,
    - age_30_34,
    - age_35_39,
    - age_40_44,
    - age_45_49,
    - age_50_54,
    - age_55_59,
    - age_60_61,
    - age_62_64,
    - age_65_66,
    - age_67_69,
    - age_70_74,
    - age_75_79,
    - age_80_84,
    - age_85_plus)
view(state_data_16_20d)

state_data_16_20d <- state_data_16_20d %>%
  rename_with(~ paste0(.x, "_state"), -c(Year, State))  

merged_16_20data<-county_data_16_20final%>%
  left_join(state_data_16_20d, by=c("State","Year"))

####2000 Census
variables_2000 <- c(
  total_population = "P001001", 
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
    geography= "state",
    variables = variables_2000,
    year = 2000
  ) %>%
  mutate(Year = 2000)  

# Reshape data from long to wide format
census_data_wide_2000 <- get_census_data_2000 %>%
  select(GEOID, NAME, variable, value, Year) %>%
  pivot_wider(names_from = variable, values_from = value)


view(census_data_wide_2000)

census_data_wide_2000= census_data_wide_2000%>%
mutate(
  age_18_29= age_18_19+age_20_24+age_25_29,
  age_30_44= age_30_34+age_35_39+age_40_44,
  age_45_66= age_45_49+age_50_54+age_55_59+age_60_61+age_62_64+age_65_66,
  age_67_74= age_67_69+age_70_74,
  age_75_plus= age_75_79+age_80_84+age_85_plus)%>%
  select(
    -age_18_19,
    - age_20_24,
    - age_25_29,
    - age_30_34,
    - age_35_39,
    - age_40_44,
    - age_45_49,
    - age_50_54,
    - age_55_59,
    - age_60_61,
    - age_62_64,
    - age_65_66,
    - age_67_69,
    - age_70_74,
    - age_75_79,
    - age_80_84,
    - age_85_plus)
view(census_data_wide_2000)

census_county_data_00a<-census_data_wide_2000%>%
  mutate(
    Age_18_29_00=sum(age_18_29),
    Age_30_44_00=sum(age_30_44),
    Age_45_66_00=sum(age_45_66),
    Age_67_74_00=sum(age_67_74),
    Age_75_plus_00=sum(age_75_plus),
    total_population_00=sum(total_population)
  )
census_county_data_00b<-census_county_data_00a%>%
  select(
    GEOID,
    NAME,
    Year,
    Age_18_29_00,
    Age_30_44_00,
    Age_45_66_00,
    Age_67_74_00,
    Age_75_plus_00,
    total_population_00
  )%>%
  rename(
    "State"=NAME
  )

census_county_data_00c<-census_county_data_00b%>%
  filter(State %in% c("Arizona","Florida","Georgia","Kentucky","Massachusetts","Minnesota",
                       "Nebraska",
                       "New Jersey",
                      "New York",
                       "North Carolina",
                      "Oregon",
                       "Washington",
                     "Wisconsin"))
#Save
write_xlsx(census_county_data_00c, "county_data_00final.xlsx")
county_data_00final<- read_xlsx("county_data_00final.xlsx")

##Merge the Datasets to Create one large Census reference dataset 
merged_census_county <- left_join(merged_16_20data, county_data_00final,
                                  by = c("State"))
merged_census_county=merged_census_county%>%
  select(
    - Year.y,
    -GEOID.y,
    -GEOID_state)%>%
  rename(
    "Year"=Year.x,
    "GEOID"=GEOID.x
  )
view(merged_census_county)
write_xlsx(merged_census_county, "census_data_county_all.xlsx")

#YEAR DATA
census_data_16_20_year<- map_dfr(years, ~ {
  # Use ACS 5-year estimates for 2020 (1-year estimates are unavailable)
  get_acs(
    geography = "state",
    variables = variables, 
    state = c("AZ", "FL", "GA", "KY", "MA", "MN", "NE","NJ","NY", "NC", "OR", "WA", "WI"),
    year = .x,
    survey = "acs5"
  ) %>%      
    mutate(Year = .x)
})
view(census_data_16_20_year)

# Pivot the data to make it wider for better readability
year_data_16_20 <- census_data_16_20_year%>%
  select(GEOID, NAME, Year, variable, estimate) %>%  # Select necessary columns
  pivot_wider(
    names_from = variable,    # Pivot to wide format by variable
    values_from = estimate    # Use the 'estimate' column as the values
  )

year_data_16_20a <- year_data_16_20 %>% 
  rename(
    "State"=NAME)
year_data_16_20b <- year_data_16_20a %>%
  mutate(Poverty_Rate= (below_poverty / total_population_poverty) * 100)

#Create Columns to Match SID Dataset
year_data_16_20c<- year_data_16_20b %>%
  mutate(
    other = non_hispanic_other+non_hispanic_two_or_more,
    non_hispanic_aspi=non_hispanic_asian+non_hispanic_pacific_islander)%>%
  select(
    - non_hispanic_asian,
    - non_hispanic_pacific_islander,
    - non_hispanic_other,
    - non_hispanic_two_or_more,
    - below_poverty,
    - total_population_poverty)

year_data_16_20d<- year_data_16_20c%>%
  mutate(
    age_18_29= age_18_19+age_20_24+age_25_29,
    age_30_44= age_30_34+age_35_39+age_40_44,
    age_45_66= age_45_49+age_50_54+age_55_59+age_60_61+age_62_64+age_65_66,
    age_67_74= age_67_69+age_70_74,
    age_75_plus= age_75_79+age_80_84+age_85_plus)%>%
  select(
    -age_18_19,
    - age_20_24,
    - age_25_29,
    - age_30_34,
    - age_35_39,
    - age_40_44,
    - age_45_49,
    - age_50_54,
    - age_55_59,
    - age_60_61,
    - age_62_64,
    - age_65_66,
    - age_67_69,
    - age_70_74,
    - age_75_79,
    - age_80_84,
    - age_85_plus)

year_data_16_20e<- year_data_16_20d%>%
  mutate(
    Age_18_29_year=sum(age_18_29),
    Age_30_44_year=sum(age_30_44),
    Age_45_66_year=sum(age_45_66),
    Age_67_74_year=sum(age_67_74),
    Age_75_plus_year=sum(age_75_plus),
    total_population_year=sum(total_population),
    nh_aspi=sum(non_hispanic_aspi),
    nh_black=sum(non_hispanic_black),
    nh_na= sum(non_hispanic_native_american),
    nh_white= sum(non_hispanic_white),
    Other_year= sum(other),
    Hispanic_year= sum(hispanic),
    male_pop_year=sum(male_pop),
    female_pop_year=sum(female_pop)
  )
year_data_16_20e<- year_data_16_20e%>%
  select(
    GEOID,
    State,
    Year,
    Age_18_29_year,
    Age_30_44_year,
    Age_45_66_year,
    Age_67_74_year,
    Age_75_plus_year,
    total_population_year,
    nh_aspi,
    nh_black,
    nh_na,
    nh_white,
    Other_year,
    Hispanic_year,
    male_pop_year,
    female_pop_year
  )



write_xlsx(year_data_16_20e, "year_census_data1620.xlsx")
county_data_00final<- read_xlsx("county_data_00final.xlsx")