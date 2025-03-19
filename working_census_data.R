#CENSUS DATA WORKING POPULATION
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
# Variables for desired age groups by race and sex
variables <- c(
  age_18_19_m = "B01001_007E",
  age_20_m = "B01001_008E",
  age_21_m = "B01001_009E",
  age_22_24_m = "B01001_010E",
  age_25_29_m = "B01001_011E",
  age_30_34_m = "B01001_012E",
  age_35_39_m = "B01001_013E",
  age_40_44_m = "B01001_014E",
  age_45_49_m = "B01001_015E",
  age_50_54_m = "B01001_016E",
  age_55_59_m = "B01001_017E",
  age_60_61_m = "B01001_018E",
  age_62_64_m = "B01001_019E",
  age_65_66_m = "B01001_020E",
  age_18_19_f = "B01001_031E",
  age_20_f = "B01001_032E",
  age_21_f = "B01001_033E",
  age_22_24_f = "B01001_034E",
  age_25_29_f = "B01001_035E",
  age_30_34_f = "B01001_036E",
  age_35_39_f = "B01001_037E",
  age_40_44_f = "B01001_038E",
  age_45_49_f = "B01001_039E",
  age_50_54_f = "B01001_040E",
  age_55_59_f = "B01001_041E",
  age_60_61_f = "B01001_042E",
  age_62_64_f = "B01001_043E",
  age_65_66_f = "B01001_044E",
  white_m_18_19 = "B01001A_007",
  white_m_20_24 = "B01001A_008",
  white_m_25_29 = "B01001A_009",
  white_m_30_34 = "B01001A_010",
  white_m_35_44 = "B01001A_011",
  white_m_45_54 = "B01001A_012",
  white_m_55_64 = "B01001A_013",
  white_f_18_19 = "B01001A_022",
  white_f_20_24 = "B01001A_023",
  white_f_25_29 = "B01001A_024",
  white_f_30_34 = "B01001A_025",
  white_f_35_44 = "B01001A_026",
  white_f_45_54 = "B01001A_027",
  white_f_55_64 = "B01001A_028",
  black_m_18_19 = "B01001B_007",
  black_m_20_24 = "B01001B_008",
  black_m_25_29 = "B01001B_009",
  black_m_30_34 = "B01001B_010",
  black_m_35_44 = "B01001B_011",
  black_m_45_54 = "B01001B_012",
  black_m_55_64 = "B01001B_013",
  black_f_18_19 = "B01001B_022",
  black_f_20_24 = "B01001B_023",
  black_f_25_29 = "B01001B_024",
  black_f_30_34 = "B01001B_025",
  black_f_35_44 = "B01001B_026",
  black_f_45_54 = "B01001B_027",
  black_f_55_64 = "B01001B_028",
  na_m_18_19 = "B01001C_007",
  na_m_20_24 = "B01001C_008",
  na_m_25_29 = "B01001C_009",
  na_m_30_34 = "B01001C_010",
  na_m_35_44 = "B01001C_011",
  na_m_45_54 = "B01001C_012",
  na_m_55_64 = "B01001C_013",
  na_f_18_19 = "B01001C_022",
  na_f_20_24 = "B01001C_023",
  na_f_25_29 = "B01001C_024",
  na_f_30_34 = "B01001C_025",
  na_f_35_44 = "B01001C_026",
  na_f_45_54 = "B01001C_027",
  na_f_55_64 = "B01001C_028",
  as_m_18_19 = "B01001D_007",
  as_m_20_24 = "B01001D_008",
  as_m_25_29 = "B01001D_009",
  as_m_30_34 = "B01001D_010",
  as_m_35_44 = "B01001D_011",
  as_m_45_54 = "B01001D_012",
  as_m_55_64 = "B01001D_013",
  as_f_18_19 = "B01001D_022",
  as_f_20_24 = "B01001D_023",
  as_f_25_29 = "B01001D_024",
  as_f_30_34 = "B01001D_025",
  as_f_35_44 = "B01001D_026",
  as_f_45_54 = "B01001D_027",
  as_f_55_64 = "B01001D_028",
  pi_m_18_19 = "B01001E_007",
  pi_m_20_24 = "B01001E_008",
  pi_m_25_29 = "B01001E_009",
  pi_m_30_34 = "B01001E_010",
  pi_m_35_44 = "B01001E_011",
  pi_m_45_54 = "B01001E_012",
  pi_m_55_64 = "B01001E_013",
  pi_f_18_19 = "B01001E_022",
  pi_f_20_24 = "B01001E_023",
  pi_f_25_29 = "B01001E_024",
  pi_f_30_34 = "B01001E_025",
  pi_f_35_44 = "B01001E_026",
  pi_f_45_54 = "B01001E_027",
  pi_f_55_64 = "B01001E_028",
  o_m_18_19 = "B01001F_007",
  o_m_20_24 = "B01001F_008",
  o_m_25_29 = "B01001F_009",
  o_m_30_34 = "B01001F_010",
  o_m_35_44 = "B01001F_011",
  o_m_45_54 = "B01001F_012",
  o_m_55_64 = "B01001F_013",
  o_f_18_19 = "B01001F_022",
  o_f_20_24 = "B01001F_023",
  o_f_25_29 = "B01001F_024",
  o_f_30_34 = "B01001F_025",
  o_f_35_44 = "B01001F_026",
  o_f_45_54 = "B01001F_027",
  o_f_55_64 = "B01001F_028",
  t_m_18_19 = "B01001G_007",
  t_m_20_24 = "B01001G_008",
  t_m_25_29 = "B01001G_009",
  t_m_30_34 = "B01001G_010",
  t_m_35_44 = "B01001G_011",
  t_m_45_54 = "B01001G_012",
  t_m_55_64 = "B01001G_013",
  t_f_18_19 = "B01001G_022",
  t_f_20_24 = "B01001G_023",
  t_f_25_29 = "B01001G_024",
  t_f_30_34 = "B01001G_025",
  t_f_35_44 = "B01001G_026",
  t_f_45_54 = "B01001G_027",
  t_f_55_64 = "B01001G_028",
  h_m_18_19 = "B01001H_007",
  h_m_20_24 = "B01001H_008",
  h_m_25_29 = "B01001H_009",
  h_m_30_34 = "B01001H_010",
  h_m_35_44 = "B01001H_011",
  h_m_45_54 = "B01001H_012",
  h_m_55_64 = "B01001H_013",
  h_f_18_19 = "B01001H_022",
  h_f_20_24 = "B01001H_023",
  h_f_25_29 = "B01001H_024",
  h_f_30_34 = "B01001H_025",
  h_f_35_44 = "B01001H_026",
  h_f_45_54 = "B01001H_027",
  h_f_55_64 = "B01001H_028")
# Pull data from ACS 5-year estimates

# Define years of interest
years<-2016:2020

census_data_16_20w <- map_dfr(years, ~ {
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
view(census_data_16_20w)

# Pivot the data to make it wider for better readability
county_data_16_20w <- census_data_16_20w %>%
  select(GEOID, NAME, Year, variable, estimate) %>%  # Select necessary columns
  pivot_wider(
    names_from = variable,    # Pivot to wide format by variable
    values_from = estimate    # Use the 'estimate' column as the values
  )
view(county_data_16_20w)
county_data_16_20aw <- county_data_16_20w %>% 
  separate(NAME, into = c("County", "State"), sep = ", ")
county_data_16_20bw <-county_data_16_20aw %>%
  mutate(County = str_remove(County, " County$"))

#Create Columns to Match SID Dataset
county_data_16_20cw<- county_data_16_20bw %>%
  mutate(
    males_18_29= B01001_007+B01001_008+B01001_009+B01001_010+B01001_011,
    males_30_44= B01001_012+B01001_013+B01001_014,
    males_45_66= B01001_015+B01001_016+B01001_017+B01001_018+B01001_019+B01001_020,
    females_18_29= B01001_031+B01001_032+B01001_033+B01001_034+B01001_035,
    females_30_44= B01001_036+B01001_037+B01001_038,
    females_45_66= B01001_039+B01001_040+B01001_041+B01001_042+B01001_043+B01001_044,
    white_18_29= white_m_18_19+white_m_20_24+white_m_25_29+white_f_18_19+white_f_20_24+white_f_25_29,
    white_30_44= white_m_30_34+white_m_35_44+white_f_30_34+white_f_35_44,
    white_45_66= white_m_45_54+white_m_55_64+white_f_45_54+white_f_55_64,
    black_18_29= black_m_18_19+black_m_20_24+black_m_25_29,
    black_30_44= black_m_30_34+black_m_35_44+black_f_30_34+black_f_35_44,
    black_45_66= black_m_45_54+black_m_55_64+ black_f_45_54+black_f_55_64,
    na_18_29=na_m_18_19+na_m_20_24+na_m_25_29+na_f_18_19+na_f_20_24+na_f_25_29,
    na_30_44= na_m_30_34+na_m_35_44+na_f_30_34+na_f_35_44,
    na_45_66=na_m_45_54+na_m_55_64+na_f_45_54+na_f_55_64,
    aspi_18_29= as_m_18_19+as_m_20_24+as_m_25_29+as_f_18_19+as_f_20_24+as_f_25_29+
      pi_m_18_19+pi_f_18_19+pi_m_20_24+pi_f_20_24+pi_m_25_29+pi_f_25_29,
    aspi_30_44= as_m_30_34+as_m_35_44+as_f_30_34+as_f_35_44+pi_m_30_34+
      pi_m_35_44+pi_f_30_34+pi_m_35_44,
    aspi_45_66= as_m_45_54+as_m_55_64+as_f_45_54+as_f_55_64+pi_m_45_54+pi_m_55_64,
      pi_f_45_54+pi_f_55_64,
    hispanic_18_29=h_m_18_19+h_m_20_24+h_m_30_34+h_f_18_19+h_f_20_24+h_f_25_29,
    hispanic_30_44= h_m_30_34+h_m_35_44+h_f_30_34+h_f_35_44,
    hispanic_45_66=h_m_45_54+h_m_55_64+h_f_45_54+h_f_55_64,
    other_18_29=o_m_18_19+o_m_20_24+o_m_25_29+o_f_18_19+o_f_25_29+t_m_18_19+
      t_m_20_24+t_m_25_29+t_f_18_19+t_f_20_24+t_f_25_29,
    other_30_44=o_m_30_34+o_m_35_44+o_f_30_34+o_f_35_44+t_m_30_34+t_m_35_44+
      t_f_30_34+t_f_35_44,
    other_45_66=o_m_45_54+o_m_55_64+o_f_45_54+o_f_55_64+t_m_45_54+t_m_55_64+
      t_f_45_54+t_f_55_64)%>% 
  select(GEOID,
    County,
    State,
    Year,
    males_18_29,
    males_30_44,
    males_45_66,
    females_18_29,
    females_30_44,
    females_45_66,
    white_18_29,
    white_30_44,
    white_45_66,
    black_18_29,
    black_30_44,
    black_45_66,
    na_18_29,
    na_30_44,
    na_45_66,
    hispanic_18_29,
    hispanic_30_44,
    hispanic_45_66,
    aspi_18_29,
    aspi_30_44,
    aspi_45_66,
    other_18_29,
    other_30_44,
    other_45_66
    )
view(county_data_16_20cw)


#Save
write_xlsx(county_data_16_20cw,"county_data_16_20finalw.xlsx")
county_data_16_20finalw<-read_xlsx("county_data_16_20finalw.xlsx")

census_data_16_20_Statew<- map_dfr(years, ~ {
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
view(census_data_16_20_Statew)

# Pivot the data to make it wider for better readability
state_data_16_20w <- census_data_16_20_Statew%>%
  select(GEOID, NAME, Year, variable, estimate) %>%  # Select necessary columns
  pivot_wider(
    names_from = variable,    # Pivot to wide format by variable
    values_from = estimate    # Use the 'estimate' column as the values
  )
view(state_data_16_20w)
state_data_16_20aw <- state_data_16_20w %>% 
  rename(
    "State"=NAME)

#Create Columns to Match SID Dataset
state_data_16_20bw<- state_data_16_20aw %>%
  mutate(
    males_18_29= B01001_007+B01001_008+B01001_009+B01001_010+B01001_011,
    males_30_44= B01001_012+B01001_013+B01001_014,
    males_45_66= B01001_015+B01001_016+B01001_017+B01001_018+B01001_019+B01001_020,
    females_18_29= B01001_031+B01001_032+B01001_033+B01001_034+B01001_035,
    females_30_44= B01001_036+B01001_037+B01001_038,
    females_45_66= B01001_039+B01001_040+B01001_041+B01001_042+B01001_043+B01001_044,
    white_18_29= white_m_18_19+white_m_20_24+white_m_25_29+white_f_18_19+white_f_20_24+white_f_25_29,
    white_30_44= white_m_30_34+white_m_35_44+white_f_30_34+white_f_35_44,
    white_45_66= white_m_45_54+white_m_55_64+white_f_45_54+white_f_55_64,
    black_18_29= black_m_18_19+black_m_20_24+black_m_25_29,
    black_30_44= black_m_30_34+black_m_35_44+black_f_30_34+black_f_35_44,
    black_45_66= black_m_45_54+black_m_55_64+ black_f_45_54+black_f_55_64,
    na_18_29=na_m_18_19+na_m_20_24+na_m_25_29+na_f_18_19+na_f_20_24+na_f_25_29,
    na_30_44= na_m_30_34+na_m_35_44+na_f_30_34+na_f_35_44,
    na_45_66=na_m_45_54+na_m_55_64+na_f_45_54+na_f_55_64,
    aspi_18_29= as_m_18_19+as_m_20_24+as_m_25_29+as_f_18_19+as_f_20_24+as_f_25_29+
      pi_m_18_19+pi_f_18_19+pi_m_20_24+pi_f_20_24+pi_m_25_29+pi_f_25_29,
    aspi_30_44= as_m_30_34+as_m_35_44+as_f_30_34+as_f_35_44+pi_m_30_34+
      pi_m_35_44+pi_f_30_34+pi_m_35_44,
    aspi_45_66= as_m_45_54+as_m_55_64+as_f_45_54+as_f_55_64+pi_m_45_54+pi_m_55_64,
    pi_f_45_54+pi_f_55_64,
    hispanic_18_29=h_m_18_19+h_m_20_24+h_m_30_34+h_f_18_19+h_f_20_24+h_f_25_29,
    hispanic_30_44= h_m_30_34+h_m_35_44+h_f_30_34+h_f_35_44,
    hispanic_45_66=h_m_45_54+h_m_55_64+h_f_45_54+h_f_55_64,
    other_18_29=o_m_18_19+o_m_20_24+o_m_25_29+o_f_18_19+o_f_25_29+t_m_18_19+
      t_m_20_24+t_m_25_29+t_f_18_19+t_f_20_24+t_f_25_29,
    other_30_44=o_m_30_34+o_m_35_44+o_f_30_34+o_f_35_44+t_m_30_34+t_m_35_44+
      t_f_30_34+t_f_35_44,
    other_45_66=o_m_45_54+o_m_55_64+o_f_45_54+o_f_55_64+t_m_45_54+t_m_55_64+
      t_f_45_54+t_f_55_64)%>% 
  select(
         State,
         Year,
         males_18_29,
         males_30_44,
         males_45_66,
         females_18_29,
         females_30_44,
         females_45_66,
         white_18_29,
         white_30_44,
         white_45_66,
         black_18_29,
         black_30_44,
         black_45_66,
         na_18_29,
         na_30_44,
         na_45_66,
         hispanic_18_29,
         hispanic_30_44,
         hispanic_45_66,
         aspi_18_29,
         aspi_30_44,
         aspi_45_66,
         other_18_29,
         other_30_44,
         other_45_66
  )

state_data_16_20cw <- state_data_16_20bw %>%
  rename_with(~ paste0(.x, "_state"), -c(Year, State))  

merged_16_20dataw<-county_data_16_20finalw%>%
  left_join(state_data_16_20cw, by=c("State","Year"))

####2000 Census
variables_2000w <- c(
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
  age_65_66 = "P012012"           # 65-66 years
)

get_census_data_2000w <- 
  get_decennial(
    geography= "state",
    variables = variables_2000w,
    year = 2000
  ) %>%
  mutate(Year = 2000)  

# Reshape data from long to wide format
census_data_wide_2000w <- get_census_data_2000w %>%
  select(GEOID, NAME, variable, value, Year) %>%
  pivot_wider(names_from = variable, values_from = value)


view(census_data_wide_2000w)

census_data_wide_2000w= census_data_wide_2000w%>%
  mutate(
    age_18_29_00= age_18_19+age_20_24+age_25_29,
    age_30_44_00= age_30_34+age_35_39+age_40_44,
    age_45_66_00= age_45_49+age_50_54+age_55_59+age_60_61+age_62_64+age_65_66)%>%
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
    - age_65_66)
view(census_data_wide_2000w)

census_state_data_00aw<-census_data_wide_2000w%>%
  select(
    GEOID,
    NAME,
    Year,
    age_18_29_00,
    age_30_44_00,
    age_45_66_00
  )%>%
  rename(
    "State"=NAME
  )

census_state_data_00bw<-census_state_data_00aw%>%
  filter(State %in% c("Arizona","Florida","Georgia","Kentucky","Massachusetts","Minnesota",
                      "Nebraska",
                      "New Jersey",
                      "New York",
                      "North Carolina",
                      "Oregon",
                      "Washington",
                      "Wisconsin"))
#Save
write_xlsx(census_state_data_00bw, "state_data_00finalw.xlsx")
state_data_00finalw<- read_xlsx("state_data_00finalw.xlsx")

##Merge the Datasets to Create one large Census reference dataset 
merged_census_statew <- left_join(merged_16_20dataw, state_data_00finalw,
                                  by = c("State"))
merged_census_statew=merged_census_statew%>%
  select(
    - Year.y,
    -GEOID.y)%>%
  rename(
    "Year"=Year.x,
    "GEOID"=GEOID.x
  )
view(merged_census_statew)
write_xlsx(merged_census_statew, "census_data_allw.xlsx")

#YEAR DATA
census_data_16_20_yearw<- map_dfr(years, ~ {
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
view(census_data_16_20_yearw)

# Pivot the data to make it wider for better readability
year_data_16_20w <- census_data_16_20_yearw%>%
  select(GEOID, NAME, Year, variable, estimate) %>%  # Select necessary columns
  pivot_wider(
    names_from = variable,    # Pivot to wide format by variable
    values_from = estimate    # Use the 'estimate' column as the values
  )

year_data_16_20aw <- year_data_16_20w %>% 
  rename(
    "State"=NAME)


#Create Columns to Match SID Dataset
year_data_16_20bw<- year_data_16_20aw%>%
  mutate(
    males_18_29= B01001_007+B01001_008+B01001_009+B01001_010+B01001_011,
    males_30_44= B01001_012+B01001_013+B01001_014,
    males_45_66= B01001_015+B01001_016+B01001_017+B01001_018+B01001_019+B01001_020,
    females_18_29= B01001_031+B01001_032+B01001_033+B01001_034+B01001_035,
    females_30_44= B01001_036+B01001_037+B01001_038,
    females_45_66= B01001_039+B01001_040+B01001_041+B01001_042+B01001_043+B01001_044,
    white_18_29= white_m_18_19+white_m_20_24+white_m_25_29+white_f_18_19+white_f_20_24+white_f_25_29,
    white_30_44= white_m_30_34+white_m_35_44+white_f_30_34+white_f_35_44,
    white_45_66= white_m_45_54+white_m_55_64+white_f_45_54+white_f_55_64,
    black_18_29= black_m_18_19+black_m_20_24+black_m_25_29,
    black_30_44= black_m_30_34+black_m_35_44+black_f_30_34+black_f_35_44,
    black_45_66= black_m_45_54+black_m_55_64+ black_f_45_54+black_f_55_64,
    na_18_29=na_m_18_19+na_m_20_24+na_m_25_29+na_f_18_19+na_f_20_24+na_f_25_29,
    na_30_44= na_m_30_34+na_m_35_44+na_f_30_34+na_f_35_44,
    na_45_66=na_m_45_54+na_m_55_64+na_f_45_54+na_f_55_64,
    aspi_18_29= as_m_18_19+as_m_20_24+as_m_25_29+as_f_18_19+as_f_20_24+as_f_25_29+
      pi_m_18_19+pi_f_18_19+pi_m_20_24+pi_f_20_24+pi_m_25_29+pi_f_25_29,
    aspi_30_44= as_m_30_34+as_m_35_44+as_f_30_34+as_f_35_44+pi_m_30_34+
      pi_m_35_44+pi_f_30_34+pi_m_35_44,
    aspi_45_66= as_m_45_54+as_m_55_64+as_f_45_54+as_f_55_64+pi_m_45_54+pi_m_55_64,
    pi_f_45_54+pi_f_55_64,
    hispanic_18_29=h_m_18_19+h_m_20_24+h_m_30_34+h_f_18_19+h_f_20_24+h_f_25_29,
    hispanic_30_44= h_m_30_34+h_m_35_44+h_f_30_34+h_f_35_44,
    hispanic_45_66=h_m_45_54+h_m_55_64+h_f_45_54+h_f_55_64,
    other_18_29=o_m_18_19+o_m_20_24+o_m_25_29+o_f_18_19+o_f_25_29+t_m_18_19+
      t_m_20_24+t_m_25_29+t_f_18_19+t_f_20_24+t_f_25_29,
    other_30_44=o_m_30_34+o_m_35_44+o_f_30_34+o_f_35_44+t_m_30_34+t_m_35_44+
      t_f_30_34+t_f_35_44,
    other_45_66=o_m_45_54+o_m_55_64+o_f_45_54+o_f_55_64+t_m_45_54+t_m_55_64+
      t_f_45_54+t_f_55_64)%>%
  select(
    State,
    Year,
    males_18_29,
    males_30_44,
    males_45_66,
    females_18_29,
    females_30_44,
    females_45_66,
    white_18_29,
    white_30_44,
    white_45_66,
    black_18_29,
    black_30_44,
    black_45_66,
    na_18_29,
    na_30_44,
    na_45_66,
    hispanic_18_29,
    hispanic_30_44,
    hispanic_45_66,
    aspi_18_29,
    aspi_30_44,
    aspi_45_66,
    other_18_29,
    other_30_44,
    other_45_66
  )

year_data_16_20cw<- year_data_16_20bw%>%
  mutate(
    males_18_29_year=sum(males_18_29),
    males_30_44_year=sum(males_30_44),
    males_45_66_year=sum(males_45_66),
    females_18_29year=sum(females_18_29),
    females_30_44year=sum(females_30_44),
    females_45_66year=sum(females_45_66),
    white_18_29year=sum(white_18_29),
    white_30_44year=sum(white_30_44),
    white_45_66year=sum(white_45_66),
    black_18_29year=sum(black_18_29),
    black_30_44year=sum(black_30_44),
    black_45_66year=sum(black_45_66),
    na_18_29year=sum(na_18_29),
    na_30_44year=sum(na_30_44),
    na_45_66year=sum(na_45_66),
    aspi_18_29year=sum(aspi_18_29),
    aspi_30_44year=sum(aspi_30_44),
    aspi_45_66year=sum(aspi_45_66),
    hispanic_18_29year=sum(hispanic_18_29),
    hispanic_30_44year=sum(hispanic_30_44),
    hispanic_45_66year=sum(hispanic_45_66),
    other_18_29year=sum(other_18_29),
    other_30_44year=sum(other_30_44),
    other_45_66year=sum(other_45_66)
  )%>%
  select(
    State,
    Year,
    males_18_29_year,
    males_30_44_year,
    males_45_66_year,
    females_18_29year,
    females_30_44year,
    females_45_66year,
    white_18_29year,
    white_30_44year,
    white_45_66year,
    black_18_29year,
    black_30_44year,
    black_45_66year,
    na_18_29year,
    na_30_44year,
    na_45_66year,
    aspi_18_29year,
    aspi_30_44year,
    aspi_45_66year,
    hispanic_18_29year,
    hispanic_30_44year,
    hispanic_45_66year,
    other_18_29year,
    other_30_44year,
    other_45_66year
  )



write_xlsx(year_data_16_20cw, "year_census_data1620w.xlsx")
merged_census_data_statew_distinct <- merged_census_statew%>%
  distinct(GEOID,County, State, Year, .keep_all = TRUE)

merged_census_dataw1<- left_join(merged_census_data_statew_distinct, year_data_16_20cw,
                                  by = c("State","Year"))
write_xlsx(merged_census_dataw1, "merged_census_data1w.xlsx")
county_data_00finalw<- read_xlsx("year_census_data1620w.xlsx")