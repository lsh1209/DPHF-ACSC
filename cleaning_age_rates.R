library(arrow)
library(tidycensus)
library(tidyverse)
library(dplyr)
library(tidyr)
library(purrr)
library(writexl)
library(readxl)
library(scales)
library(openxlsx)

#LOAD CLEANED DATASET FROM CLEANED_DATA.R
load("cleaned_data_1.RData")

##CHANGE STATE NAME FOR EASIER MERGING
cleaned_data_filtered2<-cleaned_data_filtered1%>%
  mutate(State = recode(State,
                        "AZ" = "Arizona",
                        "FL" = "Florida",
                        "GA" = "Georgia",
                        "KY" = "Kentucky",
                        "MA" = "Massachusetts",
                        "MN" = "Minnesota",
                        "NE" = "Nebraska",
                        "NJ" = "New Jersey",
                        "NY" = "New York",
                        "NC" = "North Carolina",
                        "OR" = "Oregon",
                        "WA" = "Washington",
                        "WI" = "Wisconsin"))

##LOAD CENSUS INFORMATION
census_data_all<- read_xlsx("census_data_county_all.xlsx")

##MERGE
colnames(cleaned_data_filtered2)
colnames(census_data_all)

cleaned_merged_data <- left_join(census_data_all, cleaned_data_filtered2, by = c("GEOID"= "FIPS", "State" = "State", "Year"="Year"))

##FURTHER CLEAN AFTER MERGE (CREATING NEW VARIABLES TO SUMMARIZE DATA) 
cleaned_merged_data1 <- cleaned_merged_data %>%
  mutate(
    standard_age_population = case_when(
      `Age Cat.` == "18-29" ~ Age_18_29_00,  
      `Age Cat.` == "30-44" ~ Age_30_44_00,
      `Age Cat.` == "45-66" ~ Age_45_66_00,
      `Age Cat.` == "67-74" ~ Age_67_74_00,
      `Age Cat.` == "75+"   ~ Age_75_plus_00,
      TRUE ~ NA_real_  # For any other categories, assign NA
    ))%>%
  mutate(
    age_population_county = case_when(
      `Age Cat.` == "18-29" ~ age_18_29,  
      `Age Cat.` == "30-44" ~ age_30_44,  
      `Age Cat.` == "45-66" ~ age_45_66,  
      `Age Cat.` == "67-74" ~ age_67_74,  
      `Age Cat.` == "75+" ~ age_75_plus,  
      TRUE ~ NA_real_  # For any other categories, assign NA
    ))%>%
  mutate(
    age_population_state = case_when(
      `Age Cat.` == "18-29" ~ age_18_29_state,  
      `Age Cat.` == "30-44" ~ age_30_44_state,  
      `Age Cat.` == "45-66" ~ age_45_66_state,  
      `Age Cat.` == "67-74" ~ age_67_74_state,  
      `Age Cat.` == "75+" ~ age_75_plus_state,  
      TRUE ~ NA_real_  # For any other categories, assign NA
    ))
cleaned_merged_data2<-cleaned_merged_data1%>%
  select(
    -age_18_29,
    -age_30_44,
    -age_45_66,
    -age_67_74,
    -age_75_plus,
    -Age_18_29_00,
    -Age_30_44_00,
    -Age_45_66_00,
    -Age_67_74_00,
    -Age_75_plus_00,
    -total_population_00,
    -age_18_29_state,
    -age_30_44_state,
    -age_45_66,
    -age_67_74_state,
    -age_75_plus_state)
cleaned_merged_data3<-cleaned_merged_data2%>%
  mutate(
    race_population_county = case_when(
      Race == "Native American" ~ non_hispanic_native_american,  
      Race == "NH White" ~ non_hispanic_white,  
      Race == "NH Black" ~ non_hispanic_black,  
      Race == "Hispanic" ~ hispanic,  
      Race == "NH Asian/PI" ~ non_hispanic_aspi,  
      Race == "Other" ~ other,
      TRUE ~ NA_real_  # For any other categories, assign NA
    ))%>%
  mutate(
    race_population_state = case_when(
      Race == "Native American" ~ non_hispanic_native_american_state,  
      Race == "NH White" ~ non_hispanic_white_state,  
      Race == "NH Black" ~ non_hispanic_black_state,  
      Race == "Hispanic" ~ hispanic_state,  
      Race == "NH Asian/PI" ~ non_hispanic_aspi_state,  
      Race == "Other" ~ other_state,
      TRUE ~ NA_real_  # For any other categories, assign NA
    ))
cleaned_merged_data3=cleaned_merged_data3%>%
  mutate (
    sex_population_county = case_when(
      Sex == "Female"~ female_pop,
      Sex == "Male" ~male_pop,
      TRUE ~ NA_real_  # For any other categories, assign NA
    ))%>%
  mutate (
    sex_population_state = case_when(
      Sex == "Female"~ female_pop_state,
      Sex == "Male" ~male_pop_state,
      TRUE ~ NA_real_  # For any other categories, assign NA
    ))
cleaned_merged_data4 <- cleaned_merged_data3%>%
  select(
    - non_hispanic_white,
    - non_hispanic_black,
    - non_hispanic_native_american,
    - non_hispanic_aspi,
    - hispanic,
    - other,
    - female_pop,
    - male_pop,
    -non_hispanic_white_state,
    -non_hispanic_black_state,
    -non_hispanic_native_american_state,
    -non_hispanic_aspi_state,
    -hispanic_state,
    -other_state,
   - female_pop_state,
   -male_pop_state)
cleaned_merged_final<-cleaned_merged_data4%>%
  select(GEOID, County, State, Year, Age, `Age Cat.`, Sex, Race, `Insurance Type`, `ACSC Hospitalization`,
         `Acute Hospitalization`, `Chronic Hospitalization`, `Diabetes Hospitalization`, 
         total_population, total_population_state, age_population_county,
         age_population_state, standard_age_population,
         sex_population_county, sex_population_state,
         race_population_county, race_population_state,
         median_income, median_income_state, Poverty_Rate, Poverty_Rate_state) #KEEPING AGE FOR TABLE ONE INFORMATION

save(cleaned_merged_final,file ="cleaned_merged_final.RData")
load("cleaned_merged_final.RData")

#RATE CALCULATIONS
# Step 1: Calculate hospitalizations and age populations at the county level
cases_by_age_county <- cleaned_merged_data3 %>%
  group_by(GEOID, County, State, Year, `Age Cat.`, age_population_county, standard_age_population) %>% 
  summarise(
    ACSC_Hosp = sum(`ACSC Hospitalization`, na.rm = TRUE),
    Acute_Hosp = sum(`Acute Hospitalization`, na.rm = TRUE),
    Chronic_Hosp = sum(`Chronic Hospitalization`, na.rm = TRUE),
    Diabetes_Hosp = sum(`Diabetes Hospitalization`, na.rm = TRUE),
    total_standard_age_population = 435079676+27655755+34467220+22688497+25306917, na.rm = TRUE) %>%
  ungroup()

# Step 2: Calculate age-specific rates per each county 
age_specific_rates <- cases_by_age_county %>%
  mutate( 
    Age_specific_rate_acsc_c = ACSC_Hosp / age_population_county * 1000,
    Age_specific_rate_acute_c = Acute_Hosp / age_population_county * 1000,
    Age_specific_rate_chronic_c = Chronic_Hosp / age_population_county * 1000,
    Age_specific_rate_diabetes_c= Diabetes_Hosp / age_population_county * 1000
  ) %>%
  select(GEOID, County, State, Year, `Age Cat.`, Age_specific_rate_acsc_c, Age_specific_rate_acute_c, 
         Age_specific_rate_chronic_c, Age_specific_rate_diabetes_c, standard_age_population
         , total_standard_age_population, age_population_county)

# Step 3: Calculate expected cases using summed standard population
expected_cases <- age_specific_rates %>%
  mutate(
    expected_cases_acsc = Age_specific_rate_acsc_c / 1000 * standard_age_population,
    expected_cases_acute = Age_specific_rate_acute_c / 1000 * standard_age_population,
    expected_cases_chronic = Age_specific_rate_chronic_c / 1000 * standard_age_population,
    expected_cases_diabetes = Age_specific_rate_diabetes_c / 1000 * standard_age_population
  ) %>%
  select(GEOID, County, State, Year, `Age Cat.`, expected_cases_acsc, 
         expected_cases_acute, expected_cases_chronic, expected_cases_diabetes, 
         standard_age_population, total_standard_age_population)

# Step 4: Total expected cases by county
total_expected_cases <- expected_cases %>%
  group_by(GEOID, County, State, Year) %>%
  summarise(
    total_expected_cases_acsc = sum(expected_cases_acsc, na.rm = TRUE),
    total_expected_cases_acute = sum(expected_cases_acute, na.rm = TRUE),
    total_expected_cases_chronic = sum(expected_cases_chronic, na.rm = TRUE),
    total_expected_cases_diabetes = sum(expected_cases_diabetes, na.rm = TRUE)) %>% ungroup()

# Step 5: Calculate age-adjusted rates based on summed expected cases
age_rates_adj <- total_expected_cases %>%
  left_join(expected_cases, by = c("GEOID", "County", "State", "Year")) %>%
  mutate(
    Age_Adjusted_ACSC_Rate_c = (total_expected_cases_acsc / total_standard_age_population) * 1000,
    Age_Adjusted_Acute_Rate_c = (total_expected_cases_acute / total_standard_age_population) * 1000,
    Age_Adjusted_Chronic_Rate_c = (total_expected_cases_chronic / total_standard_age_population) * 1000,
    Age_Adjusted_Diabetes_Rate_c = (total_expected_cases_diabetes / total_standard_age_population) * 1000
  ) %>%
  select(GEOID, County, State, Year, Age_Adjusted_ACSC_Rate_c, Age_Adjusted_Acute_Rate_c, 
         Age_Adjusted_Chronic_Rate_c, Age_Adjusted_Diabetes_Rate_c)

# Step 6: Calculate crude rates at the county level
age_rates_crude <- cases_by_age_county %>%
  group_by(GEOID, County, State, Year) %>%
  summarise(
    Crude_ACSC_Rate_c = sum(ACSC_Hosp, na.rm = TRUE) / sum(age_population_county, na.rm = TRUE) * 1000,
    Crude_Acute_Rate_c = sum(Acute_Hosp, na.rm = TRUE) / sum(age_population_county, na.rm = TRUE) * 1000,
    Crude_Chronic_Rate_c = sum(Chronic_Hosp, na.rm = TRUE) / sum(age_population_county, na.rm = TRUE) * 1000,
    Crude_Diabetes_Rate_c = sum(Diabetes_Hosp, na.rm = TRUE) / sum(age_population_county, na.rm = TRUE) * 1000
  ) %>%
  ungroup()

# Step 7: Combine age-specific rates, crude rates, and adjusted rates
final_rates_county_age <- age_rates_crude %>%
  left_join(age_rates_adj, by = c("GEOID", "County", "State", "Year")) %>%
  left_join(age_specific_rates, by = c("GEOID", "County", "State", "Year"))
final_rates_county_age=final_rates_county_age%>%
  select(
    -total_standard_age_population,
    -age_population_county,
    -standard_age_population
  )

#Create State Rates
# Step 1: Aggregate hospitalizations and age populations at the state level
state_data <- cleaned_merged_data3 %>%
  group_by(State, Year, `Age Cat.`, age_population_state, standard_age_population) %>% 
  summarise(
    ACSC_Hosp = sum(`ACSC Hospitalization`, na.rm = TRUE),
    Acute_Hosp = sum(`Acute Hospitalization`, na.rm = TRUE),
    Chronic_Hosp = sum(`Chronic Hospitalization`, na.rm = TRUE),
    Diabetes_Hosp = sum(`Diabetes Hospitalization`, na.rm = TRUE),
    total_standard_age_population = 435079676+27655755+34467220+22688497+25306917, na.rm = TRUE)%>% ungroup

# Step 2: Calculate age-specific rates for each state
age_specific_rates_state <- state_data %>%
  mutate( 
    Age_specific_rate_acsc_s = ACSC_Hosp / age_population_state * 1000,
    Age_specific_rate_acute_s = Acute_Hosp / age_population_state * 1000,
    Age_specific_rate_chronic_s= Chronic_Hosp / age_population_state * 1000,
    Age_specific_rate_diabetes_s = Diabetes_Hosp / age_population_state * 1000
  ) %>%
  select(State, Year, `Age Cat.`, Age_specific_rate_acsc_s, Age_specific_rate_acute_s, 
         Age_specific_rate_chronic_s, Age_specific_rate_diabetes_s, 
         age_population_state, total_standard_age_population, standard_age_population)

# Step 3: Expected cases
expected_cases_state <- age_specific_rates_state %>%
  mutate(
    expected_cases_acsc = Age_specific_rate_acsc_s / 1000 * standard_age_population,
    expected_cases_acute = Age_specific_rate_acute_s / 1000 * standard_age_population,
    expected_cases_chronic = Age_specific_rate_chronic_s/ 1000 * standard_age_population,
    expected_cases_diabetes = Age_specific_rate_diabetes_s / 1000 * standard_age_population
  ) %>%
  select(State, Year, `Age Cat.`, expected_cases_acsc, 
         expected_cases_acute, expected_cases_chronic, expected_cases_diabetes,
         total_standard_age_population,standard_age_population)

# Step 4: Total expected cases by state
total_expected_cases_state <- expected_cases_state %>%
  group_by(State, Year) %>%
  summarise(
    total_expected_cases_acsc = sum(expected_cases_acsc, na.rm = TRUE),
    total_expected_cases_acute = sum(expected_cases_acute, na.rm = TRUE),
    total_expected_cases_chronic = sum(expected_cases_chronic, na.rm = TRUE),
    total_expected_cases_diabetes = sum(expected_cases_diabetes, na.rm = TRUE)
  ) %>%
  ungroup()

# Step 5: Calculate age-adjusted rates
age_rates_adj_state <- total_expected_cases_state %>%
  left_join(expected_cases_state, by = c("State", "Year")) %>%
  mutate(
    Age_Adjusted_ACSC_Rate_s = (total_expected_cases_acsc / total_standard_age_population) * 1000,
    Age_Adjusted_Acute_Rate_s = (total_expected_cases_acute / total_standard_age_population) * 1000,
    Age_Adjusted_Chronic_Rate_s = (total_expected_cases_chronic / total_standard_age_population) * 1000,
    Age_Adjusted_Diabetes_Rate_s = (total_expected_cases_diabetes / total_standard_age_population) * 1000
  ) %>%
  select(State, Year, Age_Adjusted_ACSC_Rate_s, Age_Adjusted_Acute_Rate_s, 
         Age_Adjusted_Chronic_Rate_s, Age_Adjusted_Diabetes_Rate_s)

# Step 6: Calculate crude rates at the state level
age_rates_crude_state <- state_data %>%
  group_by(State, Year) %>%
  summarise(
    Crude_ACSC_Rate_s = sum(ACSC_Hosp, na.rm = TRUE) / sum(age_population_state, na.rm = TRUE) * 1000,
    Crude_Acute_Rate_s = sum(Acute_Hosp, na.rm = TRUE) / sum(age_population_state, na.rm = TRUE) * 1000,
    Crude_Chronic_Rate_s = sum(Chronic_Hosp, na.rm = TRUE) / sum(age_population_state, na.rm = TRUE) * 1000,
    Crude_Diabetes_Rate_s = sum(Diabetes_Hosp, na.rm = TRUE) / sum(age_population_state, na.rm = TRUE) * 1000
  ) %>%
  ungroup()

# Step 7: Merge the final results into one data frame
final_rates_state_age <- age_rates_crude_state %>%
  left_join(age_rates_adj_state, by = c("State", "Year")) %>%
  left_join(age_specific_rates_state, by = c("State", "Year"))
final_rates_state_age <- final_rates_state_age %>%
  select(
    - age_population_state,
    -total_standard_age_population,
    -standard_age_population
  )

#YEAR RATES (TABLE 1 INFORMATION)
merged_data_year<-cleaned_merged_data3%>%
  left_join(year_data_16_20e, by=c("State","Year"))
merged_data_year1<-merged_data_year%>%
  select(
    -GEOID.y
  )%>%
  rename(
    "GEOID"=GEOID.x
  )
merged_data_year1=merged_data_year1%>%
  left_join(county_data_00final, by=c("State"))
merged_data_year1=merged_data_year1%>%
  select(
    -GEOID.y
  )%>%
  rename(
    "GEOID"=GEOID.x
  )
merged_data_year2 <- merged_data_year1 %>%
  mutate(
    standard_age_population = case_when(
      `Age Cat.` == "18-29" ~ Age_18_29_00,  
      `Age Cat.` == "30-44" ~ Age_30_44_00,
      `Age Cat.` == "45-66" ~ Age_45_66_00,
      `Age Cat.` == "67-74" ~ Age_67_74_00,
      `Age Cat.` == "75+"   ~ Age_75_plus_00,
      TRUE ~ NA_real_  # For any other categories, assign NA
    ))%>%
  mutate(
    age_population_year = case_when(
      `Age Cat.` == "18-29" ~ Age_18_29_year,  
      `Age Cat.` == "30-44" ~ Age_30_44_year,  
      `Age Cat.` == "45-66" ~ Age_45_66_year,  
      `Age Cat.` == "67-74" ~ Age_67_74_year,  
      `Age Cat.` == "75+" ~ Age_75_plus_year,  
      TRUE ~ NA_real_  # For any other categories, assign NA
    ))
merged_data_year3<-merged_data_year2%>%
  select(
    -Age_18_29_year,
    -Age_30_44_year,
    -Age_45_66_year,
    -Age_67_74_year,
    -Age_75_plus_year,
    -Age_18_29_00,
    -Age_30_44_00,
    -Age_45_66_00,
    -Age_67_74_00,
    -Age_75_plus_00,
    -total_population_00)
merged_data_year4<-merged_data_year3%>%
  mutate(
    race_population_year = case_when(
      Race == "Native American" ~ nh_na,  
      Race == "NH White" ~ nh_white,  
      Race == "NH Black" ~ nh_black,  
      Race == "Hispanic" ~ Hispanic_year,  
      Race == "NH Asian/PI" ~ nh_aspi,  
      Race == "Other" ~ Other_year,
      TRUE ~ NA_real_  # For any other categories, assign NA
    ))
merged_data_year5<-merged_data_year4%>%
  mutate (
    sex_population_year = case_when(
      Sex == "Female"~ female_pop_year,
      Sex == "Male" ~male_pop_year,
      TRUE ~ NA_real_  # For any other categories, assign NA
    ))
merged_data_year6<-merged_data_year5%>%
  select(
    - non_hispanic_white,
    - non_hispanic_black,
    - non_hispanic_native_american,
    - non_hispanic_aspi,
    - hispanic,
    - other,
    - female_pop,
    - male_pop,
    -nh_aspi,
    - nh_black,
    -nh_white,
    -nh_na,
    -Hispanic_year,
    -Other_year,
    -female_pop_year,
    -male_pop_year,
    -non_hispanic_aspi_state,
    -non_hispanic_black_state,
    -non_hispanic_native_american_state,
    -non_hispanic_aspi_state,
    -hispanic_state,
    -other_state,
    - female_pop_state,
    -male_pop_state)
merged_data_yearfin<-merged_data_year6%>%
  select(GEOID, County, State, Year.x, `Age Cat.`, Sex, Race, `Insurance Type`, `ACSC Hospitalization`,
         `Acute Hospitalization`, `Chronic Hospitalization`, `Diabetes Hospitalization`, 
         total_population, total_population_year, age_population_year,
         standard_age_population,
         sex_population_year,
         race_population_year,
         median_income, Poverty_Rate)
merged_data_yearfin=merged_data_yearfin%>%
  rename(
    "Year"=Year.x
  )

# Step 1: Aggregate hospitalizations and age populations at year level
year_data<- merged_data_yearfin%>%
  group_by(Year, `Age Cat.`, age_population_year, standard_age_population) %>% 
  summarise(
    ACSC_Hosp = sum(`ACSC Hospitalization`, na.rm = TRUE),
    Acute_Hosp = sum(`Acute Hospitalization`, na.rm = TRUE),
    Chronic_Hosp = sum(`Chronic Hospitalization`, na.rm = TRUE),
    Diabetes_Hosp = sum(`Diabetes Hospitalization`, na.rm = TRUE),
    total_standard_age_population = 435079676+27655755+34467220+22688497+25306917, na.rm = TRUE)
%>% ungroup

# Step 2: Calculate age-specific rates for each year
age_specific_rates_year<-year_data %>%
  mutate( 
    Age_specific_rate_acsc_y = ACSC_Hosp / age_population_year * 1000,
    Age_specific_rate_acute_y = Acute_Hosp / age_population_year * 1000,
    Age_specific_rate_chronic_y = Chronic_Hosp / age_population_year * 1000,
    Age_specific_rate_diabetes_y = Diabetes_Hosp / age_population_year * 1000
  ) %>%
  select(Year, `Age Cat.`, Age_specific_rate_acsc_y, Age_specific_rate_acute_y, 
         Age_specific_rate_chronic_y, Age_specific_rate_diabetes_y, 
         age_population_year, total_standard_age_population, standard_age_population)

# Step 3: Expected cases
expected_cases_year <- age_specific_rates_year %>%
  mutate(
    expected_cases_acsc = Age_specific_rate_acsc_y / 1000 * standard_age_population,
    expected_cases_acute = Age_specific_rate_acute_y/ 1000 * standard_age_population,
    expected_cases_chronic = Age_specific_rate_chronic_y / 1000 * standard_age_population,
    expected_cases_diabetes = Age_specific_rate_diabetes_y / 1000 * standard_age_population
  ) %>%
  select(Year, `Age Cat.`, expected_cases_acsc, 
         expected_cases_acute, expected_cases_chronic, expected_cases_diabetes,
         total_standard_age_population,standard_age_population)

# Step 4: Total expected cases by year
total_expected_cases_year <- expected_cases_year %>%
  group_by(Year) %>%
  summarise(
    total_expected_cases_acsc = sum(expected_cases_acsc, na.rm = TRUE),
    total_expected_cases_acute = sum(expected_cases_acute, na.rm = TRUE),
    total_expected_cases_chronic = sum(expected_cases_chronic, na.rm = TRUE),
    total_expected_cases_diabetes = sum(expected_cases_diabetes, na.rm = TRUE)
  ) %>%
  ungroup()

# Step 5: Calculate age-adjusted rates
age_rates_adj_year <- total_expected_cases_year %>%
  left_join(expected_cases_year, by = c("Year")) %>%
  mutate(
    Age_Adjusted_ACSC_Rate_y = (total_expected_cases_acsc / total_standard_age_population) * 1000,
    Age_Adjusted_Acute_Rate_y = (total_expected_cases_acute / total_standard_age_population) * 1000,
    Age_Adjusted_Chronic_Rate_y = (total_expected_cases_chronic / total_standard_age_population) * 1000,
    Age_Adjusted_Diabetes_Rate_y = (total_expected_cases_diabetes / total_standard_age_population) * 1000
  ) %>%
  select(Year, Age_Adjusted_ACSC_Rate_y, Age_Adjusted_Acute_Rate_y, 
         Age_Adjusted_Chronic_Rate_y, Age_Adjusted_Diabetes_Rate_y)

# Step 6: Calculate crude rates at the year level
age_rates_crude_year <- year_data %>%
  group_by(Year) %>%
  summarise(
    Crude_ACSC_Rate_y= sum(ACSC_Hosp, na.rm = TRUE) / sum(age_population_year, na.rm = TRUE) * 1000,
    Crude_Acute_Rate_y = sum(Acute_Hosp, na.rm = TRUE) / sum(age_population_year, na.rm = TRUE) * 1000,
    Crude_Chronic_Rate_y = sum(Chronic_Hosp, na.rm = TRUE) / sum(age_population_year, na.rm = TRUE) * 1000,
    Crude_Diabetes_Rate_y = sum(Diabetes_Hosp, na.rm = TRUE) / sum(age_population_year,na.rm = TRUE) * 1000
  ) %>%
  ungroup()

# Step 7: Merge the final results into one data frame
final_rates_year_age <- age_rates_crude_year %>%
  left_join(age_rates_adj_year, by = c("Year")) %>%
  left_join(age_specific_rates_year, by = c("Year"))
final_rates_year_age <- final_rates_year_age %>%
  select(
    - age_population_year,
    -total_standard_age_population,
    -standard_age_population
  )

##MERGING ALL RATES TOGETHER TO ONE DATASET
colnames(final_rates_county_age)
colnames(final_rates_state_age)
colnames(final_rates_year_age)

##ENSURE AN EQUAL MERGE 
final_rates_county_age_distinct <- final_rates_county_age %>%
  distinct(County, State, Year, `Age Cat.`, .keep_all = TRUE)
final_rates_state_age_distinct <- final_rates_state_age %>%
  distinct(State, Year, `Age Cat.`, .keep_all = TRUE)

#MERGE ONE
final_data_age_rates <- final_rates_county_age_distinct %>%
  left_join(final_rates_state_age_distinct, by = c("State", "Year", "Age Cat."))

final_rates_year_age_distinct <- final_rates_year_age %>%
  distinct(Year, `Age Cat.`, .keep_all = TRUE)
#FINAL MERGE
cleaned_data_age_rates_fin<-final_data_age_rates%>%
  left_join(final_rates_year_age_distinct, by=c("Year","Age Cat."))

save(cleaned_data_age_rates_fin, file="cleaned_data_age_final.RData")
load("cleaned_data_age_final.RData")

#RATES FOR WORKING POPULATION DATASET
#Working Population Dataset
cleaned_data_filteredw<-cleaned_data_filtered3%>%
  filter(`Age Cat.`%in% c("18-29","30-44","45-66"))

cleaned_merged_dataw <- left_join(merged_census_dataw1, cleaned_data_filteredw,
                                by = c("GEOID"= "FIPS",
                                       "State" = "State", "Year"="Year"))
census_data_allw<-census_data_all%>%
  select(
    GEOID,
    County,
    State,
    Year,
    age_18_29,
    age_30_44,
    age_45_66, 
    age_18_29_state,
    age_30_44_state,
    age_45_66_state
  )
cleaned_merged_dataw1 <- left_join(cleaned_merged_dataw, census_data_allw,
                                  by = c("GEOID"= "GEOID", "County"="County",
                                         "State" = "State", "Year"="Year"))
##FUTHER CLEAN AFTER MERGE
cleaned_merged_dataw2<- cleaned_merged_dataw1 %>%
  mutate(
    standard_age_population = case_when(
      `Age Cat.` == "18-29" ~ age_18_29_00,  
      `Age Cat.` == "30-44" ~ age_30_44_00,
      `Age Cat.` == "45-66" ~ age_45_66_00,
      TRUE ~ NA_real_  # For any other categories, assign NA
    ))%>%
  mutate(
    age_population_county = case_when(
      `Age Cat.` == "18-29" ~ age_18_29,  
      `Age Cat.` == "30-44" ~ age_30_44,  
      `Age Cat.` == "45-66" ~ age_45_66,
      TRUE ~ NA_real_  # For any other categories, assign NA
    ))%>%
  mutate(
    age_population_state = case_when(
      `Age Cat.` == "18-29" ~ age_18_29_state,  
      `Age Cat.` == "30-44" ~ age_30_44_state,  
      `Age Cat.` == "45-66" ~ age_45_66_state,
      TRUE ~ NA_real_  # For any other categories, assign NA
    ))%>%group_by(Year, `Age Cat.`)%>%
  mutate(
    age_population_year= case_when(
      `Age Cat.`=="18-29"~sum(unique(age_18_29_state)),
      `Age Cat.`=="30-44"~sum(unique(age_30_44_state)),
      `Age Cat.`=="45-66"~sum(unique(age_45_66_state)),
      TRUE ~ NA_real_  # For any other categories, assign NA
    ))
cleaned_merged_dataw3<-cleaned_merged_dataw2%>%
  select(
    -age_18_29,
    -age_30_44,
    -age_45_66,
    -age_18_29_00,
    -age_30_44_00,
    -age_45_66_00,
    - age_18_29_state,
    -age_30_44_state,
    -age_45_66_state)
cleaned_merged_dataw4<-cleaned_merged_dataw3%>%
  mutate(race_population_county = case_when(
    Race == "Native American" & `Age Cat.` == "18-29" ~ na_18_29,
    Race == "Native American" & `Age Cat.` == "30-44" ~ na_30_44,
    Race == "Native American" & `Age Cat.` == "45-66" ~ na_45_66,
    Race == "NH White" & `Age Cat.` == "18-29" ~ white_18_29,
    Race == "NH White" & `Age Cat.` == "30-44" ~ white_30_44,
    Race == "NH White" & `Age Cat.` == "45-66" ~ white_45_66,
    Race == "NH Black" & `Age Cat.` == "18-29" ~ black_18_29,
    Race == "NH Black" & `Age Cat.` == "30-44" ~ black_30_44,
    Race == "NH Black" & `Age Cat.` == "45-66" ~ black_45_66,
    Race ==  "NH Asian/PI"& `Age Cat.`=="18-29" ~aspi_18_29,
    Race ==  "NH Asian/PI"& `Age Cat.`=="30-44" ~aspi_30_44,
    Race ==  "NH Asian/PI"& `Age Cat.`=="45-66" ~aspi_45_66,
    Race == "Hispanic" & `Age Cat.` == "18-29" ~ hispanic_18_29,
    Race == "Hispanic" & `Age Cat.` == "30-44" ~ hispanic_30_44,
    Race == "Hispanic" & `Age Cat.` == "45-66" ~ hispanic_45_66,
    Race == "Other" & `Age Cat.` == "18-29" ~ other_18_29,
    Race == "Other" & `Age Cat.` == "30-44" ~ other_30_44,
    Race == "Other" & `Age Cat.` == "45-66" ~ other_45_66,
      TRUE ~ NA_real_  # For any other categories, assign NA
    ))%>%
  mutate(
    race_population_state = case_when(
      Race == "Native American" & `Age Cat.` == "18-29" ~ na_18_29_state,
      Race == "Native American" & `Age Cat.` == "30-44" ~ na_30_44_state,
      Race == "Native American" & `Age Cat.` == "45-66" ~ na_45_66_state,
      Race == "NH White" & `Age Cat.` == "18-29" ~ white_18_29_state,
      Race == "NH White" & `Age Cat.` == "30-44" ~ white_30_44_state,
      Race == "NH White" & `Age Cat.` == "45-66" ~ white_45_66_state,
      Race == "NH Black" & `Age Cat.` == "18-29" ~ black_18_29_state,
      Race == "NH Black" & `Age Cat.` == "30-44" ~ black_30_44_state,
      Race == "NH Black" & `Age Cat.` == "45-66" ~ black_45_66_state,
      Race ==  "NH Asian/PI"& `Age Cat.`=="18-29" ~aspi_18_29_state,
      Race ==  "NH Asian/PI"& `Age Cat.`=="30-44" ~aspi_30_44_state,
      Race ==  "NH Asian/PI"& `Age Cat.`=="45-66" ~aspi_45_66_state,
      Race == "Hispanic" & `Age Cat.` == "18-29" ~ hispanic_18_29_state,
      Race == "Hispanic" & `Age Cat.` == "30-44" ~ hispanic_30_44_state,
      Race == "Hispanic" & `Age Cat.` == "45-66" ~ hispanic_45_66_state,
      Race == "Other" & `Age Cat.` == "18-29" ~ other_18_29_state,
      Race == "Other" & `Age Cat.` == "30-44" ~ other_30_44_state,
      Race == "Other" & `Age Cat.` == "45-66" ~ other_45_66_state,
      TRUE ~ NA_real_  # For any other categories, assign NA
    ))%>%
  mutate(
    race_population_year = case_when(
      Race == "Native American" & `Age Cat.` == "18-29" ~ na_18_29year,
      Race == "Native American" & `Age Cat.` == "30-44" ~ na_30_44year,
      Race == "Native American" & `Age Cat.` == "45-66" ~ na_45_66year,
      Race == "NH White" & `Age Cat.` == "18-29" ~ white_18_29year,
      Race == "NH White" & `Age Cat.` == "30-44" ~ white_30_44year,
      Race == "NH White" & `Age Cat.` == "45-66" ~ white_45_66year,
      Race == "NH Black" & `Age Cat.` == "18-29" ~ black_18_29year,
      Race == "NH Black" & `Age Cat.` == "30-44" ~ black_30_44year,
      Race == "NH Black" & `Age Cat.` == "45-66" ~ black_45_66year,
      Race ==  "NH Asian/PI"& `Age Cat.`=="18-29" ~aspi_18_29year,
      Race ==  "NH Asian/PI"& `Age Cat.`=="30-44" ~aspi_30_44year,
      Race ==  "NH Asian/PI"& `Age Cat.`=="45-66" ~aspi_45_66year,
      Race == "Hispanic" & `Age Cat.` == "18-29" ~ hispanic_18_29year,
      Race == "Hispanic" & `Age Cat.` == "30-44" ~ hispanic_30_44year,
      Race == "Hispanic" & `Age Cat.` == "45-66" ~ hispanic_45_66year,
      Race == "Other" & `Age Cat.` == "18-29" ~ other_18_29year,
      Race == "Other" & `Age Cat.` == "30-44" ~ other_30_44year,
      Race == "Other" & `Age Cat.` == "45-66" ~ other_45_66year,
  ))
cleaned_merged_dataw5<-cleaned_merged_dataw4%>%
  select(
    -na_18_29,-na_30_44,-na_45_66,-na_18_29_state,-na_30_44_state,-na_45_66_state,-na_18_29year,-na_30_44year,-na_45_66year,
    -white_18_29,-white_30_44,-white_45_66,-white_18_29_state,-white_30_44_state,-white_45_66_state,-white_18_29year,-white_30_44year,-white_45_66year,
    -black_18_29,-black_30_44,-black_45_66,-black_18_29_state,-black_30_44_state,-black_45_66_state,-black_18_29year,-black_30_44year,-black_45_66year,
    -aspi_18_29,-aspi_30_44,-aspi_45_66,-aspi_18_29_state,-aspi_30_44_state,-aspi_45_66_state,-aspi_18_29year,-aspi_30_44year,-aspi_45_66year,
    -hispanic_18_29,-hispanic_30_44,-hispanic_45_66,-hispanic_18_29_state,-hispanic_30_44_state,-hispanic_45_66_state,-hispanic_18_29year,-hispanic_30_44year,-hispanic_45_66year,
    -other_18_29,-other_30_44,-other_45_66,-other_18_29_state,-other_30_44_state,-other_45_66_state,-other_18_29year,-other_30_44year,-other_45_66year
  )
cleaned_merged_dataw6=cleaned_merged_dataw5%>%
  mutate (
    sex_population_county = case_when(
      Sex == "Female" & `Age Cat.`=="18-29" ~ females_18_29,
      Sex == "Female" & `Age Cat.`== "30-44" ~ females_30_44,
      Sex == "Female" & `Age Cat.` == "45-66" ~females_45_66,
      Sex == "Male" & `Age Cat.`== "18-29" ~males_18_29,
      Sex == "Male" & `Age Cat.`== "30-44" ~males_30_44,
      Sex == "Male" & `Age Cat.`== "45-66" ~males_45_66,
      TRUE ~ NA_real_  # For any other categories, assign NA
    ))%>%
  mutate (
    sex_population_state = case_when(
      Sex == "Female" & `Age Cat.`=="18-29" ~ females_18_29_state,
      Sex == "Female" & `Age Cat.`== "30-44" ~ females_30_44_state,
      Sex == "Female" & `Age Cat.` == "45-66" ~females_45_66_state,
      Sex == "Male" & `Age Cat.`== "18-29" ~males_18_29_state,
      Sex == "Male" & `Age Cat.`== "30-44" ~males_30_44_state,
      Sex == "Male" & `Age Cat.`== "45-66" ~males_45_66_state,
      TRUE ~ NA_real_  # For any other categories, assign NA
    ))%>%
  mutate (
    sex_population_year = case_when(
      Sex == "Female" & `Age Cat.`=="18-29" ~ females_18_29year,
      Sex == "Female" & `Age Cat.`== "30-44" ~ females_30_44year,
      Sex == "Female" & `Age Cat.` == "45-66" ~females_45_66year,
      Sex == "Male" & `Age Cat.`== "18-29" ~males_18_29_year,
      Sex == "Male" & `Age Cat.`== "30-44" ~males_30_44_year,
      Sex == "Male" & `Age Cat.`== "45-66" ~males_45_66_year,
      TRUE ~ NA_real_  # For any other categories, assign NA
    ))
cleaned_merged_dataw7 <- cleaned_merged_dataw6%>%
  select(
    - females_18_29, -females_30_44,-females_45_66,-females_18_29_state,-females_30_44_state,-females_45_66_state,
    -females_18_29year,-females_30_44year,-females_45_66year,-males_18_29,-males_45_66,-males_30_44,-males_18_29_state,
    -males_30_44_state,-males_45_66_state,-males_18_29_year,-males_30_44_year,-males_45_66_year)
 
#COUNTY LEVEL DATA 
# Step 1: Calculate hospitalizations and age populations at the county level
cases_by_age_countyw <- cleaned_merged_dataw7 %>%
  group_by(GEOID, County, State, Year, `Age Cat.`, age_population_county, standard_age_population) %>% 
  summarise(
    ACSC_Hosp = sum(`ACSC Hospitalization`, na.rm = TRUE),
    Acute_Hosp = sum(`Acute Hospitalization`, na.rm = TRUE),
    Chronic_Hosp = sum(`Chronic Hospitalization`, na.rm = TRUE),
    Diabetes_Hosp = sum(`Diabetes Hospitalization`, na.rm = TRUE),
    total_standard_age_population = 435079676+27655755+34467220, na.rm = TRUE) %>%
  ungroup()

# Step 2: Calculate age-specific rates
age_specific_ratesw <- cases_by_age_countyw %>%
  mutate( 
    Age_specific_rate_acsc_c = ACSC_Hosp / age_population_county * 1000,
    Age_specific_rate_acute_c = Acute_Hosp / age_population_county * 1000,
    Age_specific_rate_chronic_c = Chronic_Hosp / age_population_county * 1000,
    Age_specific_rate_diabetes_c= Diabetes_Hosp / age_population_county * 1000
  ) %>%
  select(GEOID, County, State, Year, `Age Cat.`, Age_specific_rate_acsc_c, Age_specific_rate_acute_c, 
         Age_specific_rate_chronic_c, Age_specific_rate_diabetes_c, standard_age_population
         , total_standard_age_population, age_population_county)

# Step 3: Calculate expected cases using summed standard population
expected_casesw <- age_specific_ratesw %>%
  mutate(
    expected_cases_acsc = Age_specific_rate_acsc_c / 1000 * standard_age_population,
    expected_cases_acute = Age_specific_rate_acute_c / 1000 * standard_age_population,
    expected_cases_chronic = Age_specific_rate_chronic_c / 1000 * standard_age_population,
    expected_cases_diabetes = Age_specific_rate_diabetes_c / 1000 * standard_age_population
  ) %>%
  select(GEOID, County, State, Year, `Age Cat.`, expected_cases_acsc, 
         expected_cases_acute, expected_cases_chronic, expected_cases_diabetes, 
         standard_age_population, total_standard_age_population)

# Step 4: Total expected cases by county
total_expected_casesw <- expected_casesw %>%
  group_by(GEOID, County, State, Year) %>%
  summarise(
    total_expected_cases_acsc = sum(expected_cases_acsc, na.rm = TRUE),
    total_expected_cases_acute = sum(expected_cases_acute, na.rm = TRUE),
    total_expected_cases_chronic = sum(expected_cases_chronic, na.rm = TRUE),
    total_expected_cases_diabetes = sum(expected_cases_diabetes, na.rm = TRUE)) %>% ungroup()

# Step 5: Calculate age-adjusted rates based on summed expected cases
age_rates_adjw <- total_expected_casesw %>%
  left_join(expected_casesw, by = c("GEOID", "County", "State", "Year")) %>%
  mutate(
    Age_Adjusted_ACSC_Rate_c = (total_expected_cases_acsc / total_standard_age_population) * 1000,
    Age_Adjusted_Acute_Rate_c = (total_expected_cases_acute / total_standard_age_population) * 1000,
    Age_Adjusted_Chronic_Rate_c = (total_expected_cases_chronic / total_standard_age_population) * 1000,
    Age_Adjusted_Diabetes_Rate_c = (total_expected_cases_diabetes / total_standard_age_population) * 1000
  ) %>%
  select(GEOID, County, State, Year, Age_Adjusted_ACSC_Rate_c, Age_Adjusted_Acute_Rate_c, 
         Age_Adjusted_Chronic_Rate_c, Age_Adjusted_Diabetes_Rate_c)

# Step 6: Calculate crude rates at the county level
age_rates_crudew <- cases_by_age_countyw %>%
  group_by(GEOID, County, State, Year) %>%
  summarise(
    Crude_ACSC_Rate_c = sum(ACSC_Hosp, na.rm = TRUE) / sum(age_population_county, na.rm = TRUE) * 1000,
    Crude_Acute_Rate_c = sum(Acute_Hosp, na.rm = TRUE) / sum(age_population_county, na.rm = TRUE) * 1000,
    Crude_Chronic_Rate_c = sum(Chronic_Hosp, na.rm = TRUE) / sum(age_population_county, na.rm = TRUE) * 1000,
    Crude_Diabetes_Rate_c = sum(Diabetes_Hosp, na.rm = TRUE) / sum(age_population_county, na.rm = TRUE) * 1000
  ) %>%
  ungroup()

# Step 7: Combine age-specific rates, crude rates, and adjusted rates
final_rates_county_agew <- age_rates_crudew %>%
  left_join(age_rates_adjw, by = c("GEOID", "County", "State", "Year")) %>%
  left_join(age_specific_ratesw, by = c("GEOID", "County", "State", "Year"))
final_rates_county_agew=final_rates_county_agew%>%
  select(
    -total_standard_age_population,
    -age_population_county,
    -standard_age_population
  )

#STATE LEVEL RATES
# Step 1: Aggregate hospitalizations and age populations at the state level
state_dataw <- cleaned_merged_dataw7 %>%
  group_by(State, Year, `Age Cat.`, age_population_state, standard_age_population) %>% 
  summarise(
    ACSC_Hosp = sum(`ACSC Hospitalization`, na.rm = TRUE),
    Acute_Hosp = sum(`Acute Hospitalization`, na.rm = TRUE),
    Chronic_Hosp = sum(`Chronic Hospitalization`, na.rm = TRUE),
    Diabetes_Hosp = sum(`Diabetes Hospitalization`, na.rm = TRUE),
    total_standard_age_population = 435079676+27655755+34467220, na.rm = TRUE)%>% ungroup

# Step 2: Calculate age-specific rates for each state
age_specific_rates_statew <- state_dataw %>%
  mutate( 
    Age_specific_rate_acsc_s = ACSC_Hosp / age_population_state * 1000,
    Age_specific_rate_acute_s = Acute_Hosp / age_population_state * 1000,
    Age_specific_rate_chronic_s= Chronic_Hosp / age_population_state * 1000,
    Age_specific_rate_diabetes_s = Diabetes_Hosp / age_population_state * 1000
  ) %>%
  select(State, Year, `Age Cat.`, Age_specific_rate_acsc_s, Age_specific_rate_acute_s, 
         Age_specific_rate_chronic_s, Age_specific_rate_diabetes_s, 
         age_population_state, total_standard_age_population, standard_age_population)

# Step 3: Expected cases
expected_cases_statew <- age_specific_rates_statew %>%
  mutate(
    expected_cases_acsc = Age_specific_rate_acsc_s / 1000 * standard_age_population,
    expected_cases_acute = Age_specific_rate_acute_s / 1000 * standard_age_population,
    expected_cases_chronic = Age_specific_rate_chronic_s/ 1000 * standard_age_population,
    expected_cases_diabetes = Age_specific_rate_diabetes_s / 1000 * standard_age_population
  ) %>%
  select(State, Year, `Age Cat.`, expected_cases_acsc, 
         expected_cases_acute, expected_cases_chronic, expected_cases_diabetes,
         total_standard_age_population,standard_age_population)

# Step 4: Total expected cases by state
total_expected_cases_statew <- expected_cases_statew %>%
  group_by(State, Year) %>%
  summarise(
    total_expected_cases_acsc = sum(expected_cases_acsc, na.rm = TRUE),
    total_expected_cases_acute = sum(expected_cases_acute, na.rm = TRUE),
    total_expected_cases_chronic = sum(expected_cases_chronic, na.rm = TRUE),
    total_expected_cases_diabetes = sum(expected_cases_diabetes, na.rm = TRUE)
  ) %>%
  ungroup()

# Step 5: Calculate age-adjusted rates
age_rates_adj_statew <- total_expected_cases_statew %>%
  left_join(expected_cases_statew, by = c("State", "Year")) %>%
  mutate(
    Age_Adjusted_ACSC_Rate_s = (total_expected_cases_acsc / total_standard_age_population) * 1000,
    Age_Adjusted_Acute_Rate_s = (total_expected_cases_acute / total_standard_age_population) * 1000,
    Age_Adjusted_Chronic_Rate_s = (total_expected_cases_chronic / total_standard_age_population) * 1000,
    Age_Adjusted_Diabetes_Rate_s = (total_expected_cases_diabetes / total_standard_age_population) * 1000
  ) %>%
  select(State, Year, Age_Adjusted_ACSC_Rate_s, Age_Adjusted_Acute_Rate_s, 
         Age_Adjusted_Chronic_Rate_s, Age_Adjusted_Diabetes_Rate_s)

# Step 6: Calculate crude rates at the state level
age_rates_crude_statew <- state_dataw %>%
  group_by(State, Year) %>%
  summarise(
    Crude_ACSC_Rate_s = sum(ACSC_Hosp, na.rm = TRUE) / sum(age_population_state, na.rm = TRUE) * 1000,
    Crude_Acute_Rate_s = sum(Acute_Hosp, na.rm = TRUE) / sum(age_population_state, na.rm = TRUE) * 1000,
    Crude_Chronic_Rate_s = sum(Chronic_Hosp, na.rm = TRUE) / sum(age_population_state, na.rm = TRUE) * 1000,
    Crude_Diabetes_Rate_s = sum(Diabetes_Hosp, na.rm = TRUE) / sum(age_population_state, na.rm = TRUE) * 1000
  ) %>%
  ungroup()

# Step 7: Merge the final results into one data frame
final_rates_state_agew <- age_rates_crude_statew %>%
  left_join(age_rates_adj_statew, by = c("State", "Year")) %>%
  left_join(age_specific_rates_statew, by = c("State", "Year"))
final_rates_state_agew <- final_rates_state_agew %>%
  select(
    - age_population_state,
    -total_standard_age_population,
    -standard_age_population
  )

#YEAR LEVEL INFORMATION
# Step 1: Aggregate hospitalizations and age populations at year level
year_dataw<- cleaned_merged_dataw7%>%
  group_by(Year, `Age Cat.`, age_population_year, standard_age_population) %>% 
  summarise(
    ACSC_Hosp = sum(`ACSC Hospitalization`, na.rm = TRUE),
    Acute_Hosp = sum(`Acute Hospitalization`, na.rm = TRUE),
    Chronic_Hosp = sum(`Chronic Hospitalization`, na.rm = TRUE),
    Diabetes_Hosp = sum(`Diabetes Hospitalization`, na.rm = TRUE),
    total_standard_age_population = 435079676+27655755+34467220, na.rm = TRUE)%>% ungroup
# Step 2: Calculate age-specific rates for each year
age_specific_rates_yearw<-year_dataw %>%
  mutate( 
    Age_specific_rate_acsc_y = ACSC_Hosp / age_population_year * 1000,
    Age_specific_rate_acute_y = Acute_Hosp / age_population_year * 1000,
    Age_specific_rate_chronic_y = Chronic_Hosp / age_population_year * 1000,
    Age_specific_rate_diabetes_y = Diabetes_Hosp / age_population_year * 1000
  ) %>%
  select(Year, `Age Cat.`, Age_specific_rate_acsc_y, Age_specific_rate_acute_y, 
         Age_specific_rate_chronic_y, Age_specific_rate_diabetes_y, 
         age_population_year, total_standard_age_population, standard_age_population)
# Step 3: Expected cases
expected_cases_yearw <- age_specific_rates_yearw %>%
  mutate(
    expected_cases_acsc = Age_specific_rate_acsc_y / 1000 * standard_age_population,
    expected_cases_acute = Age_specific_rate_acute_y/ 1000 * standard_age_population,
    expected_cases_chronic = Age_specific_rate_chronic_y / 1000 * standard_age_population,
    expected_cases_diabetes = Age_specific_rate_diabetes_y / 1000 * standard_age_population
  ) %>%
  select(Year, `Age Cat.`, expected_cases_acsc, 
         expected_cases_acute, expected_cases_chronic, expected_cases_diabetes,
         total_standard_age_population,standard_age_population)
# Step 4: Total expected cases by state
total_expected_cases_yearw <- expected_cases_yearw %>%
  group_by(Year) %>%
  summarise(
    total_expected_cases_acsc = sum(expected_cases_acsc, na.rm = TRUE),
    total_expected_cases_acute = sum(expected_cases_acute, na.rm = TRUE),
    total_expected_cases_chronic = sum(expected_cases_chronic, na.rm = TRUE),
    total_expected_cases_diabetes = sum(expected_cases_diabetes, na.rm = TRUE)
  ) %>%
  ungroup()

# Step 5: Calculate age-adjusted rates
age_rates_adj_yearw <- total_expected_cases_yearw %>%
  left_join(expected_cases_yearw, by = c("Year")) %>%
  mutate(
    Age_Adjusted_ACSC_Rate_y = (total_expected_cases_acsc / total_standard_age_population) * 1000,
    Age_Adjusted_Acute_Rate_y = (total_expected_cases_acute / total_standard_age_population) * 1000,
    Age_Adjusted_Chronic_Rate_y = (total_expected_cases_chronic / total_standard_age_population) * 1000,
    Age_Adjusted_Diabetes_Rate_y = (total_expected_cases_diabetes / total_standard_age_population) * 1000
  ) %>%
  select(Year, Age_Adjusted_ACSC_Rate_y, Age_Adjusted_Acute_Rate_y, 
         Age_Adjusted_Chronic_Rate_y, Age_Adjusted_Diabetes_Rate_y)

# Step 6: Calculate crude rates at the year level
age_rates_crude_yearw <- year_dataw %>%
  group_by(Year) %>%
  summarise(
    Crude_ACSC_Rate_y= sum(ACSC_Hosp, na.rm = TRUE) / sum(age_population_year, na.rm = TRUE) * 1000,
    Crude_Acute_Rate_y = sum(Acute_Hosp, na.rm = TRUE) / sum(age_population_year, na.rm = TRUE) * 1000,
    Crude_Chronic_Rate_y = sum(Chronic_Hosp, na.rm = TRUE) / sum(age_population_year, na.rm = TRUE) * 1000,
    Crude_Diabetes_Rate_y = sum(Diabetes_Hosp, na.rm = TRUE) / sum(age_population_year,na.rm = TRUE) * 1000
  ) %>%
  ungroup()

# Step 7: Merge the final results into one data frame
final_rates_year_agew<- age_rates_crude_yearw %>%
  left_join(age_rates_adj_yearw, by = c("Year")) %>%
  left_join(age_specific_rates_yearw, by = c("Year"))
final_rates_year_agew <- final_rates_year_agew %>%
  select(
    - age_population_year,
    -total_standard_age_population,
    -standard_age_population
  )

#MERGING 
colnames(final_rates_county_agew)
colnames(final_rates_state_agew)
colnames(final_rates_year_agew)

#ENSURE EQUAL MERGE
final_rates_county_age_distinctw <- final_rates_county_agew %>%
  distinct(County, State, Year, `Age Cat.`, .keep_all = TRUE)
final_rates_state_age_distinctw <- final_rates_state_agew %>%
  distinct(State, Year, `Age Cat.`, .keep_all = TRUE)
#MERGE ONE
final_data_age_ratesw <- final_rates_county_age_distinctw %>%
  left_join(final_rates_state_age_distinctw, by = c("State", "Year", "Age Cat."))

final_rates_year_age_distinctw <- final_rates_year_agew %>%
  distinct(Year, `Age Cat.`, .keep_all = TRUE)
#FINAL MERGE
cleaned_data_age_rates_finw<-final_data_age_ratesw%>%
  left_join(final_rates_year_age_distinctw, by=c("Year","Age Cat."))

save(cleaned_data_age_rates_finw, file="cleaned_data_age_finalw.RData")
load("cleaned_data_age_finalw.RData")


#GRAPHS
#ACSC
ggplot(cleaned_data_age_rates_fin, aes(x=Year))+
  geom_line(aes(y=Crude_ACSC_Rate_y,color="Crude Rate"),size=.75)+
  geom_line(aes(y=Age_Adjusted_ACSC_Rate_y,color="Age-Adjusted Rate"),size=.75, linetype=
              "dashed")+
  geom_point(aes(y=Crude_ACSC_Rate_y, color="Crude Rate"),size=1)+
  geom_point(aes(y=Age_Adjusted_ACSC_Rate_y, color="Age-Adjusted Rate"),size=1)+
  labs(
    title = "ACSC Hospitalization Rates from 2016-2020",
    subtitle="Total Adult Population (18+)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Rate Type",
    caption= "Data Source: Age-Adjusted Rates used 2000 US Census Population"
  ) +
  theme_minimal()

##WORKING
ggplot(cleaned_data_age_rates_finw, aes(x=Year))+
  geom_line(aes(y=Crude_ACSC_Rate_y,color="Crude Rate"),size=.75)+
  geom_line(aes(y=Age_Adjusted_ACSC_Rate_y,color="Age-Adjusted Rate"),size=.75, linetype=
              "dashed")+
  geom_point(aes(y=Crude_ACSC_Rate_y, color="Crude Rate"),size=1)+
  geom_point(aes(y=Age_Adjusted_ACSC_Rate_y, color="Age-Adjusted Rate"),size=1)+
  labs(
    title = "ACSC Hospitalization Rates from 2016-2020",
    subtitle="Total Working Population (18-66)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Rate Type",
    caption= "Data Source: Age-Adjusted Rates used 2000 US Census Population"
  ) +scale_y_continuous(expand=c(0,.25))+
  theme_minimal()

#Acute
ggplot(cleaned_data_age_rates_fin, aes(x=Year))+
  geom_line(aes(y=Crude_Acute_Rate_y,color="Crude Rate"),size=.75)+
  geom_line(aes(y=Age_Adjusted_Acute_Rate_y,color="Age-Adjusted Rate"),size=.75, linetype=
              "dashed")+
  geom_point(aes(y=Crude_Acute_Rate_y, color="Crude Rate"),size=1)+
  geom_point(aes(y=Age_Adjusted_Acute_Rate_y, color="Age-Adjusted Rate"),size=1)+
  labs(
    title = "Acute Hospitalization Rates from 2016-2020",
    subtitle="Total Adult Population (18+)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Rate Type",
    caption= "Data Source: Age-Adjusted Rates used 2000 US Census Population"
  ) +
  theme_minimal()

##WORKING
ggplot(cleaned_data_age_rates_finw, aes(x=Year))+
  geom_line(aes(y=Crude_Acute_Rate_y,color="Crude Rate"),size=.75)+
  geom_line(aes(y=Age_Adjusted_Acute_Rate_y,color="Age-Adjusted Rate"),size=.75, linetype=
              "dashed")+
  geom_point(aes(y=Crude_Acute_Rate_y, color="Crude Rate"),size=1)+
  geom_point(aes(y=Age_Adjusted_Acute_Rate_y, color="Age-Adjusted Rate"),size=1)+
  labs(
    title = "Acute Hospitalization Rates from 2016-2020",
    subtitle="Total Working Population (18-66)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Rate Type",
    caption= "Data Source: Age-Adjusted Rates used 2000 US Census Population"
  ) +scale_y_continuous(expand=c(0,.1))+
  theme_minimal()
#Chronic
ggplot(cleaned_data_age_rates_fin, aes(x=Year))+
  geom_line(aes(y=Crude_Chronic_Rate_y,color="Crude Rate"),size=.75)+
  geom_line(aes(y=Age_Adjusted_Chronic_Rate_y,color="Age-Adjusted Rate"),size=.75, linetype=
              "dashed")+
  geom_point(aes(y=Crude_Chronic_Rate_y, color="Crude Rate"),size=1)+
  geom_point(aes(y=Age_Adjusted_Chronic_Rate_y, color="Age-Adjusted Rate"),size=1)+
  labs(
    title = "Chronic Hospitalization Rates from 2016-2020",
    subtitle="Total Adult Population (18+)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Rate Type",
    caption= "Data Source: Age-Adjusted Rates used 2000 US Census Population"
  ) +
  theme_minimal()

##WORKING
ggplot(cleaned_data_age_rates_finw, aes(x=Year))+
  geom_line(aes(y=Crude_Chronic_Rate_y,color="Crude Rate"),size=.75)+
  geom_line(aes(y=Age_Adjusted_Chronic_Rate_y,color="Age-Adjusted Rate"),size=.75, linetype=
              "dashed")+
  geom_point(aes(y=Crude_Chronic_Rate_y, color="Crude Rate"),size=1)+
  geom_point(aes(y=Age_Adjusted_Chronic_Rate_y, color="Age-Adjusted Rate"),size=1)+
  labs(
    title = "Chronic Hospitalization Rates from 2016-2020",
    subtitle="Total Working Population (18-66)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Rate Type",
    caption= "Data Source: Age-Adjusted Rates used 2000 US Census Population"
  ) +scale_y_continuous(expand=c(0,.1))+
  theme_minimal()
#Diabetes
ggplot(cleaned_data_age_rates_fin, aes(x=Year))+
  geom_line(aes(y=Crude_Diabetes_Rate_y,color="Crude Rate"),size=.75)+
  geom_line(aes(y=Age_Adjusted_Diabetes_Rate_y,color="Age-Adjusted Rate"),size=.75, linetype=
              "dashed")+
  geom_point(aes(y=Crude_Diabetes_Rate_y, color="Crude Rate"),size=1)+
  geom_point(aes(y=Age_Adjusted_Diabetes_Rate_y, color="Age-Adjusted Rate"),size=1)+
  labs(
    title = "Diabetes Hospitalization Rates from 2016-2020",
    subtitle="Total Adult Population (18+)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Rate Type",
    caption= "Data Source: Age-Adjusted Rates used 2000 US Census Population"
  ) +
  theme_minimal()

##WORKING
ggplot(cleaned_data_age_rates_finw, aes(x=Year))+
  geom_line(aes(y=Crude_Diabetes_Rate_y,color="Crude Rate"),size=.75)+
  geom_line(aes(y=Age_Adjusted_Diabetes_Rate_y,color="Age-Adjusted Rate"),size=.75, linetype=
              "dashed")+
  geom_point(aes(y=Crude_Diabetes_Rate_y, color="Crude Rate"),size=1)+
  geom_point(aes(y=Age_Adjusted_Diabetes_Rate_y, color="Age-Adjusted Rate"),size=1)+
  labs(
    title = "Diabetes Hospitalization Rates from 2016-2020",
    subtitle="Total Working Population (18-66)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Rate Type",
    caption= "Data Source: Age-Adjusted Rates used 2000 US Census Population"
  ) +scale_y_continuous(expand=c(0,.1))+
  theme_minimal()

#GRAPHS PT2
ggplot(cleaned_data_age_rates_fin, aes(x = Year)) +
  geom_line(aes(y = Crude_ACSC_Rate_s, color = "Crude Rate"), size = .75) +
  geom_line(aes(y = Age_Adjusted_ACSC_Rate_s, color = "Age-Adjusted Rate"), size = .75, linetype = "dashed") +
  geom_point(aes(y=Crude_ACSC_Rate_s, color="Crude Rate"), size=1)+
  geom_point(aes(y= Age_Adjusted_ACSC_Rate_s, color="Age-Adjusted Rate"), size=1)+
  facet_wrap(~ State) +  # Separate plots for each state
  labs(
    title = "ACSC Hospitalization Rates from 2016-2020",
    subtitle="Total Adult Population (18+)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Rate Type",
    caption= "Data Source: Age-Adjusted Rates used 2000 US Census Population"
  ) +
  theme_minimal()

ggplot(cleaned_data_age_rates_finw, aes(x = Year)) +
  geom_line(aes(y = Crude_ACSC_Rate_s, color = "Crude Rate"), size = .75) +
  geom_line(aes(y = Age_Adjusted_ACSC_Rate_s, color = "Age-Adjusted Rate"), size = .75, linetype = "dashed") +
  geom_point(aes(y=Crude_ACSC_Rate_s, color="Crude Rate"), size=1)+
  geom_point(aes(y= Age_Adjusted_ACSC_Rate_s, color="Age-Adjusted Rate"), size=1)+
  facet_wrap(~ State) +  # Separate plots for each state
  labs(
    title = "ACSC Hospitalization Rates from 2016-2020",
    subtitle="Total Working Population (18-66)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Rate Type",
    caption= "Data Source: Age-Adjusted Rates used 2000 US Census Population"
  ) +
  theme_minimal()
##ACUTE
ggplot(cleaned_data_age_rates_fin, aes(x = Year)) +
  geom_line(aes(y = Crude_Acute_Rate_s, color = "Crude Rate"), size = .75) +
  geom_line(aes(y = Age_Adjusted_Acute_Rate_s, color = "Age-Adjusted Rate"), size = .75, linetype = "dashed") +
  geom_point(aes(y=Crude_Acute_Rate_s, color="Crude Rate"), size=1)+
  geom_point(aes(y= Age_Adjusted_Acute_Rate_s, color="Age-Adjusted Rate"), size=1)+
  facet_wrap(~ State) +  # Separate plots for each state
  labs(
    title = "Acute Hospitalization Rates from 2016-2020",
    subtitle="Total Adult Population (18+)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Rate Type",
    caption= "Data Source: Age-Adjusted Rates used 2000 US Census Population"
  ) +
  theme_minimal()

ggplot(cleaned_data_age_rates_finw, aes(x = Year)) +
  geom_line(aes(y = Crude_Acute_Rate_s, color = "Crude Rate"), size = .75) +
  geom_line(aes(y = Age_Adjusted_Acute_Rate_s, color = "Age-Adjusted Rate"), size = .75, linetype = "dashed") +
  geom_point(aes(y=Crude_Acute_Rate_s, color="Crude Rate"), size=1)+
  geom_point(aes(y= Age_Adjusted_Acute_Rate_s, color="Age-Adjusted Rate"), size=1)+
  facet_wrap(~ State) +  # Separate plots for each state
  labs(
    title = "Acute Hospitalization Rates from 2016-2020",
    subtitle="Total Working Population (18-66)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Rate Type",
    caption= "Data Source: Age-Adjusted Rates used 2000 US Census Population"
  ) +
  theme_minimal()
##CHRONIC
ggplot(cleaned_data_age_rates_fin, aes(x = Year)) +
  geom_line(aes(y = Crude_Chronic_Rate_s, color = "Crude Rate"), size = .75) +
  geom_line(aes(y = Age_Adjusted_Chronic_Rate_s, color = "Age-Adjusted Rate"), size = .75, linetype = "dashed") +
  geom_point(aes(y=Crude_Chronic_Rate_s, color="Crude Rate"), size=1)+
  geom_point(aes(y= Age_Adjusted_Chronic_Rate_s, color="Age-Adjusted Rate"), size=1)+
  facet_wrap(~ State) +  # Separate plots for each state
  labs(
    title = "Chronic Hospitalization Rates from 2016-2020",
    subtitle="Total Adult Population (18+)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Rate Type",
    caption= "Data Source: Age-Adjusted Rates used 2000 US Census Population"
  ) +
  theme_minimal()

ggplot(cleaned_data_age_rates_finw, aes(x = Year)) +
  geom_line(aes(y = Crude_Chronic_Rate_s, color = "Crude Rate"), size = .75) +
  geom_line(aes(y = Age_Adjusted_Chronic_Rate_s, color = "Age-Adjusted Rate"), size = .75, linetype = "dashed") +
  geom_point(aes(y=Crude_Chronic_Rate_s, color="Crude Rate"), size=1)+
  geom_point(aes(y= Age_Adjusted_Chronic_Rate_s, color="Age-Adjusted Rate"), size=1)+
  facet_wrap(~ State) +  # Separate plots for each state
  labs(
    title = "Chronic Hospitalization Rates from 2016-2020",
    subtitle="Total Working Population (18-66)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Rate Type",
    caption= "Data Source: Age-Adjusted Rates used 2000 US Census Population"
  ) +
  theme_minimal()

##DIABETES
ggplot(cleaned_data_age_rates_fin, aes(x = Year)) +
  geom_line(aes(y = Crude_Diabetes_Rate_s, color = "Crude Rate"), size = .75) +
  geom_line(aes(y = Age_Adjusted_Diabetes_Rate_s, color = "Age-Adjusted Rate"), size = .75, linetype = "dashed") +
  geom_point(aes(y=Crude_Diabetes_Rate_s, color="Crude Rate"), size=1)+
  geom_point(aes(y= Age_Adjusted_Diabetes_Rate_s, color="Age-Adjusted Rate"), size=1)+
  facet_wrap(~ State) +  # Separate plots for each state
  labs(
    title = "Diabetes Hospitalization Rates from 2016-2020",
    subtitle="Total Adult Population (18+)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Rate Type",
    caption= "Data Source: Age-Adjusted Rates used 2000 US Census Population"
  ) +
  theme_minimal()

ggplot(cleaned_data_age_rates_finw, aes(x = Year)) +
  geom_line(aes(y = Crude_Diabetes_Rate_s, color = "Crude Rate"), size = .75) +
  geom_line(aes(y = Age_Adjusted_Diabetes_Rate_s, color = "Age-Adjusted Rate"), size = .75, linetype = "dashed") +
  geom_point(aes(y=Crude_Diabetes_Rate_s, color="Crude Rate"), size=1)+
  geom_point(aes(y= Age_Adjusted_Diabetes_Rate_s, color="Age-Adjusted Rate"), size=1)+
  facet_wrap(~ State) +  # Separate plots for each state
  labs(
    title = "Diabetes Hospitalization Rates from 2016-2020",
    subtitle="Total Working Population (18-66)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Rate Type",
    caption= "Data Source: Age-Adjusted Rates used 2000 US Census Population"
  ) +
  theme_minimal()


#CRUDE RATES GRAPH 
##ACSC
ggplot(cleaned_data_age_rates_fin %>% 
         filter(!is.na(Age_specific_rate_acsc_s), !is.na(`Age Cat.`)), 
       aes(x = Year, y = Age_specific_rate_acsc_s, color = `Age Cat.`)) +
  geom_line(size = 0.75) +
  geom_point(size = 1) +
  facet_wrap(~ State) +  # Separate plots for each state
  labs(
    title = "ACSC Hospitalization Rates from 2016-2020",
    subtitle = "Total Adult Population (18+)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Age Category",
    caption = "Data Source: Age-Adjusted Rates used 2000 US Census Population"
  ) +
  theme_minimal()

ggplot(cleaned_data_age_rates_finw %>% 
         filter(!is.na(Age_specific_rate_acsc_s), !is.na(`Age Cat.`)), 
       aes(x = Year, y = Age_specific_rate_acsc_s, color = `Age Cat.`)) +
  geom_line(size = 0.75) +
  geom_point(size = 1) +
  facet_wrap(~ State) +  # Separate plots for each state
  labs(
    title = "ACSC Hospitalization Rates from 2016-2020",
    subtitle = "Total Working Population (18-66)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Age Category",
    caption = "Data Source: Age-Adjusted Rates used 2000 US Census Population"
  ) +
  theme_minimal()
##Acute
ggplot(cleaned_data_age_rates_fin %>% 
         filter(!is.na(Age_specific_rate_acute_s), !is.na(`Age Cat.`)), 
       aes(x = Year, y = Age_specific_rate_acute_s, color = `Age Cat.`)) +
  geom_line(size = 0.75) +
  geom_point(size = 1) +
  facet_wrap(~ State) +  # Separate plots for each state
  labs(
    title = "Acute Hospitalization Rates from 2016-2020",
    subtitle = "Total Adult Population (18+)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Age Category",
    caption = "Data Source: Age-Adjusted Rates used 2000 US Census Population"
  ) +
  theme_minimal()

ggplot(cleaned_data_age_rates_finw %>% 
         filter(!is.na(Age_specific_rate_acute_s), !is.na(`Age Cat.`)), 
       aes(x = Year, y = Age_specific_rate_acute_s, color = `Age Cat.`)) +
  geom_line(size = 0.75) +
  geom_point(size = 1) +
  facet_wrap(~ State) +  # Separate plots for each state
  labs(
    title = "Acute Hospitalization Rates from 2016-2020",
    subtitle = "Total Working Population (18-66)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Age Category",
    caption = "Data Source: Age-Adjusted Rates used 2000 US Census Population"
  ) +
  theme_minimal()
##Chronic
ggplot(cleaned_data_age_rates_fin %>% 
         filter(!is.na(Age_specific_rate_chronic_s), !is.na(`Age Cat.`)), 
       aes(x = Year, y = Age_specific_rate_chronic_s, color = `Age Cat.`)) +
  geom_line(size = 0.75) +
  geom_point(size = 1) +
  facet_wrap(~ State) +  # Separate plots for each state
  labs(
    title = "Chronic Hospitalization Rates from 2016-2020",
    subtitle = "Total Adult Population (18+)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Age Category",
    caption = "Data Source: Age-Adjusted Rates used 2000 US Census Population"
  ) +
  theme_minimal()

ggplot(cleaned_data_age_rates_finw %>% 
         filter(!is.na(Age_specific_rate_chronic_s), !is.na(`Age Cat.`)), 
       aes(x = Year, y = Age_specific_rate_chronic_s, color = `Age Cat.`)) +
  geom_line(size = 0.75) +
  geom_point(size = 1) +
  facet_wrap(~ State) +  # Separate plots for each state
  labs(
    title = "Chronic Hospitalization Rates from 2016-2020",
    subtitle = "Total Working Population (18-66)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Age Category",
    caption = "Data Source: Age-Adjusted Rates used 2000 US Census Population"
  ) +
  theme_minimal()

##Diabetes
ggplot(cleaned_data_age_rates_fin %>% 
         filter(!is.na(Age_specific_rate_diabetes_s), !is.na(`Age Cat.`)), 
       aes(x = Year, y = Age_specific_rate_diabetes_s, color = `Age Cat.`)) +
  geom_line(size = 0.75) +
  geom_point(size = 1) +
  facet_wrap(~ State) +  # Separate plots for each state
  labs(
    title = "Diabetes Hospitalization Rates from 2016-2020",
    subtitle = "Total Adult Population (18+)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Age Category",
    caption = "Data Source: Age-Adjusted Rates used 2000 US Census Population"
  ) +
  theme_minimal()

ggplot(cleaned_data_age_rates_finw %>% 
         filter(!is.na(Age_specific_rate_diabetes_s), !is.na(`Age Cat.`)), 
       aes(x = Year, y = Age_specific_rate_diabetes_s, color = `Age Cat.`)) +
  geom_line(size = 0.75) +
  geom_point(size = 1) +
  facet_wrap(~ State) +  # Separate plots for each state
  labs(
    title = "Diabetes Hospitalization Rates from 2016-2020",
    subtitle = "Total Working Population (18-66)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Age Category",
    caption = "Data Source: Age-Adjusted Rates used 2000 US Census Population"
  ) +
  theme_minimal()

##ALL 4 rates 
ggplot(cleaned_data_age_rates_fin, aes(x=Year))+
  geom_line(aes(y=Age_Adjusted_ACSC_Rate_y,color="ACSC"),size=1.5)+
  geom_point(aes(y=Age_Adjusted_ACSC_Rate_y, color="ACSC"),size=2)+
  geom_line(aes(y=Age_Adjusted_Acute_Rate_y,color="Acute"),size=1.5)+
  geom_point(aes(y=Age_Adjusted_Acute_Rate_y, color="Acute"),size=2)+
  geom_line(aes(y=Age_Adjusted_Chronic_Rate_y,color="Chronic"),size=1.5)+
  geom_point(aes(y=Age_Adjusted_Chronic_Rate_y, color="Chronic"),size=2)+
  geom_line(aes(y=Age_Adjusted_Diabetes_Rate_y,color="Diabetes"),size=1.5)+
  geom_point(aes(y=Age_Adjusted_Diabetes_Rate_y, color="Diabetes"),size=2)+
  labs(
    title = "Age-Adjusted Hospitalization Rates from 2016-2020",
    subtitle="Total Adult Population (18+)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Rate Type",
  ) +
  theme_minimal()

#WORKING
ggplot(cleaned_data_age_rates_finw, aes(x=Year))+
  geom_line(aes(y=Age_Adjusted_ACSC_Rate_y,color="ACSC"),size=1.5)+
  geom_point(aes(y=Age_Adjusted_ACSC_Rate_y, color="ACSC"),size=2)+
  geom_line(aes(y=Age_Adjusted_Acute_Rate_y,color="Acute"),size=1.5)+
  geom_point(aes(y=Age_Adjusted_Acute_Rate_y, color="Acute"),size=2)+
  geom_line(aes(y=Age_Adjusted_Chronic_Rate_y,color="Chronic"),size=1.5)+
  geom_point(aes(y=Age_Adjusted_Chronic_Rate_y, color="Chronic"),size=2)+
  geom_line(aes(y=Age_Adjusted_Diabetes_Rate_y,color="Diabetes"),size=1.5)+
  geom_point(aes(y=Age_Adjusted_Diabetes_Rate_y, color="Diabetes"),size=2)+
  labs(
    title = "Age-Adjusted Hospitalization Rates from 2016-2020",
    subtitle="Total Working Population (18-66)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Rate Type",
  ) +
  theme_minimal()


###MERGE FINAL DATASETS

cleaned_data_age_merged<-cleaned_merged_final%>%
  left_join(cleaned_data_age_rates_fin, by=c("GEOID","County","State","Year","Age Cat."))
save(cleaned_data_age_merged,file="cleaned_data_age_merged.RData")
load("cleaned_data_age_merged.RData")

#WORKING POPULATION DATASET
cleaned_data_age_mergedw<-cleaned_merged_dataw7%>%
  left_join(cleaned_data_age_rates_finw, by=c("GEOID","County","State","Year","Age Cat."))
save(cleaned_data_age_mergedw, file="cleaned_data_age_mergedw.RData")
