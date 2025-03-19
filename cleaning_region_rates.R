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

###Create Region Data from Census 
variables <- c(
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
  age_85_plus = "B01001_023"            # 85+ years
)
# Define years of interest
years<-2016:2020

region_census_data <- map_dfr(years, ~ {
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
view(region_census_data)

region_census_data_wide <- region_census_data %>%
  select(GEOID, NAME, Year, variable, estimate) %>%  # Select necessary columns
  pivot_wider(
    names_from = variable,    # Pivot to wide format by variable
    values_from = estimate    # Use the 'estimate' column as the values
  )
region_16_20<- region_census_data_wide%>%
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

northeast<-region_16_20%>%
  filter(NAME %in% c("Massachusetts","New York","New Jersey"))
southeast<-region_16_20%>%
  filter(NAME%in% c("North Carolina","Kentucky","Georgia","Florida"))
midwest<-region_16_20%>%
  filter(NAME %in% c("Minnesota","Wisconsin","Nebraska"))
southwest<-region_16_20%>%
  filter(NAME=="Arizona")
west<-region_16_20%>%
  filter(NAME%in%c("Oregon","Washington"))

regions<-region_16_20%>%
  filter(NAME%in%c("Massachusetts","New York","New Jersey","North Carolina",
                   "Kentucky","Georgia","Florida","Arizona",
                   "Minnesota","Wisconsin","Nebraska","Oregon","Washington"
                   ))%>%
  mutate(Region = case_when(
    NAME %in% c("North Carolina", "Kentucky", "Georgia", "Florida") ~ "Southeast",
    NAME %in% c("Massachusetts", "New York", "New Jersey") ~ "Northeast",
    NAME %in% c("Arizona") ~ "Southwest",
    NAME %in% c("Washington", "Oregon") ~ "West",
    NAME %in% c("Wisconsin", "Minnesota", "Iowa", "Missouri","Nebraska") ~ "Midwest"))

#FOR THE YEAR DATA
regions_census<-regions%>%
  group_by(Year, Region)%>%
  mutate(
    age_18_29_year_r=sum(age_18_29),
    age_30_44_year_r=sum(age_30_44),
    age_45_66_year_r=sum(age_45_66),
    age_67_74_year_r=sum(age_67_74),
    age_75_plus_year_r=sum(age_75_plus)
  )%>%
  select(
    NAME,
    Year,
    Region,
    age_18_29_year_r,
    age_30_44_year_r,
    age_45_66_year_r,
    age_67_74_year_r,
    age_75_plus_year_r
  )

#CLEAN DATA AND ADD VARIABLES FOR EASIER CODING
cleaned_data<-cleaned_data_age_race_sex_ins_merged%>%
  select(State, Year, `Age Cat.`,`ACSC Hospitalization`, `Acute Hospitalization`, 
          `Chronic Hospitalization`, `Diabetes Hospitalization`, standard_age_population)%>%
  mutate(Region = case_when(
    State %in% c("North Carolina", "Kentucky", "Georgia", "Florida") ~ "Southeast",
    State %in% c("Massachusetts", "New York", "New Jersey") ~ "Northeast",
    State %in% c("Arizona") ~ "Southwest",
    State %in% c("Washington", "Oregon") ~ "West",
    State %in% c("Wisconsin", "Minnesota", "Iowa", "Missouri","Nebraska") ~ "Midwest"))
cleaned_data_merged<-cleaned_data%>%
  left_join(regions_census, by=c("State"="NAME","Year","Region"))
cleaned_data_merged1<-cleaned_data_merged%>%
   mutate(
    age_population = case_when(
      `Age Cat.` == "18-29" ~ age_18_29_year_r,  
      `Age Cat.` == "30-44" ~ age_30_44_year_r,  
      `Age Cat.` == "45-66" ~ age_45_66_year_r,  
      `Age Cat.` == "67-74" ~ age_67_74_year_r,  
      `Age Cat.` == "75+" ~ age_75_plus_year_r
    ))%>%
  select(
    -age_18_29_year_r,
    -age_30_44_year_r,
    -age_45_66_year_r,
    -age_67_74_year_r,
    -age_75_plus_year_r
  )
#RATE CALCULATION
# Step 1: Calculate hospitalizations and age populations at the regional (PER YEAR) level
cases_by_age_region <- cleaned_data_merged1 %>%
  group_by(Year, Region, `Age Cat.`, age_population, standard_age_population) %>% 
  summarise(
    ACSC_Hosp = sum(`ACSC Hospitalization`, na.rm = TRUE),
    Acute_Hosp = sum(`Acute Hospitalization`, na.rm = TRUE),
    Chronic_Hosp = sum(`Chronic Hospitalization`, na.rm = TRUE),
    Diabetes_Hosp = sum(`Diabetes Hospitalization`, na.rm = TRUE),
    total_standard_age_population = 435079676+27655755+34467220+22688497+25306917, na.rm = TRUE) %>%
  ungroup()

# Step 2: Calculate age-specific rates
age_specific_rates_region <- cases_by_age_region %>%
  mutate( 
    Age_specific_rate_acsc_region = ACSC_Hosp / age_population * 1000,
    Age_specific_rate_acute_region = Acute_Hosp / age_population * 1000,
    Age_specific_rate_chronic_region = Chronic_Hosp / age_population * 1000,
    Age_specific_rate_diabetes_region= Diabetes_Hosp / age_population * 1000
  ) %>%
  select(Year, Region,`Age Cat.`, Age_specific_rate_acsc_region, Age_specific_rate_acute_region, 
         Age_specific_rate_chronic_region, Age_specific_rate_diabetes_region, standard_age_population
         , total_standard_age_population, age_population)

# Step 3: Calculate expected cases using summed standard population
expected_cases_region <- age_specific_rates_region %>%
  mutate(
    expected_cases_acsc = Age_specific_rate_acsc_region/ 1000 * standard_age_population,
    expected_cases_acute = Age_specific_rate_acute_region / 1000 * standard_age_population,
    expected_cases_chronic = Age_specific_rate_chronic_region / 1000 * standard_age_population,
    expected_cases_diabetes = Age_specific_rate_diabetes_region / 1000 * standard_age_population
  ) %>%
  select(Year, Region, `Age Cat.`, expected_cases_acsc, 
         expected_cases_acute, expected_cases_chronic, expected_cases_diabetes, 
         standard_age_population, total_standard_age_population)

# Step 4: Total expected cases by Year
total_expected_cases_region <- expected_cases_region %>%
  group_by(Year, Region) %>%
  summarise(
    total_expected_cases_acsc = sum(expected_cases_acsc, na.rm = TRUE),
    total_expected_cases_acute = sum(expected_cases_acute, na.rm = TRUE),
    total_expected_cases_chronic = sum(expected_cases_chronic, na.rm = TRUE),
    total_expected_cases_diabetes = sum(expected_cases_diabetes, na.rm = TRUE)) %>% ungroup()

# Step 5: Calculate age-adjusted rates based on summed expected cases
age_rates_adj_region <- total_expected_cases_region %>%
  left_join(expected_cases_region, by = c("Year","Region")) %>%
  mutate(
    Age_Adjusted_ACSC_Rate_region = (total_expected_cases_acsc / total_standard_age_population) * 1000,
    Age_Adjusted_Acute_Rate_region = (total_expected_cases_acute / total_standard_age_population) * 1000,
    Age_Adjusted_Chronic_Rate_region = (total_expected_cases_chronic / total_standard_age_population) * 1000,
    Age_Adjusted_Diabetes_Rate_region = (total_expected_cases_diabetes / total_standard_age_population) * 1000
  ) %>%
  select(Year, Region, Age_Adjusted_ACSC_Rate_region, Age_Adjusted_Acute_Rate_region, 
         Age_Adjusted_Chronic_Rate_region, Age_Adjusted_Diabetes_Rate_region)

# Step 6: Calculate crude rates at the county level (using total_age_population)
age_rates_crude_region<- cases_by_age_region %>%
  group_by(Year, Region) %>%
  summarise(
    Crude_ACSC_Rate_region= sum(ACSC_Hosp, na.rm = TRUE) / sum(age_population, na.rm = TRUE) * 1000,
    Crude_Acute_Rate_region = sum(Acute_Hosp, na.rm = TRUE) / sum(age_population, na.rm = TRUE) * 1000,
    Crude_Chronic_Rate_region = sum(Chronic_Hosp, na.rm = TRUE) / sum(age_population, na.rm = TRUE) * 1000,
    Crude_Diabetes_Rate_region= sum(Diabetes_Hosp, na.rm = TRUE) / sum(age_population, na.rm = TRUE) * 1000
  ) %>%
  ungroup()

# Step 7: Combine age-specific rates, crude rates, and adjusted rates
age_rates_crude_region_distinct<-age_rates_crude_region%>%
  distinct(Year,Region, .keep_all=TRUE)
age_rates_adj_region_distinct<-age_rates_adj_region%>%
  distinct(Year,Region, .keep_all=TRUE)
final_rates_region_age <- age_rates_crude_region_distinct %>%
  left_join(age_rates_adj_region_distinct, by = c("Year","Region"))
age_specific_rates_region_distinct<-age_specific_rates_region%>%
  distinct(Year,Region,`Age Cat.`, .keep_all=TRUE)
final_rates_region_age1<-final_rates_region_age%>%
  left_join(age_specific_rates_region, by = c("Year","Region"))
final_rates_region_age1=final_rates_region_age1%>%
  select(
    -age_population,
    -total_standard_age_population,
    -standard_age_population
  )

#MERGING
cleaned_data_age_race_sex_ins_reg_<-cleaned_data_merged1%>%
  left_join(final_rates_region_age1, by=c("Year","Age Cat.","Region"))%>%
  select(
    -age_population,
    -standard_age_population)

colnames(cleaned_data_age_race_sex_ins_merged)
colnames(cleaned_data_age_race_sex_ins_reg_)

cleaned_data_age_race_sex_ins_reg_ <- cleaned_data_age_race_sex_ins_reg_ %>%
  distinct(State, Year, Region, `Age Cat.`, `ACSC Hospitalization`, `Acute Hospitalization`,
           `Chronic Hospitalization`, `Diabetes Hospitalization`, .keep_all = TRUE)

final_merged_data <- cleaned_data_age_race_sex_ins_merged %>%
  left_join(cleaned_data_age_race_sex_ins_reg_, by = c("State", "Year", "Age Cat.",
                                                       "ACSC Hospitalization", "Acute Hospitalization",
                                                       "Chronic Hospitalization", "Diabetes Hospitalization"))

cleaned_data_age_race_sex_ins_reg_merged<-cleaned_data_age_race_sex_ins_merged%>%
  left_join(cleaned_data_age_race_sex_ins_reg_, by=c("State","Year","Age Cat.",
                                                     "ACSC Hospitalization",
                                                     "Acute Hospitalization",
                                                     "Chronic Hospitalization",
                                                     "Diabetes Hospitalization"))

save(cleaned_data_age_race_sex_ins_reg_merged,file="cleaned_data_age_race_sex_ins_reg_merged.RData")

#WORKING POPULATION
region_16_20w<- region_census_data_wide%>%
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

regionsw<-region_16_20w%>%
  filter(NAME%in%c("Massachusetts","New York","New Jersey","North Carolina",
                   "Kentucky","Georgia","Florida","Arizona",
                   "Minnesota","Wisconsin","Nebraska","Oregon","Washington"
  ))%>%
  mutate(Region = case_when(
    NAME %in% c("North Carolina", "Kentucky", "Georgia", "Florida") ~ "Southeast",
    NAME %in% c("Massachusetts", "New York", "New Jersey") ~ "Northeast",
    NAME %in% c("Arizona") ~ "Southwest",
    NAME %in% c("Washington", "Oregon") ~ "West",
    NAME %in% c("Wisconsin", "Minnesota", "Iowa", "Missouri","Nebraska") ~ "Midwest"))

regions_censusw<-regionsw%>%
  group_by(Year, Region)%>%
  mutate(
    age_18_29_year_r=sum(age_18_29),
    age_30_44_year_r=sum(age_30_44),
    age_45_66_year_r=sum(age_45_66)
  )%>%
  select(
    NAME,
    Year,
    Region,
    age_18_29_year_r,
    age_30_44_year_r,
    age_45_66_year_r
  )

#CLEANING DATA 

cleaned_dataw<-cleaned_data_age_race_sex_ins_mergedw%>%
  select(State, Year, `Age Cat.`,`ACSC Hospitalization`, `Acute Hospitalization`, 
         `Chronic Hospitalization`, `Diabetes Hospitalization`, standard_age_population)%>%
  mutate(Region = case_when(
    State %in% c("North Carolina", "Kentucky", "Georgia", "Florida") ~ "Southeast",
    State %in% c("Massachusetts", "New York", "New Jersey") ~ "Northeast",
    State %in% c("Arizona") ~ "Southwest",
    State %in% c("Washington", "Oregon") ~ "West",
    State %in% c("Wisconsin", "Minnesota", "Iowa", "Missouri","Nebraska") ~ "Midwest"))
cleaned_data_mergedw<-cleaned_dataw%>%
  left_join(regions_censusw, by=c("State"="NAME","Year","Region"))
cleaned_data_merged1w<-cleaned_data_mergedw%>%
  mutate(
    age_population = case_when(
      `Age Cat.` == "18-29" ~ age_18_29_year_r,  
      `Age Cat.` == "30-44" ~ age_30_44_year_r,  
      `Age Cat.` == "45-66" ~ age_45_66_year_r
    ))%>%
  select(
    -age_18_29_year_r,
    -age_30_44_year_r,
    -age_45_66_year_r
  )
#RATE CALCULATION
# Step 1: Calculate hospitalizations and age populations at the regional level
cases_by_age_regionw <- cleaned_data_merged1w %>%
  group_by(Year, Region, `Age Cat.`, age_population, standard_age_population) %>% 
  summarise(
    ACSC_Hosp = sum(`ACSC Hospitalization`, na.rm = TRUE),
    Acute_Hosp = sum(`Acute Hospitalization`, na.rm = TRUE),
    Chronic_Hosp = sum(`Chronic Hospitalization`, na.rm = TRUE),
    Diabetes_Hosp = sum(`Diabetes Hospitalization`, na.rm = TRUE),
    total_standard_age_population = 435079676+27655755+34467220+22688497+25306917, na.rm = TRUE) %>%
  ungroup()

# Step 2: Calculate age-specific rates
age_specific_rates_regionw <- cases_by_age_regionw %>%
  mutate( 
    Age_specific_rate_acsc_region = ACSC_Hosp / age_population * 1000,
    Age_specific_rate_acute_region = Acute_Hosp / age_population * 1000,
    Age_specific_rate_chronic_region = Chronic_Hosp / age_population * 1000,
    Age_specific_rate_diabetes_region= Diabetes_Hosp / age_population * 1000
  ) %>%
  select(Year, Region,`Age Cat.`, Age_specific_rate_acsc_region, Age_specific_rate_acute_region, 
         Age_specific_rate_chronic_region, Age_specific_rate_diabetes_region, standard_age_population
         , total_standard_age_population, age_population)

# Step 3: Calculate expected cases using summed standard population
expected_cases_regionw <- age_specific_rates_regionw %>%
  mutate(
    expected_cases_acsc = Age_specific_rate_acsc_region/ 1000 * standard_age_population,
    expected_cases_acute = Age_specific_rate_acute_region / 1000 * standard_age_population,
    expected_cases_chronic = Age_specific_rate_chronic_region / 1000 * standard_age_population,
    expected_cases_diabetes = Age_specific_rate_diabetes_region / 1000 * standard_age_population
  ) %>%
  select(Year, Region, `Age Cat.`, expected_cases_acsc, 
         expected_cases_acute, expected_cases_chronic, expected_cases_diabetes, 
         standard_age_population, total_standard_age_population)

# Step 4: Total expected cases by Year
total_expected_cases_regionw <- expected_cases_regionw %>%
  group_by(Year, Region) %>%
  summarise(
    total_expected_cases_acsc = sum(expected_cases_acsc, na.rm = TRUE),
    total_expected_cases_acute = sum(expected_cases_acute, na.rm = TRUE),
    total_expected_cases_chronic = sum(expected_cases_chronic, na.rm = TRUE),
    total_expected_cases_diabetes = sum(expected_cases_diabetes, na.rm = TRUE)) %>% ungroup()

# Step 5: Calculate age-adjusted rates based on summed expected cases
age_rates_adj_regionw <- total_expected_cases_regionw %>%
  left_join(expected_cases_regionw, by = c("Year","Region")) %>%
  mutate(
    Age_Adjusted_ACSC_Rate_region = (total_expected_cases_acsc / total_standard_age_population) * 1000,
    Age_Adjusted_Acute_Rate_region = (total_expected_cases_acute / total_standard_age_population) * 1000,
    Age_Adjusted_Chronic_Rate_region = (total_expected_cases_chronic / total_standard_age_population) * 1000,
    Age_Adjusted_Diabetes_Rate_region = (total_expected_cases_diabetes / total_standard_age_population) * 1000
  ) %>%
  select(Year, Region, Age_Adjusted_ACSC_Rate_region, Age_Adjusted_Acute_Rate_region, 
         Age_Adjusted_Chronic_Rate_region, Age_Adjusted_Diabetes_Rate_region)

# Step 6: Calculate crude rates at the county level (using total_age_population)
age_rates_crude_regionw<- cases_by_age_regionw %>%
  group_by(Year, Region) %>%
  summarise(
    Crude_ACSC_Rate_region= sum(ACSC_Hosp, na.rm = TRUE) / sum(age_population, na.rm = TRUE) * 1000,
    Crude_Acute_Rate_region = sum(Acute_Hosp, na.rm = TRUE) / sum(age_population, na.rm = TRUE) * 1000,
    Crude_Chronic_Rate_region = sum(Chronic_Hosp, na.rm = TRUE) / sum(age_population, na.rm = TRUE) * 1000,
    Crude_Diabetes_Rate_region= sum(Diabetes_Hosp, na.rm = TRUE) / sum(age_population, na.rm = TRUE) * 1000
  ) %>%
  ungroup()

# Step 7: Combine age-specific rates, crude rates, and adjusted rates
age_rates_crude_region_distinctw<-age_rates_crude_regionw%>%
  distinct(Year,Region, .keep_all=TRUE)
age_rates_adj_region_distinctw<-age_rates_adj_regionw%>%
  distinct(Year,Region, .keep_all=TRUE)
final_rates_region_agew <- age_rates_crude_region_distinctw %>%
  left_join(age_rates_adj_region_distinctw, by = c("Year","Region"))
age_specific_rates_region_distinctw<-age_specific_rates_regionw%>%
  distinct(Year,Region,`Age Cat.`, .keep_all=TRUE)
final_rates_region_age1w<-final_rates_region_agew%>%
  left_join(age_specific_rates_regionw, by = c("Year","Region"))
final_rates_region_age1w=final_rates_region_age1w%>%
  select(
    -age_population,
    -total_standard_age_population,
    -standard_age_population
  )


cleaned_data_age_race_sex_ins_reg_w<-cleaned_data_merged1w%>%
  left_join(final_rates_region_age1w, by=c("Year","Age Cat.","Region"))%>%
  select(
    -age_population,
    -standard_age_population)
#MERGING 
colnames(cleaned_data_age_race_sex_mergedw)
colnames(cleaned_data_age_race_sex_ins_reg_w)

cleaned_data_age_race_sex_ins_reg_w <- cleaned_data_age_race_sex_ins_reg_w %>%
  distinct(State, Year, Region, `Age Cat.`, `ACSC Hospitalization`, `Acute Hospitalization`,
           `Chronic Hospitalization`, `Diabetes Hospitalization`, .keep_all = TRUE)

cleaned_data_age_race_sex_ins_reg_mergedw<-cleaned_data_age_race_sex_ins_mergedw%>%
  left_join(cleaned_data_age_race_sex_ins_reg_w, by=c("State","Year","Age Cat.",
                                                     "ACSC Hospitalization",
                                                     "Acute Hospitalization",
                                                     "Chronic Hospitalization",
                                                     "Diabetes Hospitalization"))

save(cleaned_data_age_race_sex_ins_reg_mergedw,file="cleaned_data_age_race_sex_ins_reg_mergedw.RData")


