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

load("cleaned_data_age_merged.RData")
ls()
###RACE RATES
#CLEAN DATASET
cleaned_merged_data_race<-cleaned_merged_final%>%
  select(
    - `Age Cat.`,
    - age_population_county,
    -age_population_state,
    - standard_age_population,
    -median_income_state,
    -Poverty_Rate_state,
    -sex_population_county,
    -sex_population_state,
    - `Insurance Type`,
    - median_income,
    - Poverty_Rate,
    - Sex,
  )

#CALCULATION OF COUNTS
# Step 1: Calculate hospitalizations and age populations at the county level
cases_by_race_county <- cleaned_merged_data_race %>%
  group_by(GEOID, County, State, Year, Race, race_population_county) %>% 
  summarise(
    ACSC_Hosp = sum(`ACSC Hospitalization`, na.rm = TRUE),
    Acute_Hosp = sum(`Acute Hospitalization`, na.rm = TRUE),
    Chronic_Hosp = sum(`Chronic Hospitalization`, na.rm = TRUE),
    Diabetes_Hosp = sum(`Diabetes Hospitalization`, na.rm = TRUE))%>%
  ungroup()

# Step 2: Calculate race-specific rates
race_specific_rates <- cases_by_race_county %>%
  mutate( 
    race_specific_rate_acsc_c = ACSC_Hosp / race_population_county * 1000,
    race_specific_rate_acute_c = Acute_Hosp / race_population_county * 1000,
    race_specific_rate_chronic_c = Chronic_Hosp / race_population_county * 1000,
    race_specific_rate_diabetes_c= Diabetes_Hosp / race_population_county * 1000
  ) %>%
  select(GEOID, County, State, Year, Race, race_specific_rate_acsc_c, race_specific_rate_acute_c, 
         race_specific_rate_chronic_c, race_specific_rate_diabetes_c, race_population_county)
# Step 6: Calculate crude rates at the county level (using total_age_population)
race_rates_crude <- cases_by_race_county %>%
  group_by(GEOID, County, State, Year) %>%
  summarise(
    Crude_ACSC_Rate_Race_c = sum(ACSC_Hosp, na.rm = TRUE) / sum(race_population_county, na.rm = TRUE) * 1000,
    Crude_Acute_Rate_Race_c = sum(Acute_Hosp, na.rm = TRUE) / sum(race_population_county, na.rm = TRUE) * 1000,
    Crude_Chronic_Rate_Race_c = sum(Chronic_Hosp, na.rm = TRUE) / sum(race_population_county, na.rm = TRUE) * 1000,
    Crude_Diabetes_Rate_Race_c = sum(Diabetes_Hosp, na.rm = TRUE) / sum(race_population_county, na.rm = TRUE) * 1000
  ) %>%
  ungroup()

# Step 7: Combine age-specific rates, crude rates, and adjusted rates
final_rates_county_race <- race_rates_crude %>%
  left_join(race_specific_rates, by = c("GEOID", "County", "State", "Year"))
final_rates_county_race=final_rates_county_race%>%
  select(
    -race_population_county
  )


##RACE-SPECIFIC FOR EACH STATE
# Step 1: Aggregate hospitalizations and age populations at state level
state_data_race <- cleaned_merged_data_race%>%
  group_by(State, Year, Race, race_population_state) %>% 
  summarise(
    ACSC_Hosp = sum(`ACSC Hospitalization`, na.rm = TRUE),
    Acute_Hosp = sum(`Acute Hospitalization`, na.rm = TRUE),
    Chronic_Hosp = sum(`Chronic Hospitalization`, na.rm = TRUE),
    Diabetes_Hosp = sum(`Diabetes Hospitalization`, na.rm = TRUE))

# Step 2: Calculate age-specific rates for each state
race_specific_rates_state <- state_data_race %>%
  mutate( 
    Race_specific_rate_acsc_s = ACSC_Hosp / race_population_state * 1000,
    Race_specific_rate_acute_s = Acute_Hosp / race_population_state * 1000,
    Race_specific_rate_chronic_s= Chronic_Hosp / race_population_state * 1000,
    Race_specific_rate_diabetes_s = Diabetes_Hosp / race_population_state * 1000
  ) %>%
  select(State, Year, Race, Race_specific_rate_acsc_s, Race_specific_rate_acute_s, 
         Race_specific_rate_chronic_s, Race_specific_rate_diabetes_s, 
         race_population_state)
# Step 6: Calculate crude rates at the state level
race_rates_crude_state <- state_data_race %>%
  group_by(State, Year) %>%
  summarise(
    Crude_ACSC_Rate_race_s = sum(ACSC_Hosp, na.rm = TRUE) / sum(race_population_state, na.rm = TRUE) * 1000,
    Crude_Acute_Rate_race_s = sum(Acute_Hosp, na.rm = TRUE) / sum(race_population_state, na.rm = TRUE) * 1000,
    Crude_Chronic_Rate_race_s = sum(Chronic_Hosp, na.rm = TRUE) / sum(race_population_state, na.rm = TRUE) * 1000,
    Crude_Diabetes_Rate_race_s = sum(Diabetes_Hosp, na.rm = TRUE) / sum(race_population_state, na.rm = TRUE) * 1000
  ) %>%
  ungroup()

# Step 7: Merge the final results into one data frame
final_rates_state_race <- race_rates_crude_state %>%
  left_join(race_specific_rates_state, by = c("State", "Year"))
final_rates_state_race <- final_rates_state_race %>%
  select(
    - race_population_state
  )

#YEAR LEVEL RATES
# Step 1: Aggregate hospitalizations and age populations at year level
year_data_race<- merged_data_yearfin%>%
  group_by(Year, Race, race_population_year) %>% 
  summarise(
    ACSC_Hosp = sum(`ACSC Hospitalization`, na.rm = TRUE),
    Acute_Hosp = sum(`Acute Hospitalization`, na.rm = TRUE),
    Chronic_Hosp = sum(`Chronic Hospitalization`, na.rm = TRUE),
    Diabetes_Hosp = sum(`Diabetes Hospitalization`, na.rm = TRUE))%>% ungroup
# Step 2: Calculate age-specific rates for each year
race_specific_rates_year<-year_data_race %>%
  mutate( 
    Race_specific_rate_acsc_y = ACSC_Hosp / race_population_year * 1000,
    Race_specific_rate_acute_y = Acute_Hosp / race_population_year * 1000,
    Race_specific_rate_chronic_y = Chronic_Hosp / race_population_year * 1000,
    Race_specific_rate_diabetes_y = Diabetes_Hosp / race_population_year * 1000
  ) %>%
  select(Year, Race, Race_specific_rate_acsc_y, Race_specific_rate_acute_y, 
         Race_specific_rate_chronic_y, Race_specific_rate_diabetes_y, 
         race_population_year)

# Step 6: Calculate crude rates at the year level
race_rates_crude_year <- year_data_race %>%
  group_by(Year) %>%
  summarise(
    Crude_ACSC_Rate__Race_y= sum(ACSC_Hosp, na.rm = TRUE) / sum(race_population_year, na.rm = TRUE) * 1000,
    Crude_Acute_Rate_Race_y = sum(Acute_Hosp, na.rm = TRUE) / sum(race_population_year, na.rm = TRUE) * 1000,
    Crude_Chronic_Rate_Race_y = sum(Chronic_Hosp, na.rm = TRUE) / sum(race_population_year, na.rm = TRUE) * 1000,
    Crude_Diabetes_Rate_Race_y = sum(Diabetes_Hosp, na.rm = TRUE) / sum(race_population_year,na.rm = TRUE) * 1000
  ) %>%
  ungroup()

# Step 7: Merge the final results into one data frame
final_rates_year_race <- race_rates_crude_year %>%
  left_join(race_specific_rates_year, by = c("Year"))
final_rates_year_race <- final_rates_year_race %>%
  select(
    - race_population_year
  )


#MERGE DATA
colnames(final_rates_county_race)
colnames(final_rates_state_race)
colnames(final_rates_year_race)

duplicates_county_race <- final_rates_county_race %>%
  group_by(State, Year, Race) %>%
  filter(n() > 1)


duplicates_state_race <- final_rates_state_race %>%
  group_by(State, Year, Race) %>%
  filter(n() > 1)

final_rates_county_race_distinct <- final_rates_county_race %>%
  distinct(County, State, Year, Race, .keep_all = TRUE)
final_rates_state_race_distinct <- final_rates_state_race %>%
  distinct(State, Year, Race, .keep_all = TRUE)
final_data_race_rates <- final_rates_county_race_distinct %>%
  left_join(final_rates_state_race_distinct, by = c("State", "Year", "Race"))

duplicates_year_race <- final_rates_year_race %>%
  group_by( Year, Race) %>%
  filter(n() > 1)

final_rates_year_race_distinct <- final_rates_year_race %>%
  distinct(Year, Race, .keep_all = TRUE)

cleaned_data_race_rates_fin<-final_data_race_rates%>%
  left_join(final_rates_year_race_distinct, by=c("Year","Race"))

save(cleaned_data_race_rates_fin, file="cleaned_data_race_final.RData")
load("cleaned_data_race_final.RData")
#WORKING POPULATION
cleaned_merged_data_racew<-cleaned_merged_final%>%
  filter(`Age Cat.`%in% c("18-29","30-44","45-66"))%>%
  select(
    - `Age Cat.`,
    - age_population_county,
    -age_population_state,
    - standard_age_population,
    -median_income_state,
    -Poverty_Rate_state,
    -sex_population_county,
    -sex_population_state,
    - `Insurance Type`,
    - median_income,
    - Poverty_Rate,
    - Sex,
  )
#COUNTY LEVEL DATA 
cleaned_merged_data_racew<-cleaned_merged_dataw7%>%
  select(
    - age_population_county,
    -age_population_state,
    - age_population_year,
    - standard_age_population,
    -sex_population_county,
    -sex_population_state,
    -sex_population_year,
    - `Insurance Type`,
    - Sex,
  )
#CALCULATION OF COUNTS
# Step 1: Calculate hospitalizations and age populations at the county level
cases_by_race_countyw <- cleaned_merged_data_racew %>%
  group_by(GEOID, County, State, Year, Race, race_population_county) %>% 
  summarise(
    ACSC_Hosp = sum(`ACSC Hospitalization`, na.rm = TRUE),
    Acute_Hosp = sum(`Acute Hospitalization`, na.rm = TRUE),
    Chronic_Hosp = sum(`Chronic Hospitalization`, na.rm = TRUE),
    Diabetes_Hosp = sum(`Diabetes Hospitalization`, na.rm = TRUE))%>%
  ungroup()

# Step 2: Calculate race-specific rates
race_specific_ratesw <- cases_by_race_countyw %>%
  mutate( 
    race_specific_rate_acsc_c = ACSC_Hosp / race_population_county * 1000,
    race_specific_rate_acute_c = Acute_Hosp / race_population_county * 1000,
    race_specific_rate_chronic_c = Chronic_Hosp / race_population_county * 1000,
    race_specific_rate_diabetes_c= Diabetes_Hosp / race_population_county * 1000
  ) %>%
  select(GEOID, County, State, Year, Race, race_specific_rate_acsc_c, race_specific_rate_acute_c, 
         race_specific_rate_chronic_c, race_specific_rate_diabetes_c, race_population_county)
# Step 6: Calculate crude rates at the county level (using total_age_population)
race_rates_crudew <- cases_by_race_countyw %>%
  group_by(GEOID, County, State, Year) %>%
  summarise(
    Crude_ACSC_Rate_Race_c = sum(ACSC_Hosp, na.rm = TRUE) / sum(race_population_county, na.rm = TRUE) * 1000,
    Crude_Acute_Rate_Race_c = sum(Acute_Hosp, na.rm = TRUE) / sum(race_population_county, na.rm = TRUE) * 1000,
    Crude_Chronic_Rate_Race_c = sum(Chronic_Hosp, na.rm = TRUE) / sum(race_population_county, na.rm = TRUE) * 1000,
    Crude_Diabetes_Rate_Race_c = sum(Diabetes_Hosp, na.rm = TRUE) / sum(race_population_county, na.rm = TRUE) * 1000
  ) %>%
  ungroup()

# Step 7: Combine age-specific rates, crude rates, and adjusted rates
final_rates_county_racew<- race_rates_crudew %>%
  left_join(race_specific_ratesw, by = c("GEOID", "County", "State", "Year"))
final_rates_county_racew=final_rates_county_racew%>%
  select(
    -race_population_county
  )


##RACE-SPECIFIC FOR EACH STATE
# Step 1: Aggregate hospitalizations and age populations at state level
state_data_racew <- cleaned_merged_data_racew%>%
  group_by(State, Year, Race, race_population_state) %>% 
  summarise(
    ACSC_Hosp = sum(`ACSC Hospitalization`, na.rm = TRUE),
    Acute_Hosp = sum(`Acute Hospitalization`, na.rm = TRUE),
    Chronic_Hosp = sum(`Chronic Hospitalization`, na.rm = TRUE),
    Diabetes_Hosp = sum(`Diabetes Hospitalization`, na.rm = TRUE))

# Step 2: Calculate age-specific rates for each state
race_specific_rates_statew <- state_data_racew %>%
  mutate( 
    Race_specific_rate_acsc_s = ACSC_Hosp / race_population_state * 1000,
    Race_specific_rate_acute_s = Acute_Hosp / race_population_state * 1000,
    Race_specific_rate_chronic_s= Chronic_Hosp / race_population_state * 1000,
    Race_specific_rate_diabetes_s = Diabetes_Hosp / race_population_state * 1000
  ) %>%
  select(State, Year, Race, Race_specific_rate_acsc_s, Race_specific_rate_acute_s, 
         Race_specific_rate_chronic_s, Race_specific_rate_diabetes_s, 
         race_population_state)
# Step 6: Calculate crude rates at the state level
race_rates_crude_statew <- state_data_racew %>%
  group_by(State, Year) %>%
  summarise(
    Crude_ACSC_Rate_race_s = sum(ACSC_Hosp, na.rm = TRUE) / sum(race_population_state, na.rm = TRUE) * 1000,
    Crude_Acute_Rate_race_s = sum(Acute_Hosp, na.rm = TRUE) / sum(race_population_state, na.rm = TRUE) * 1000,
    Crude_Chronic_Rate_race_s = sum(Chronic_Hosp, na.rm = TRUE) / sum(race_population_state, na.rm = TRUE) * 1000,
    Crude_Diabetes_Rate_race_s = sum(Diabetes_Hosp, na.rm = TRUE) / sum(race_population_state, na.rm = TRUE) * 1000
  ) %>%
  ungroup()

# Step 7: Merge the final results into one data frame
final_rates_state_racew <- race_rates_crude_statew %>%
  left_join(race_specific_rates_statew, by = c("State", "Year"))
final_rates_state_racew <- final_rates_state_racew %>%
  select(
    - race_population_state
  )

#YEAR LEVEL RATES
# Step 1: Aggregate hospitalizations and age populations at year level
year_data_racew<- cleaned_merged_data_racew%>%
  group_by(Year, Race, race_population_year) %>% 
  summarise(
    ACSC_Hosp = sum(`ACSC Hospitalization`, na.rm = TRUE),
    Acute_Hosp = sum(`Acute Hospitalization`, na.rm = TRUE),
    Chronic_Hosp = sum(`Chronic Hospitalization`, na.rm = TRUE),
    Diabetes_Hosp = sum(`Diabetes Hospitalization`, na.rm = TRUE))%>% ungroup
# Step 2: Calculate age-specific rates for each year
race_specific_rates_yearw<-year_data_racew %>%
  mutate( 
    Race_specific_rate_acsc_y = ACSC_Hosp / race_population_year * 1000,
    Race_specific_rate_acute_y = Acute_Hosp / race_population_year * 1000,
    Race_specific_rate_chronic_y = Chronic_Hosp / race_population_year * 1000,
    Race_specific_rate_diabetes_y = Diabetes_Hosp / race_population_year * 1000
  ) %>%
  select(Year, Race, Race_specific_rate_acsc_y, Race_specific_rate_acute_y, 
         Race_specific_rate_chronic_y, Race_specific_rate_diabetes_y, 
         race_population_year)

# Step 6: Calculate crude rates at the year level
race_rates_crude_yearw <- year_data_racew %>%
  group_by(Year) %>%
  summarise(
    Crude_ACSC_Rate__Race_y= sum(ACSC_Hosp, na.rm = TRUE) / sum(race_population_year, na.rm = TRUE) * 1000,
    Crude_Acute_Rate_Race_y = sum(Acute_Hosp, na.rm = TRUE) / sum(race_population_year, na.rm = TRUE) * 1000,
    Crude_Chronic_Rate_Race_y = sum(Chronic_Hosp, na.rm = TRUE) / sum(race_population_year, na.rm = TRUE) * 1000,
    Crude_Diabetes_Rate_Race_y = sum(Diabetes_Hosp, na.rm = TRUE) / sum(race_population_year,na.rm = TRUE) * 1000
  ) %>%
  ungroup()

# Step 7: Merge the final results into one data frame
final_rates_year_racew<- race_rates_crude_yearw %>%
  left_join(race_specific_rates_yearw, by = c("Year"))
final_rates_year_racew <- final_rates_year_racew %>%
  select(
    - race_population_year
  )


#MERGE DATA
colnames(final_rates_county_racew)
colnames(final_rates_state_racew)
colnames(final_rates_year_racew)

duplicates_county_racew <- final_rates_county_racew %>%
  group_by(State, Year, Race) %>%
  filter(n() > 1)


duplicates_state_racew <- final_rates_state_racew %>%
  group_by(State, Year, Race) %>%
  filter(n() > 1)

final_rates_county_race_distinctw<- final_rates_county_racew %>%
  distinct(County, State, Year, Race, .keep_all = TRUE)
final_rates_state_race_distinctw <- final_rates_state_racew%>%
  distinct(State, Year, Race, .keep_all = TRUE)
final_data_race_ratesw <- final_rates_county_race_distinctw %>%
  left_join(final_rates_state_race_distinctw, by = c("State", "Year", "Race"))

duplicates_year_racew <- final_rates_year_racew %>%
  group_by( Year, Race) %>%
  filter(n() > 1)

final_rates_year_race_distinctw <- final_rates_year_racew %>%
  distinct(Year, Race, .keep_all = TRUE)

cleaned_data_race_rates_finw<-final_data_race_ratesw%>%
  left_join(final_rates_year_race_distinctw, by=c("Year","Race"))

save(cleaned_data_race_rates_finw, file="cleaned_data_race_finalw.RData")
load("cleaned_data_race_finalw.RData")

#GRAPHS- years (TABLE 1)
#ACSC
ggplot(cleaned_data_race_rates_fin %>% 
         filter(!is.na(Race_specific_rate_acsc_s), !is.na(Race)), 
       aes(x = Year, y = Race_specific_rate_acsc_s, color = Race)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ State) +  # Separate plots for each state
  labs(
    title = "Race-Specific ACSC Hospitalization Rates from 2016-2020",
    subtitle = "Total Adult Population (18+)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Race Category",
  ) +
  theme_minimal()
#WORKING
ggplot(cleaned_data_race_rates_finw %>% 
         filter(!is.na(Race_specific_rate_acsc_s), !is.na(Race)), 
       aes(x = Year, y = Race_specific_rate_acsc_s, color = Race)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ State) +  # Separate plots for each state
  labs(
    title = "Race-Specific ACSC Hospitalization Rates from 2016-2020",
    subtitle = "Total Working Population (18-66)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Race Category",
  ) +
  theme_minimal()


#ACUTE
ggplot(cleaned_data_race_rates_fin %>% 
         filter(!is.na(Race_specific_rate_acute_s), !is.na(Race)), 
       aes(x = Year, y = Race_specific_rate_acute_s, color = Race)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ State) +  # Separate plots for each state
  labs(
    title = "Race-Specific Acute Hospitalization Rates from 2016-2020",
    subtitle = "Total Adult Population (18+)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Race Category",
  ) +
  theme_minimal()
#WORKING
ggplot(cleaned_data_race_rates_finw %>% 
         filter(!is.na(Race_specific_rate_acute_s), !is.na(Race)), 
       aes(x = Year, y = Race_specific_rate_acute_s, color = Race)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ State) +  # Separate plots for each state
  labs(
    title = "Race-Specific Acute Hospitalization Rates from 2016-2020",
    subtitle = "Total Working Population (18-66)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Race Category",
  ) +
  theme_minimal()


#CHRONIC
ggplot(cleaned_data_race_rates_fin %>% 
         filter(!is.na(Race_specific_rate_chronic_s), !is.na(Race)), 
       aes(x = Year, y = Race_specific_rate_chronic_s, color = Race)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ State) +  # Separate plots for each state
  labs(
    title = "Race-Specific Chronic Hospitalization Rates from 2016-2020",
    subtitle = "Total Adult Population (18+)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Race Category",
  ) +
  theme_minimal()
#WORKING
ggplot(cleaned_data_race_rates_finw %>% 
         filter(!is.na(Race_specific_rate_chronic_s), !is.na(Race)), 
       aes(x = Year, y = Race_specific_rate_chronic_s, color = Race)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ State) +  # Separate plots for each state
  labs(
    title = "Race-Specific Chronic Hospitalization Rates from 2016-2020",
    subtitle = "Total Working Population (18-66)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Race Category",
  ) +
  theme_minimal()

#DIABETES
ggplot(cleaned_data_race_rates_fin %>% 
         filter(!is.na(Race_specific_rate_diabetes_s), !is.na(Race)), 
       aes(x = Year, y = Race_specific_rate_diabetes_s, color = Race)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ State) +  # Separate plots for each state
  labs(
    title = "Race-Specific Diabetes Hospitalization Rates from 2016-2020",
    subtitle = "Total Adult Population (18+)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Race Category",
  ) +
  theme_minimal()
#WORKING
ggplot(cleaned_data_race_rates_finw %>% 
         filter(!is.na(Race_specific_rate_diabetes_s), !is.na(Race)), 
       aes(x = Year, y = Race_specific_rate_diabetes_s, color = Race)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ State) +  # Separate plots for each state
  labs(
    title = "Race-Specific Diabetes Hospitalization Rates from 2016-2020",
    subtitle = "Total Working Population (18-66)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Race Category",
  ) +
  theme_minimal()

#Year total (TABLE 1)
cleaned_data_race_rates_fin%>% 
  filter(!is.na(Race_specific_rate_acsc_y), !is.na(Race))%>%
  filter(!is.na(Race_specific_rate_acute_y), !is.na(Race))%>%
  filter(!is.na(Race_specific_rate_chronic_y), !is.na(Race))%>%
  filter(!is.na(Race_specific_rate_diabetes_y), !is.na(Race))%>%
  ggplot(aes(x = Year)) +
  geom_line(aes(y=Race_specific_rate_acsc_y,color="ACSC"),linewidth=1) +
  geom_line(aes(y=Race_specific_rate_acute_y,color="Acute"),linewidth=1) +
  geom_line(aes(y=Race_specific_rate_chronic_y,color="Chronic"),linewidth=1) +
  geom_line(aes(y=Race_specific_rate_diabetes_y,color="Diabetes"),linewidth=1) +
  geom_point(aes(y=Race_specific_rate_acsc_y,color="ACSC"),size=1.5) +
  geom_point(aes(y=Race_specific_rate_acute_y,color="Acute"),size=1.5) +
  geom_point(aes(y=Race_specific_rate_chronic_y,color="Chronic"),size=1.5) +
  geom_point(aes(y=Race_specific_rate_diabetes_y,color="Diabetes"),size=1.5) +
  facet_wrap(~Race)+
  labs(
    title = "Hospitalization Specific Rates Over Time",
    subtitle = "Adult Population (18+), Stratified by Race",
    x = "Year",
    y = "Rate (per 1,000)",
    color = "Rate Type"
  ) + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  
  )
#WORKING
cleaned_data_race_rates_finw%>% 
  filter(!is.na(Race_specific_rate_acsc_y), !is.na(Race))%>%
  filter(!is.na(Race_specific_rate_acute_y), !is.na(Race))%>%
  filter(!is.na(Race_specific_rate_chronic_y), !is.na(Race))%>%
  filter(!is.na(Race_specific_rate_diabetes_y), !is.na(Race))%>%
  ggplot(aes(x = Year)) +
  geom_line(aes(y=Race_specific_rate_acsc_y,color="ACSC"),linewidth=1) +
  geom_line(aes(y=Race_specific_rate_acute_y,color="Acute"),linewidth=1) +
  geom_line(aes(y=Race_specific_rate_chronic_y,color="Chronic"),linewidth=1) +
  geom_line(aes(y=Race_specific_rate_diabetes_y,color="Diabetes"),linewidth=1) +
  geom_point(aes(y=Race_specific_rate_acsc_y,color="ACSC"),size=1.5) +
  geom_point(aes(y=Race_specific_rate_acute_y,color="Acute"),size=1.5) +
  geom_point(aes(y=Race_specific_rate_chronic_y,color="Chronic"),size=1.5) +
  geom_point(aes(y=Race_specific_rate_diabetes_y,color="Diabetes"),size=1.5) +
  facet_wrap(~Race)+
  labs(
    title = "Hospitalization Specific Rates Over Time",
    subtitle = "Working Population (18-66), Stratified by Race",
    x = "Year",
    y = "Rate (per 1,000)",
    color = "Rate Type"
  ) + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  
  )

##MERGE DATASETS 
cleaned_data_age_race_merged<-cleaned_data_age_merged%>%
  left_join(cleaned_data_race_rates_fin, by=c("GEOID","County","State","Year","Race"))
save(cleaned_data_age_race_merged,file="cleaned_data_age_race_merged.RData")
load("cleaned_data_age_race_merged.RData")
#WORKING POPULATION DATASET
cleaned_data_age_race_mergedw<-cleaned_data_age_mergedw%>%
  left_join(cleaned_data_race_rates_finw, by=c("GEOID","County","State","Year","Race"))
save(cleaned_data_age_race_mergedw,file="cleaned_data_age_race_mergedw.RData")
load("cleaned_data_age_race_mergedw.RData")