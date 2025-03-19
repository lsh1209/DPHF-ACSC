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

load("cleaned_data_age_race_merged.RData")


##SEX RATES
#CLEAN DATASET
cleaned_merged_data_sex<-cleaned_merged_final%>%
  select(
    - `Age Cat.`,
    - age_population_county,
    -age_population_state,
    - standard_age_population,
    -median_income_state,
    -Poverty_Rate_state,
    -race_population_county,
    -race_population_state,
    - `Insurance Type`,
    - median_income,
    - Poverty_Rate,
    - Race,
  )

#CALCULATION OF COUNTS
# Step 1: Calculate hospitalizations and sex populations at the county level
cases_by_sex_county <- cleaned_merged_data_sex %>%
  group_by(GEOID, County, State, Year, Sex, sex_population_county) %>% 
  summarise(
    ACSC_Hosp = sum(`ACSC Hospitalization`, na.rm = TRUE),
    Acute_Hosp = sum(`Acute Hospitalization`, na.rm = TRUE),
    Chronic_Hosp = sum(`Chronic Hospitalization`, na.rm = TRUE),
    Diabetes_Hosp = sum(`Diabetes Hospitalization`, na.rm = TRUE))%>%
  ungroup()

# Step 2: Calculate sex-specific rates
sex_specific_rates <- cases_by_sex_county %>%
  mutate( 
    sex_specific_rate_acsc_c = ACSC_Hosp / sex_population_county * 1000,
    sex_specific_rate_acute_c = Acute_Hosp / sex_population_county * 1000,
    sex_specific_rate_chronic_c = Chronic_Hosp / sex_population_county * 1000,
    sex_specific_rate_diabetes_c= Diabetes_Hosp / sex_population_county * 1000
  ) %>%
  select(GEOID, County, State, Year, Sex, sex_specific_rate_acsc_c, sex_specific_rate_acute_c, 
         sex_specific_rate_chronic_c, sex_specific_rate_diabetes_c, sex_population_county)
# Step 6: Calculate crude rates at the county level (using total_sex_population)
sex_rates_crude <- cases_by_sex_county %>%
  group_by(GEOID, County, State, Year) %>%
  summarise(
    Crude_ACSC_Rate_Sex_c = sum(ACSC_Hosp, na.rm = TRUE) / sum(sex_population_county, na.rm = TRUE) * 1000,
    Crude_Acute_Rate_Sex_c = sum(Acute_Hosp, na.rm = TRUE) / sum(sex_population_county, na.rm = TRUE) * 1000,
    Crude_Chronic_Rate_Sex_c = sum(Chronic_Hosp, na.rm = TRUE) / sum(sex_population_county, na.rm = TRUE) * 1000,
    Crude_Diabetes_Rate_Sex_c = sum(Diabetes_Hosp, na.rm = TRUE) / sum(sex_population_county, na.rm = TRUE) * 1000
  ) %>%
  ungroup()

# Step 7: Combine sex-specific rates, crude rates, and adjusted rates
final_rates_county_sex <- sex_rates_crude %>%
  left_join(sex_specific_rates, by = c("GEOID", "County", "State", "Year"))
final_rates_county_sex=final_rates_county_sex%>%
  select(
    -sex_population_county
  )

##SEX-SPECIFIC FOR EACH STATE
# Step 1: Aggregate hospitalizations and sex populations at state level
state_data_sex <- cleaned_merged_data_sex%>%
  group_by(State, Year, Sex,sex_population_state ) %>% 
  summarise(
    ACSC_Hosp = sum(`ACSC Hospitalization`, na.rm = TRUE),
    Acute_Hosp = sum(`Acute Hospitalization`, na.rm = TRUE),
    Chronic_Hosp = sum(`Chronic Hospitalization`, na.rm = TRUE),
    Diabetes_Hosp = sum(`Diabetes Hospitalization`, na.rm = TRUE))

# Step 2: Calculate sex-specific rates for each state
sex_specific_rates_state <- state_data_sex %>%
  mutate( 
    Sex_specific_rate_acsc_s = ACSC_Hosp / sex_population_state * 1000,
    Sex_specific_rate_acute_s = Acute_Hosp / sex_population_state * 1000,
    Sex_specific_rate_chronic_s= Chronic_Hosp / sex_population_state * 1000,
    Sex_specific_rate_diabetes_s = Diabetes_Hosp / sex_population_state * 1000
  ) %>%
  select(State, Year, Sex, Sex_specific_rate_acsc_s, Sex_specific_rate_acute_s, 
         Sex_specific_rate_chronic_s, Sex_specific_rate_diabetes_s, 
         sex_population_state)
# Step 6: Calculate crude rates at the state level
sex_rates_crude_state <- state_data_sex %>%
  group_by(State, Year) %>%
  summarise(
    Crude_ACSC_Rate_sex_s = sum(ACSC_Hosp, na.rm = TRUE) / sum(sex_population_state, na.rm = TRUE) * 1000,
    Crude_Acute_Rate_sex_s = sum(Acute_Hosp, na.rm = TRUE) / sum(sex_population_state, na.rm = TRUE) * 1000,
    Crude_Chronic_Rate_sex_s = sum(Chronic_Hosp, na.rm = TRUE) / sum(sex_population_state, na.rm = TRUE) * 1000,
    Crude_Diabetes_Rate_sex_s = sum(Diabetes_Hosp, na.rm = TRUE) / sum(sex_population_state, na.rm = TRUE) * 1000
  ) %>%
  ungroup()

# Step 7: Merge the final results into one data frame
final_rates_state_sex <- sex_rates_crude_state %>%
  left_join(sex_specific_rates_state, by = c("State", "Year"))
final_rates_state_sex <- final_rates_state_sex %>%
  select(
    - sex_population_state
  )

#YEAR LEVEL RATES
# Step 1: Aggregate hospitalizations and sex populations at year level
year_data_sex<- merged_data_yearfin%>%
  group_by(Year, Sex, sex_population_year) %>% 
  summarise(
    ACSC_Hosp = sum(`ACSC Hospitalization`, na.rm = TRUE),
    Acute_Hosp = sum(`Acute Hospitalization`, na.rm = TRUE),
    Chronic_Hosp = sum(`Chronic Hospitalization`, na.rm = TRUE),
    Diabetes_Hosp = sum(`Diabetes Hospitalization`, na.rm = TRUE))%>% ungroup
# Step 2: Calculate sex-specific rates for each year
sex_specific_rates_year<-year_data_sex %>%
  mutate( 
    Sex_specific_rate_acsc_y = ACSC_Hosp / sex_population_year * 1000,
    Sex_specific_rate_acute_y = Acute_Hosp / sex_population_year * 1000,
    Sex_specific_rate_chronic_y = Chronic_Hosp / sex_population_year * 1000,
    Sex_specific_rate_diabetes_y = Diabetes_Hosp / sex_population_year * 1000
  ) %>%
  select(Year, Sex, Sex_specific_rate_acsc_y, Sex_specific_rate_acute_y, 
         Sex_specific_rate_chronic_y, Sex_specific_rate_diabetes_y, 
         sex_population_year)

# Step 6: Calculate crude rates at the year level
sex_rates_crude_year <- year_data_sex %>%
  group_by(Year) %>%
  summarise(
    Crude_ACSC_Rate_Sex_y= sum(ACSC_Hosp, na.rm = TRUE) / sum(sex_population_year, na.rm = TRUE) * 1000,
    Crude_Acute_Rate_Sex_y = sum(Acute_Hosp, na.rm = TRUE) / sum(sex_population_year, na.rm = TRUE) * 1000,
    Crude_Chronic_Rate_Sex_y = sum(Chronic_Hosp, na.rm = TRUE) / sum(sex_population_year, na.rm = TRUE) * 1000,
    Crude_Diabetes_Rate_Sex_y = sum(Diabetes_Hosp, na.rm = TRUE) / sum(sex_population_year,na.rm = TRUE) * 1000
  ) %>%
  ungroup()

# Step 7: Merge the final results into one data frame
final_rates_year_sex <- sex_rates_crude_year %>%
  left_join(sex_specific_rates_year, by = c("Year"))
final_rates_year_sex <- final_rates_year_sex %>%
  select(
    - sex_population_year
  )

#MERGE DATA
colnames(final_rates_county_sex)
colnames(final_rates_state_sex)
colnames(final_rates_year_sex)

duplicates_county_sex <- final_rates_county_sex %>%
  group_by(State, Year, Sex) %>%
  filter(n() > 1)


duplicates_state_sex <- final_rates_state_sex %>%
  group_by(State, Year, Sex) %>%
  filter(n() > 1)

final_rates_county_sex_distinct <- final_rates_county_sex %>%
  distinct(County, State, Year, Sex, .keep_all = TRUE)
final_rates_state_sex_distinct <- final_rates_state_sex %>%
  distinct(State, Year, Sex, .keep_all = TRUE)
final_data_sex_rates <- final_rates_county_sex_distinct %>%
  left_join(final_rates_state_sex_distinct, by = c("State", "Year", "Sex"))

duplicates_year_sex <- final_rates_year_sex %>%
  group_by( Year, Sex) %>%
  filter(n() > 1)

final_rates_year_sex_distinct <- final_rates_year_sex %>%
  distinct(Year, Sex, .keep_all = TRUE)

cleaned_data_sex_rates_fin<-final_data_sex_rates%>%
  left_join(final_rates_year_sex_distinct, by=c("Year","Sex"))

save(cleaned_data_sex_rates_fin, file="cleaned_data_sex_final.RData")
load("cleaned_data_sex_final.RData")
#WORKING POPULATION
cleaned_merged_data_sexw<-cleaned_merged_dataw7%>%
  select(
    - age_population_county,
    -age_population_state,
    - standard_age_population,
    - age_population_year,
    -race_population_county,
    -race_population_state,
    -race_population_year,
    - `Insurance Type`,
    - Race,
  )

#CALCULATION OF COUNTS
# Step 1: Calculate hospitalizations and sex populations at the county level
cases_by_sex_countyw <- cleaned_merged_data_sexw %>%
  group_by(GEOID, County, State, Year, Sex, sex_population_county) %>% 
  summarise(
    ACSC_Hosp = sum(`ACSC Hospitalization`, na.rm = TRUE),
    Acute_Hosp = sum(`Acute Hospitalization`, na.rm = TRUE),
    Chronic_Hosp = sum(`Chronic Hospitalization`, na.rm = TRUE),
    Diabetes_Hosp = sum(`Diabetes Hospitalization`, na.rm = TRUE))%>%
  ungroup()

# Step 2: Calculate sex-specific rates
sex_specific_ratesw <- cases_by_sex_countyw %>%
  mutate( 
    sex_specific_rate_acsc_c = ACSC_Hosp / sex_population_county * 1000,
    sex_specific_rate_acute_c = Acute_Hosp / sex_population_county * 1000,
    sex_specific_rate_chronic_c = Chronic_Hosp / sex_population_county * 1000,
    sex_specific_rate_diabetes_c= Diabetes_Hosp / sex_population_county * 1000
  ) %>%
  select(GEOID, County, State, Year, Sex, sex_specific_rate_acsc_c, sex_specific_rate_acute_c, 
         sex_specific_rate_chronic_c, sex_specific_rate_diabetes_c, sex_population_county)
# Step 6: Calculate crude rates at the county level (using total_sex_population)
sex_rates_crudew <- cases_by_sex_countyw %>%
  group_by(GEOID, County, State, Year) %>%
  summarise(
    Crude_ACSC_Rate_Sex_c = sum(ACSC_Hosp, na.rm = TRUE) / sum(sex_population_county, na.rm = TRUE) * 1000,
    Crude_Acute_Rate_Sex_c = sum(Acute_Hosp, na.rm = TRUE) / sum(sex_population_county, na.rm = TRUE) * 1000,
    Crude_Chronic_Rate_Sex_c = sum(Chronic_Hosp, na.rm = TRUE) / sum(sex_population_county, na.rm = TRUE) * 1000,
    Crude_Diabetes_Rate_Sex_c = sum(Diabetes_Hosp, na.rm = TRUE) / sum(sex_population_county, na.rm = TRUE) * 1000
  ) %>%
  ungroup()

# Step 7: Combine sex-specific rates, crude rates, and adjusted rates
final_rates_county_sexw <- sex_rates_crudew %>%
  left_join(sex_specific_ratesw, by = c("GEOID", "County", "State", "Year"))
final_rates_county_sexw=final_rates_county_sexw%>%
  select(
    -sex_population_county
  )

##SEX-SPECIFIC FOR EACH STATE
# Step 1: Aggregate hospitalizations and sex populations at state level
state_data_sexw <- cleaned_merged_data_sexw%>%
  group_by(State, Year, Sex,sex_population_state ) %>% 
  summarise(
    ACSC_Hosp = sum(`ACSC Hospitalization`, na.rm = TRUE),
    Acute_Hosp = sum(`Acute Hospitalization`, na.rm = TRUE),
    Chronic_Hosp = sum(`Chronic Hospitalization`, na.rm = TRUE),
    Diabetes_Hosp = sum(`Diabetes Hospitalization`, na.rm = TRUE))

# Step 2: Calculate sex-specific rates for each state
sex_specific_rates_statew <- state_data_sexw %>%
  mutate( 
    Sex_specific_rate_acsc_s = ACSC_Hosp / sex_population_state * 1000,
    Sex_specific_rate_acute_s = Acute_Hosp / sex_population_state * 1000,
    Sex_specific_rate_chronic_s= Chronic_Hosp / sex_population_state * 1000,
    Sex_specific_rate_diabetes_s = Diabetes_Hosp / sex_population_state * 1000
  ) %>%
  select(State, Year, Sex, Sex_specific_rate_acsc_s, Sex_specific_rate_acute_s, 
         Sex_specific_rate_chronic_s, Sex_specific_rate_diabetes_s, 
         sex_population_state)
# Step 6: Calculate crude rates at the state level
sex_rates_crude_statew <- state_data_sexw %>%
  group_by(State, Year) %>%
  summarise(
    Crude_ACSC_Rate_sex_s = sum(ACSC_Hosp, na.rm = TRUE) / sum(sex_population_state, na.rm = TRUE) * 1000,
    Crude_Acute_Rate_sex_s = sum(Acute_Hosp, na.rm = TRUE) / sum(sex_population_state, na.rm = TRUE) * 1000,
    Crude_Chronic_Rate_sex_s = sum(Chronic_Hosp, na.rm = TRUE) / sum(sex_population_state, na.rm = TRUE) * 1000,
    Crude_Diabetes_Rate_sex_s = sum(Diabetes_Hosp, na.rm = TRUE) / sum(sex_population_state, na.rm = TRUE) * 1000
  ) %>%
  ungroup()

# Step 7: Merge the final results into one data frame
final_rates_state_sexw <- sex_rates_crude_statew %>%
  left_join(sex_specific_rates_statew, by = c("State", "Year"))
final_rates_state_sexw <- final_rates_state_sexw %>%
  select(
    - sex_population_state
  )

#YEAR LEVEL RATES
# Step 1: Aggregate hospitalizations and sex populations at year level
year_data_sexw<- cleaned_merged_data_sexw%>%
  group_by(Year, Sex, sex_population_year) %>% 
  summarise(
    ACSC_Hosp = sum(`ACSC Hospitalization`, na.rm = TRUE),
    Acute_Hosp = sum(`Acute Hospitalization`, na.rm = TRUE),
    Chronic_Hosp = sum(`Chronic Hospitalization`, na.rm = TRUE),
    Diabetes_Hosp = sum(`Diabetes Hospitalization`, na.rm = TRUE))%>% ungroup
# Step 2: Calculate sex-specific rates for each year
sex_specific_rates_yearw<-year_data_sexw %>%
  mutate( 
    Sex_specific_rate_acsc_y = ACSC_Hosp / sex_population_year * 1000,
    Sex_specific_rate_acute_y = Acute_Hosp / sex_population_year * 1000,
    Sex_specific_rate_chronic_y = Chronic_Hosp / sex_population_year * 1000,
    Sex_specific_rate_diabetes_y = Diabetes_Hosp / sex_population_year * 1000
  ) %>%
  select(Year, Sex, Sex_specific_rate_acsc_y, Sex_specific_rate_acute_y, 
         Sex_specific_rate_chronic_y, Sex_specific_rate_diabetes_y, 
         sex_population_year)

# Step 6: Calculate crude rates at the year level
sex_rates_crude_yearw <- year_data_sexw %>%
  group_by(Year) %>%
  summarise(
    Crude_ACSC_Rate_Sex_y= sum(ACSC_Hosp, na.rm = TRUE) / sum(sex_population_year, na.rm = TRUE) * 1000,
    Crude_Acute_Rate_Sex_y = sum(Acute_Hosp, na.rm = TRUE) / sum(sex_population_year, na.rm = TRUE) * 1000,
    Crude_Chronic_Rate_Sex_y = sum(Chronic_Hosp, na.rm = TRUE) / sum(sex_population_year, na.rm = TRUE) * 1000,
    Crude_Diabetes_Rate_Sex_y = sum(Diabetes_Hosp, na.rm = TRUE) / sum(sex_population_year,na.rm = TRUE) * 1000
  ) %>%
  ungroup()

# Step 7: Merge the final results into one data frame
final_rates_year_sexw <- sex_rates_crude_yearw %>%
  left_join(sex_specific_rates_yearw, by = c("Year"))
final_rates_year_sexw <- final_rates_year_sexw %>%
  select(
    - sex_population_year
  )

#MERGE DATA
colnames(final_rates_county_sexw)
colnames(final_rates_state_sexw)
colnames(final_rates_year_sexw)

duplicates_county_sexw <- final_rates_county_sexw %>%
  group_by(State, Year, Sex) %>%
  filter(n() > 1)


duplicates_state_sexw <- final_rates_state_sexw %>%
  group_by(State, Year, Sex) %>%
  filter(n() > 1)

final_rates_county_sex_distinctw <- final_rates_county_sexw %>%
  distinct(County, State, Year, Sex, .keep_all = TRUE)
final_rates_state_sex_distinctw <- final_rates_state_sexw %>%
  distinct(State, Year, Sex, .keep_all = TRUE)
final_data_sex_ratesw <- final_rates_county_sex_distinctw %>%
  left_join(final_rates_state_sex_distinctw, by = c("State", "Year", "Sex"))

duplicates_year_sexw <- final_rates_year_sexw%>%
  group_by( Year, Sex) %>%
  filter(n() > 1)

final_rates_year_sex_distinctw <- final_rates_year_sexw%>%
  distinct(Year, Sex, .keep_all = TRUE)

cleaned_data_sex_rates_finw<-final_data_sex_ratesw%>%
  left_join(final_rates_year_sex_distinctw, by=c("Year","Sex"))

save(cleaned_data_sex_rates_finw, file="cleaned_data_sex_finalw.RData")
load("cleaned_data_sex_finalw.RData")

#GRAPHS- years (TABLE 1)
#ACSC
ggplot(cleaned_data_sex_rates_fin %>% 
         filter(!is.na(Sex_specific_rate_acsc_s), !is.na(Sex)), 
       aes(x = Year, y = Sex_specific_rate_acsc_s, color = (Sex))) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ State) +  # Separate plots for each state
  labs(
    title = "Sex-Specific ACSC Hospitalization Rates from 2016-2020",
    subtitle = "Total Adult Population (18+)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Sex Category",
  ) +
  theme_minimal()
#WORKING
ggplot(cleaned_data_sex_rates_finw %>% 
         filter(!is.na(Sex_specific_rate_acsc_s), !is.na(Sex)), 
       aes(x = Year, y = Sex_specific_rate_acsc_s, color = Sex)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ State) +  # Separate plots for each state
  labs(
    title = "Sex-Specific ACSC Hospitalization Rates from 2016-2020",
    subtitle = "Total Working Population (18-66)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Sex Category",
  ) +
  theme_minimal()


#ACUTE
ggplot(cleaned_data_sex_rates_fin %>% 
         filter(!is.na(Sex_specific_rate_acute_s), !is.na(Sex)), 
       aes(x = Year, y = Sex_specific_rate_acute_s, color = Sex)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ State) +  # Separate plots for each state
  labs(
    title = "Sex-Specific Acute Hospitalization Rates from 2016-2020",
    subtitle = "Total Adult Population (18+)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Sex Category",
  ) +
  theme_minimal()
#WORKING
ggplot(cleaned_data_sex_rates_finw %>% 
         filter(!is.na(Sex_specific_rate_acute_s), !is.na(Sex)), 
       aes(x = Year, y = Sex_specific_rate_acute_s, color = Sex)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ State) +  # Separate plots for each state
  labs(
    title = "Sex-Specific Acute Hospitalization Rates from 2016-2020",
    subtitle = "Total Working Population (18-66)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Sex Category",
  ) +
  theme_minimal()


#CHRONIC
ggplot(cleaned_data_sex_rates_fin %>% 
         filter(!is.na(Sex_specific_rate_chronic_s), !is.na(Sex)), 
       aes(x = Year, y = Sex_specific_rate_chronic_s, color = Sex)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ State) +  # Separate plots for each state
  labs(
    title = "Sex-Specific Chronic Hospitalization Rates from 2016-2020",
    subtitle = "Total Adult Population (18+)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Sex Category",
  ) +
  theme_minimal()
#WORKING
ggplot(cleaned_data_sex_rates_finw %>% 
         filter(!is.na(Sex_specific_rate_chronic_s), !is.na(Sex)), 
       aes(x = Year, y = Sex_specific_rate_chronic_s, color = Sex)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ State) +  # Separate plots for each state
  labs(
    title = "Sex-Specific Chronic Hospitalization Rates from 2016-2020",
    subtitle = "Total Working Population (18-66)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Sex Category",
  ) +
  theme_minimal()

#DIABETES
ggplot(cleaned_data_sex_rates_fin %>% 
         filter(!is.na(Sex_specific_rate_diabetes_s), !is.na(Sex)), 
       aes(x = Year, y = Sex_specific_rate_diabetes_s, color = Sex)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ State) +  # Separate plots for each state
  labs(
    title = "Sex-Specific Diabetes Hospitalization Rates from 2016-2020",
    subtitle = "Total Adult Population (18+)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Sex Category",
  ) +
  theme_minimal()
#WORKING
ggplot(cleaned_data_sex_rates_finw %>% 
         filter(!is.na(Sex_specific_rate_diabetes_s), !is.na(Sex)), 
       aes(x = Year, y = Sex_specific_rate_diabetes_s, color = Sex)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ State) +  # Separate plots for each state
  labs(
    title = "Sex-Specific Diabetes Hospitalization Rates from 2016-2020",
    subtitle = "Total Working Population (18-66)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Sex Category",
  ) +
  theme_minimal()

#Year total (TABLE 1)
cleaned_data_sex_rates_fin%>% 
  filter(!is.na(Sex_specific_rate_acsc_y), !is.na(Sex))%>%
  filter(!is.na(Sex_specific_rate_acute_y), !is.na(Sex))%>%
  filter(!is.na(Sex_specific_rate_chronic_y), !is.na(Sex))%>%
  filter(!is.na(Sex_specific_rate_diabetes_y), !is.na(Sex))%>%
  ggplot(aes(x = Year)) +
  geom_line(aes(y=Sex_specific_rate_acsc_y,color="ACSC"),linewidth=1) +
  geom_line(aes(y=Sex_specific_rate_acute_y,color="Acute"),linewidth=1) +
  geom_line(aes(y=Sex_specific_rate_chronic_y,color="Chronic"),linewidth=1) +
  geom_line(aes(y=Sex_specific_rate_diabetes_y,color="Diabetes"),linewidth=1) +
  geom_point(aes(y=Sex_specific_rate_acsc_y,color="ACSC"),size=1.5) +
  geom_point(aes(y=Sex_specific_rate_acute_y,color="Acute"),size=1.5) +
  geom_point(aes(y=Sex_specific_rate_chronic_y,color="Chronic"),size=1.5) +
  geom_point(aes(y=Sex_specific_rate_diabetes_y,color="Diabetes"),size=1.5) +
  facet_wrap(~Sex)+
  labs(
    title = "Hospitalization Specific Rates Over Time",
    subtitle = "Adult Population (18+), Stratified by Sex",
    x = "Year",
    y = "Rate (per 1,000)",
    color = "Rate Type"
  ) + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  
  )
#WORKING
cleaned_data_sex_rates_finw%>% 
  filter(!is.na(Sex_specific_rate_acsc_y), !is.na(Sex))%>%
  filter(!is.na(Sex_specific_rate_acute_y), !is.na(Sex))%>%
  filter(!is.na(Sex_specific_rate_chronic_y), !is.na(Sex))%>%
  filter(!is.na(Sex_specific_rate_diabetes_y), !is.na(Sex))%>%
  ggplot(aes(x = Year)) +
  geom_line(aes(y=Sex_specific_rate_acsc_y,color="ACSC"),linewidth=1) +
  geom_line(aes(y=Sex_specific_rate_acute_y,color="Acute"),linewidth=1) +
  geom_line(aes(y=Sex_specific_rate_chronic_y,color="Chronic"),linewidth=1) +
  geom_line(aes(y=Sex_specific_rate_diabetes_y,color="Diabetes"),linewidth=1) +
  geom_point(aes(y=Sex_specific_rate_acsc_y,color="ACSC"),size=1.5) +
  geom_point(aes(y=Sex_specific_rate_acute_y,color="Acute"),size=1.5) +
  geom_point(aes(y=Sex_specific_rate_chronic_y,color="Chronic"),size=1.5) +
  geom_point(aes(y=Sex_specific_rate_diabetes_y,color="Diabetes"),size=1.5) +
  facet_wrap(~Sex)+
  labs(
    title = "Hospitalization Specific Rates Over Time",
    subtitle = "Working Population (18-66), Stratified by Sex",
    x = "Year",
    y = "Rate (per 1,000)",
    color = "Rate Type"
  ) + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  
  )

##MERGE DATASETS 
cleaned_data_age_race_sex_merged<-cleaned_data_age_race_merged%>%
  left_join(cleaned_data_sex_rates_fin, by=c("GEOID","County","State","Year","Sex"))
save(cleaned_data_age_race_sex_merged,file="cleaned_data_age_race_sex_merged.RData")
load("cleaned_data_age_race_sex_merged.RData")
#WORKING POPULATION DATASET
cleaned_data_age_race_sex_mergedw<-cleaned_data_age_race_mergedw%>%
  left_join(cleaned_data_sex_rates_finw, by=c("GEOID","County","State","Year","Sex"))
save(cleaned_data_age_race_sex_mergedw,file="cleaned_data_age_race_sex_mergedw.RData")