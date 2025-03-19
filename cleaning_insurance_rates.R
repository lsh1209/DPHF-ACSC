
##TOTAL POPULATION 
library(tidyverse)

library(tidycensus)


years <- 2016:2020
states <- c("AZ", "FL", "GA", "KY", "MA", "MN", "NE", "NC", "NJ", "NY", "OR", "WA", "WI")
b27010_ <- function(var_num) {
  if (sum(!(var_num %in% 1:999)) > 0) {
    stop("values in var_num must be between 1 and 999.")
  }
  paste0("B27010_", sprintf("%03d", var_num))
}
# Create a list to store data
raw_acs_insur_list <- lapply(years, function(acs_year) {
  # Use survey_used logic based on the year
  acs_survey <- "acs5"  # Adjust survey type based on the year
  
  # GET ACS VAR LIST/CODEBOOK (IT'S HANDY TO LOOK AT)
  acs_vars <- load_variables(year = acs_year, dataset = acs_survey)
  
  # DOWNLOAD RAW DATA
  data <- get_acs(
    geography = "county",
    year = acs_year,
    survey = acs_survey,
    state = states,
    variables = b27010_(c(1, 18, 20:25, 27:34, 
                          36:41, 43:51, 53:57, 59:66)) ###Removed under 18 populations
  )
  
  # Add the year to the dataset
  data$Year <- acs_year
  
  return(data)
})

# Combine all the years' data into one data frame
raw_acs_insur <- bind_rows(raw_acs_insur_list)

county_insurance <- raw_acs_insur %>%
  dplyr::mutate(insurance_type = 
                  dplyr::case_when(
                    # TOTAL (DENOMINATOR)
                    variable %in% b27010_(c(18,34,51)) ~ "ins_denom", ##Makes it the total for 18-34, 35-64, and 65+ 
                    # NONE = NO INSURANCE
                    variable %in% b27010_(c(33,50,66)) ~ "ins_none",
                    # MEDICAID ALONE
                    variable %in% 
                      b27010_(c(23,39))     ~ "ins_medicaid_alone",
                    # MEDICAID+MEDICARE
                    variable %in% 
                      b27010_(c(29,62,46)) ~ "ins_both_medicaid_care",
                    # MEDICARE ALONE
                    variable %in% 
                      b27010_(c(22,38,55))  ~ "ins_medicare_alone",
                    # PRIVATE = EMPLOYER (ALONE OR COMBINATION), 
                    #           DIRECT PURCHASE (ALONE OR COMBINATION)
                    #           PRIVATE ONLY COMBINATIONS
                    #           "OTHER COVERAGE COMBINATIONS" (PUBLIC + PRIVATE)
                    variable %in% b27010_(c(20:21, 27:28,
                                            30,32,36:37, 43:45, 47,49,53:54, 
                                            59:61, 63, 65)) ~ "ins_private",
                    # OTHER (PUBLIC) = MILITARY & OTHER PUBLIC ONLY COMBINATIONS
                    variable %in% b27010_(c(24:25,31,40:41,48,
                                            56:57,64)) ~ "ins_other")) %>%
  dplyr::group_by(GEOID, insurance_type, Year) %>%
  dplyr::summarize(estimate = sum(estimate, na.rm = F)) %>%
  tidyr::pivot_wider(id_cols = c(GEOID, Year),
                     names_from = insurance_type,
                     values_from = estimate) %>%
  dplyr::mutate(ins_medicaid_2  = ins_medicaid_alone + ins_both_medicaid_care, ##FIX 
                check_total = ins_denom - 
                  (ins_none + ins_medicaid_alone +  ins_both_medicaid_care +
                     ins_medicare_alone + ins_private + ins_other))

summary(county_insurance$check_total)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#       0       0       0       0       0       0      
# REMOVE THE EXTRA CHECK_TOTAL COLUMN
county_insurance <- county_insurance %>% dplyr::select(-check_total)

## HERE, COMBINE INSURANCE TYPES INTO APPROPRIATE CATEGORIES
temp_insurance <- county_insurance %>%
  select(GEOID, Year,
         pop_ins_Medicare = ins_medicare_alone,
         pop_ins_Medicaid = ins_medicaid_2,
         pop_ins_Private  = ins_private,
         pop_ins_None     = ins_none,
         pop_ins_Other    = ins_other) %>%
  pivot_longer(cols = c(pop_ins_Medicaid, pop_ins_Medicare,
                        pop_ins_Private, pop_ins_None,
                        pop_ins_Other),
               names_prefix = "pop_ins_",
               names_to = "insurance_ses",
               values_to = "pop_ins")

#MERGE TO MAIN DATASET
load("cleaned_merged_final.RData")

cleaned_data_1<-cleaned_merged_final%>%
  select(
    GEOID,
    County,
    State,
    Year,
    `Insurance Type`,
    `ACSC Hospitalization`,
    `Acute Hospitalization`,
    `Chronic Hospitalization`,
    `Diabetes Hospitalization`
  )
cleaned_data_1=cleaned_data_1%>%
  mutate(`Insurance Type`=case_when(
  `Insurance Type` %in% c("self pay", "no charge") ~ "None",
  `Insurance Type`=="Medicare" ~"Medicare",
  `Insurance Type`== "Medicaid" ~ "Medicaid",
  `Insurance Type`=="Other"~ "Other",
  `Insurance Type`== "Private"~"Private"
  ))

cleaned_data_3 <- cleaned_data_1 %>%
  left_join(temp_insurance, by = c("GEOID","Year", "Insurance Type"="insurance_ses"))


#Rate Calculations
# Step 1: Calculate hospitalizations and insurance populations at the county level
cases_by_insurance_county <- cleaned_data_3 %>%
  group_by(GEOID, County, State, Year, `Insurance Type`,pop_ins) %>% 
  summarise(
    ACSC_Hosp = sum(`ACSC Hospitalization`, na.rm = TRUE),
    Acute_Hosp = sum(`Acute Hospitalization`, na.rm = TRUE),
    Chronic_Hosp = sum(`Chronic Hospitalization`, na.rm = TRUE),
    Diabetes_Hosp = sum(`Diabetes Hospitalization`, na.rm = TRUE),
  ) %>%
  ungroup()
# Step 2: Calculate insurance-specific rates
ins_specific_rates <- cases_by_insurance_county %>%
  mutate( 
    ins_specific_rate_acsc_c = ACSC_Hosp / pop_ins * 1000,
    ins_specific_rate_acute_c = Acute_Hosp / pop_ins * 1000,
    ins_specific_rate_chronic_c = Chronic_Hosp / pop_ins * 1000,
    ins_specific_rate_diabetes_c= Diabetes_Hosp / pop_ins * 1000
  ) %>%
  select(GEOID, County, State, Year, `Insurance Type`, ins_specific_rate_acsc_c, ins_specific_rate_acute_c, 
         ins_specific_rate_chronic_c, ins_specific_rate_diabetes_c, pop_ins)
# Step 6: Calculate crude rates at the county level 
ins_rates_crude <- cases_by_insurance_county %>%
  group_by(GEOID, County, State, Year) %>%
  summarise(
    Crude_ACSC_Rate_ins_c = sum(ACSC_Hosp, na.rm = TRUE) / sum(pop_ins, na.rm = TRUE) * 1000,
    Crude_Acute_Rate_ins_c = sum(Acute_Hosp, na.rm = TRUE) / sum(pop_ins, na.rm = TRUE) * 1000,
    Crude_Chronic_Rate_ins_c = sum(Chronic_Hosp, na.rm = TRUE) / sum(pop_ins, na.rm = TRUE) * 1000,
    Crude_Diabetes_Rate_ins_c = sum(Diabetes_Hosp, na.rm = TRUE) / sum(pop_ins, na.rm = TRUE) * 1000
  ) %>%
  ungroup()

# Step 7: Combine ins-specific rates, crude rates, and adjusted rates
final_rates_county_ins <- ins_rates_crude %>%
  left_join(ins_specific_rates, by = c("GEOID", "County", "State", "Year"))

#YEAR
# Create a list to store data
raw_acs_insur_list_state <- lapply(years, function(acs_year) {
  # Use survey_used logic based on the year
  acs_survey <- "acs5" 
  
  # GET ACS VAR LIST/CODEBOOK (IT'S HANDY TO LOOK AT)
  acs_vars <- load_variables(year = acs_year, dataset = acs_survey)
  
  # CREATE A HELPER FUNCTION FOR MAKING A LIST OF VARIABLES NAMES FOR 
  # ACS TABLE B27010
  
  # DOWNLOAD RAW DATA
  data_state <- get_acs(
    geography = "state",
    year = acs_year,
    survey = acs_survey,
    state = states,
    variables = b27010_(c(1, 18 , 20:25, 27:34, 
                          36:41, 43:51, 53:57, 59:66))
  )
  
  # Add the year to the dataset
  data_state$Year <- acs_year
  
  return(data_state)
})

# Combine all the years' data into one data frame
raw_acs_insur_state <- bind_rows(raw_acs_insur_list_state)

state_insurance<- raw_acs_insur_state %>%
  dplyr::mutate(insurance_type = 
                  dplyr::case_when(
                    # TOTAL (DENOMINATOR)
                    variable %in% b27010_(c(18,34,51)) ~ "ins_denom",
                    # NONE = NO INSURANCE
                    variable %in% b27010_(c(33,50,66)) ~ "ins_none",
                    # MEDICAID ALONE
                    variable %in% 
                      b27010_(c(23,39))     ~ "ins_medicaid_alone",
                    # MEDICAID+MEDICARE
                    variable %in% 
                      b27010_(c(29,62,46)) ~ "ins_both_medicaid_care",
                    # MEDICARE ALONE
                    variable %in% 
                      b27010_(c(22,38,55))  ~ "ins_medicare_alone",
                    # PRIVATE = EMPLOYER (ALONE OR COMBINATION), 
                    #           DIRECT PURCHASE (ALONE OR COMBINATION)
                    #           PRIVATE ONLY COMBINATIONS
                    #           "OTHER COVERAGE COMBINATIONS" (PUBLIC + PRIVATE)
                    variable %in% b27010_(c(20:21, 27:28,
                                            30,32,36:37, 43:45, 47,49,53:54, 
                                            59:61, 63, 65)) ~ "ins_private",
                    # OTHER (PUBLIC) = MILITARY & OTHER PUBLIC ONLY COMBINATIONS
                    variable %in% b27010_(c(24:25,31,40:41,48,
                                            56:57,64)) ~ "ins_other")) %>%
  dplyr::group_by(GEOID, NAME, insurance_type, Year) %>%
  dplyr::summarize(estimate = sum(estimate, na.rm = F)) %>%
  tidyr::pivot_wider(id_cols = c(GEOID, NAME,Year),
                     names_from = insurance_type,
                     values_from = estimate) %>%
  dplyr::mutate(ins_medicaid_2  = ins_medicaid_alone + ins_both_medicaid_care, 
                check_total = ins_denom - 
                  (ins_none + ins_medicaid_alone +  ins_both_medicaid_care +
                     ins_medicare_alone + ins_private + ins_other))

summary(state_insurance$check_total)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#       0       0       0       0       0       0      87
# REMOVE THE EXTRA CHECK_TOTAL COLUMN
state_insurance<- state_insurance%>% dplyr::select(-check_total)

## HERE, COMBINE INSURANCE TYPES INTO APPROPRIATE CATEGORIES
temp_insurance_state <- state_insurance %>%
  select(GEOID,NAME,Year,
         pop_ins_Medicare = ins_medicare_alone,
         pop_ins_Medicaid = ins_medicaid_2,
         pop_ins_Private  = ins_private,
         pop_ins_None     = ins_none,
         pop_ins_Other    = ins_other) %>%
  pivot_longer(cols = c(pop_ins_Medicaid, pop_ins_Medicare,
                        pop_ins_Private, pop_ins_None,
                        pop_ins_Other),
               names_prefix = "pop_ins_",
               names_to = "insurance_ses",
               values_to = "pop_ins_state")

cleaned_data_4 <- cleaned_data_3 %>%
  left_join(temp_insurance_state, by = c("State"="NAME","Year", "Insurance Type"="insurance_ses"))



final_rates_insurance_state_distinct <- cleaned_data_4 %>%
  distinct(State, Year, `Insurance Type`, .keep_all = TRUE)

final_rates_insurance_state_distinct1<-final_rates_insurance_state_distinct%>%
  filter(!`Insurance Type`=="NA")%>%
  group_by(Year, `Insurance Type`)%>%
  mutate(pop_ins_year = sum(pop_ins_state, na.rm=TRUE))%>% ungroup()

pop_ins_summary <- cleaned_data_4 %>%
  left_join(final_rates_insurance_state_distinct1, by=c("GEOID.x", "County",
                                                        "State","Year",
                                                        "Insurance Type",
                                                        "ACSC Hospitalization",
                                                        "Acute Hospitalization",
                                                        "Chronic Hospitalization",
                                                        "Diabetes Hospitalization",
                                                        "pop_ins", "GEOID.y",
                                                        "pop_ins_state"))%>%
  group_by(Year, `Insurance Type`) %>%
  mutate(pop_ins_year = first(na.omit(pop_ins_year))) %>%  # Fill missing values
  ungroup()

pop_ins_summary<-pop_ins_summary%>%
  filter(!`Insurance Type`=="NA")%>%
  select(
    -GEOID.y
  )%>%
  rename(
    "GEOID"=GEOID.x
  )
#Rate Calculations
# Step 1: Calculate hospitalizations and insurance populations at the state level
cases_by_insurance_state <- pop_ins_summary %>%
  group_by(State, Year, `Insurance Type`,pop_ins_state) %>% 
  summarise(
    ACSC_Hosp = sum(`ACSC Hospitalization`, na.rm = TRUE),
    Acute_Hosp = sum(`Acute Hospitalization`, na.rm = TRUE),
    Chronic_Hosp = sum(`Chronic Hospitalization`, na.rm = TRUE),
    Diabetes_Hosp = sum(`Diabetes Hospitalization`, na.rm = TRUE),
  ) %>%
  ungroup()
# Step 2: Calculate insurance-specific rates
ins_specific_rates_state <- cases_by_insurance_state %>%
  mutate( 
    ins_specific_rate_acsc_s = ACSC_Hosp / pop_ins_state * 1000,
    ins_specific_rate_acute_s = Acute_Hosp / pop_ins_state * 1000,
    ins_specific_rate_chronic_s = Chronic_Hosp / pop_ins_state * 1000,
    ins_specific_rate_diabetes_s= Diabetes_Hosp / pop_ins_state * 1000
  ) %>%
  select(State, Year, `Insurance Type`, ins_specific_rate_acsc_s, ins_specific_rate_acute_s, 
         ins_specific_rate_chronic_s, ins_specific_rate_diabetes_s, pop_ins_state)
# Step 6: Calculate crude rates at the state level 
ins_rates_crude_state <- cases_by_insurance_state %>%
  group_by(State, Year) %>%
  summarise(
    Crude_ACSC_Rate_ins_s= sum(ACSC_Hosp, na.rm = TRUE) / sum(pop_ins_state, na.rm = TRUE) * 1000,
    Crude_Acute_Rate_ins_s = sum(Acute_Hosp, na.rm = TRUE) / sum(pop_ins_state, na.rm = TRUE) * 1000,
    Crude_Chronic_Rate_ins_s = sum(Chronic_Hosp, na.rm = TRUE) / sum(pop_ins_state, na.rm = TRUE) * 1000,
    Crude_Diabetes_Rate_ins_s = sum(Diabetes_Hosp, na.rm = TRUE) / sum(pop_ins_state, na.rm = TRUE) * 1000
  ) %>%
  ungroup()

# Step 7: Combine ins-specific rates, crude rates, and adjusted rates
final_rates_state_ins <- ins_rates_crude_state %>%
  left_join(ins_specific_rates_state, by = c("State", "Year"))

# Step 1: Calculate hospitalizations and insurance populations at the year level
cases_by_insurance_year <- pop_ins_summary %>%
  group_by(Year, `Insurance Type`,pop_ins_year) %>% 
  summarise(
    ACSC_Hosp = sum(`ACSC Hospitalization`, na.rm = TRUE),
    Acute_Hosp = sum(`Acute Hospitalization`, na.rm = TRUE),
    Chronic_Hosp = sum(`Chronic Hospitalization`, na.rm = TRUE),
    Diabetes_Hosp = sum(`Diabetes Hospitalization`, na.rm = TRUE),
  ) %>%
  ungroup()
# Step 2: Calculate insurance-specific rates
ins_specific_rates_year <- cases_by_insurance_year %>%
  mutate( 
    ins_specific_rate_acsc_y = ACSC_Hosp / pop_ins_year * 1000,
    ins_specific_rate_acute_y = Acute_Hosp / pop_ins_year * 1000,
    ins_specific_rate_chronic_y = Chronic_Hosp / pop_ins_year * 1000,
    ins_specific_rate_diabetes_y= Diabetes_Hosp / pop_ins_year * 1000
  ) %>%
  select(Year, `Insurance Type`, ins_specific_rate_acsc_y, ins_specific_rate_acute_y, 
         ins_specific_rate_chronic_y, ins_specific_rate_diabetes_y, pop_ins_year)
# Step 6: Calculate crude rates at the year level 
ins_rates_crude_year <- cases_by_insurance_year %>%
  group_by(Year) %>%
  summarise(
    Crude_ACSC_Rate_ins_y= sum(ACSC_Hosp, na.rm = TRUE) / sum(pop_ins_year, na.rm = TRUE) * 1000,
    Crude_Acute_Rate_ins_y = sum(Acute_Hosp, na.rm = TRUE) / sum(pop_ins_year, na.rm = TRUE) * 1000,
    Crude_Chronic_Rate_ins_y = sum(Chronic_Hosp, na.rm = TRUE) / sum(pop_ins_year, na.rm = TRUE) * 1000,
    Crude_Diabetes_Rate_ins_y = sum(Diabetes_Hosp, na.rm = TRUE) / sum(pop_ins_year, na.rm = TRUE) * 1000
  ) %>%
  ungroup()

# Step 7: Combine ins-specific rates, crude rates, and adjusted rates
final_rates_year_ins <- ins_rates_crude_year %>%
  left_join(ins_specific_rates_year, by = c("Year"))


#MERGE DATA
colnames(final_rates_county_ins)
colnames(final_rates_state_ins)
colnames(final_rates_year_ins)

duplicates_county_ins <- final_rates_county_ins %>%
  group_by(State, Year, `Insurance Type`) %>%
  filter(n() > 1)


duplicates_state_ins <- final_rates_state_ins %>%
  group_by(State, Year, `Insurance Type`) %>%
  filter(n() > 1)

final_rates_county_ins_distinct <- final_rates_county_ins %>%
  distinct(County, State, Year, `Insurance Type`, .keep_all = TRUE)
final_rates_state_ins_distinct <- final_rates_state_ins %>%
  distinct(State, Year, `Insurance Type`, .keep_all = TRUE)
final_data_ins_rates <- final_rates_county_ins_distinct %>%
  left_join(final_rates_state_ins_distinct, by = c("State", "Year", "Insurance Type"))

duplicates_year_ins <- final_rates_year_ins %>%
  group_by( Year, `Insurance Type`) %>%
  filter(n() > 1)

final_rates_year_ins_distinct <- final_rates_year_ins %>%
  distinct(Year, `Insurance Type`, .keep_all = TRUE)

cleaned_data_ins_rates_fin<-final_data_ins_rates%>%
  left_join(final_rates_year_ins_distinct, by=c("Year","Insurance Type"))

save(cleaned_data_ins_rates_fin, file="cleaned_data_ins_final.RData")
load("cleaned_data_ins_final.RData")

#GRAPHS- years (TABLE 1)
#ACSC
ggplot(cleaned_data_ins_rates_fin %>% 
         filter(!is.na(ins_specific_rate_acsc_s), !is.na(`Insurance Type`)), 
       aes(x = Year, y = ins_specific_rate_acsc_s, color = `Insurance Type`)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ State) +  # Separate plots for each state
  labs(
    title = "Insurance-Specific ACSC Hospitalization Rates from 2016-2020",
    subtitle = "Total Adult Population (18+)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Insurance Category"
  )+
  theme_minimal()


#ACUTE
ggplot(cleaned_data_ins_rates_fin %>% 
         filter(!is.na(ins_specific_rate_acute_s), !is.na(`Insurance Type`)), 
       aes(x = Year, y = ins_specific_rate_acute_s, color = `Insurance Type`)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ State) +  # Separate plots for each state
  labs(
    title = "Insurance-Specific Acute Hospitalization Rates from 2016-2020",
    subtitle = "Total Adult Population (18+)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Insurance Category"
  ) +
  theme_minimal()


#CHRONIC
ggplot(cleaned_data_ins_rates_fin %>% 
         filter(!is.na(ins_specific_rate_chronic_s), !is.na(`Insurance Type`)), 
       aes(x = Year, y = ins_specific_rate_chronic_s, color =`Insurance Type`)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ State) +  # Separate plots for each state
  labs(
    title = "Insurance-Specific Chronic Hospitalization Rates from 2016-2020",
    subtitle = "Total Adult Population (18+)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Insurance Category"
  ) +
  theme_minimal()


#DIABETES
ggplot(cleaned_data_ins_rates_fin %>% 
         filter(!is.na(ins_specific_rate_diabetes_s), !is.na(`Insurance Type`)), 
       aes(x = Year, y = ins_specific_rate_diabetes_s, color = `Insurance Type`)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ State) +  # Separate plots for each state
  labs(
    title = "Insurance-Specific Diabetes Hospitalization Rates from 2016-2020",
    subtitle = "Total Adult Population (18+)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Insurance Category"
  ) +
  theme_minimal()


#Year total (TABLE 1)
cleaned_data_ins_rates_fin%>% 
  filter(!is.na(ins_specific_rate_acsc_y), !is.na(`Insurance Type`))%>%
  filter(!is.na(ins_specific_rate_acute_y), !is.na(`Insurance Type`))%>%
  filter(!is.na(ins_specific_rate_chronic_y), !is.na(`Insurance Type`))%>%
  filter(!is.na(ins_specific_rate_diabetes_y), !is.na(`Insurance Type`))%>%
  ggplot(aes(x = Year)) +
  geom_line(aes(y=ins_specific_rate_acsc_y,color="ACSC"),linewidth=1) +
  geom_line(aes(y=ins_specific_rate_acute_y,color="Acute"),linewidth=1) +
  geom_line(aes(y=ins_specific_rate_chronic_y,color="Chronic"),linewidth=1) +
  geom_line(aes(y=ins_specific_rate_diabetes_y,color="Diabetes"),linewidth=1) +
  geom_point(aes(y=ins_specific_rate_acsc_y,color="ACSC"),size=1.5) +
  geom_point(aes(y=ins_specific_rate_acute_y,color="Acute"),size=1.5) +
  geom_point(aes(y=ins_specific_rate_chronic_y,color="Chronic"),size=1.5) +
  geom_point(aes(y=ins_specific_rate_diabetes_y,color="Diabetes"),size=1.5) +
  facet_wrap(~`Insurance Type`)+
  labs(
    title = "Hospitalization Specific Rates Over Time",
    subtitle = "Adult Population (18+), Stratified by Insurance Type",
    x = "Year",
    y = "Rate (per 1,000)",
    color = "Rate Type"
  ) + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  
  )

##MERGE DATASETS 
cleaned_data_age_race_sex_ins_merged<-cleaned_data_age_race_sex_merged%>%
  left_join(cleaned_data_ins_rates_fin, by=c("GEOID","County","State","Year","Insurance Type"))
save(cleaned_data_age_race_sex_ins_merged,file="cleaned_data_age_race_sex_ins_merged.RData")
load("cleaned_data_age_race_sex_merged.RData")



##WORKING POPULATION (REMOVE AGE GROUP 65+)
years <- 2016:2020
states <- c("AZ", "FL", "GA", "KY", "MA", "MN", "NE", "NC", "NJ", "NY", "OR", "WA", "WI")
b27010_ <- function(var_num) {
  if (sum(!(var_num %in% 1:999)) > 0) {
    stop("values in var_num must be between 1 and 999.")
  }
  paste0("B27010_", sprintf("%03d", var_num))
}
# Create a list to store data
raw_acs_insur_list_working <- lapply(years, function(acs_year) {
  # Use survey_used logic based on the year
  acs_survey <- "acs5"  # Adjust survey type based on the year
  
  # GET ACS VAR LIST/CODEBOOK (IT'S HANDY TO LOOK AT)
  acs_vars <- load_variables(year = acs_year, dataset = acs_survey)
  
  # DOWNLOAD RAW DATA
  data <- get_acs(
    geography = "county",
    year = acs_year,
    survey = acs_survey,
    state = states,
    variables = b27010_(c(1, 18, 20:25, 27:34, 
                          36:41, 43:50)) ###Removed under 18 and 65+ populations
  )
  
  # Add the year to the dataset
  data$Year <- acs_year
  
  return(data)
})

# Combine all the years' data into one data frame
raw_acs_insur_working <- bind_rows(raw_acs_insur_list_working)

county_insurance_working <- raw_acs_insur_working %>%
  dplyr::mutate(insurance_type = 
                  dplyr::case_when(
                    # TOTAL (DENOMINATOR)
                    variable %in% b27010_(c(18,34)) ~ "ins_denom", ##Makes it the total for 18-34 & 35-64 
                    # NONE = NO INSURANCE
                    variable %in% b27010_(c(33,50)) ~ "ins_none",
                    # MEDICAID ALONE
                    variable %in% 
                      b27010_(c(23,39))     ~ "ins_medicaid_alone",
                    # MEDICAID+MEDICARE
                    variable %in% 
                      b27010_(c(29,46)) ~ "ins_both_medicaid_care",
                    # MEDICARE ALONE
                    variable %in% 
                      b27010_(c(22,38))  ~ "ins_medicare_alone",
                    # PRIVATE = EMPLOYER (ALONE OR COMBINATION), 
                    #           DIRECT PURCHASE (ALONE OR COMBINATION)
                    #           PRIVATE ONLY COMBINATIONS
                    #           "OTHER COVERAGE COMBINATIONS" (PUBLIC + PRIVATE)
                    variable %in% b27010_(c(20:21, 27:28,
                                            30,32,36:37, 43:45, 47,49)) ~ "ins_private",
                    # OTHER (PUBLIC) = MILITARY & OTHER PUBLIC ONLY COMBINATIONS
                    variable %in% b27010_(c(24:25,31,40:41,48)) ~ "ins_other")) %>%
  dplyr::group_by(GEOID, insurance_type, Year) %>%
  dplyr::summarize(estimate = sum(estimate, na.rm = F)) %>%
  tidyr::pivot_wider(id_cols = c(GEOID, Year),
                     names_from = insurance_type,
                     values_from = estimate) %>%
  dplyr::mutate(ins_medicaid_2  = ins_medicaid_alone + ins_both_medicaid_care, 
                check_total = ins_denom - 
                  (ins_none + ins_medicaid_alone +  ins_both_medicaid_care +
                     ins_medicare_alone + ins_private + ins_other))

summary(county_insurance_working$check_total)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#       0       0       0       0       0       0      
# REMOVE THE EXTRA CHECK_TOTAL COLUMN
county_insurance_working <- county_insurance_working %>% dplyr::select(-check_total)

## HERE, COMBINE INSURANCE TYPES INTO APPROPRIATE CATEGORIES
temp_insurance_working <- county_insurance_working %>%
  select(GEOID, Year,
         pop_ins_Medicare = ins_medicare_alone,
         pop_ins_Medicaid = ins_medicaid_2,
         pop_ins_Private  = ins_private,
         pop_ins_None     = ins_none,
         pop_ins_Other    = ins_other) %>%
  pivot_longer(cols = c(pop_ins_Medicaid, pop_ins_Medicare,
                        pop_ins_Private, pop_ins_None,
                        pop_ins_Other),
               names_prefix = "pop_ins_",
               names_to = "insurance_ses",
               values_to = "pop_ins")

#MERGE TO MAIN DATASET
load("cleaned_merged_final.RData")
cleaned_data_1w<-cleaned_merged_final%>%
  filter(`Age Cat.` %in% c("18-29","30-44","45-66"))
cleaned_data_2w<-cleaned_data_1w%>%
  select(
    GEOID,
    County,
    State,
    Year,
    `Insurance Type`,
    `ACSC Hospitalization`,
    `Acute Hospitalization`,
    `Chronic Hospitalization`,
    `Diabetes Hospitalization`
  )
cleaned_data_2w=cleaned_data_2w%>%
  mutate(`Insurance Type`=case_when(
    `Insurance Type` %in% c("self pay", "no charge") ~ "None",
    `Insurance Type`=="Medicare" ~"Medicare",
    `Insurance Type`== "Medicaid" ~ "Medicaid",
    `Insurance Type`=="Other"~ "Other",
    `Insurance Type`== "Private"~"Private"
  ))

cleaned_data_3w <- cleaned_data_2w %>%
  left_join(temp_insurance_working, by = c("GEOID","Year", "Insurance Type"="insurance_ses"))


#Rate Calculations
# Step 1: Calculate hospitalizations and insurance populations at the county level
cases_by_insurance_countyw <- cleaned_data_3w %>%
  group_by(GEOID, County, State, Year, `Insurance Type`,pop_ins) %>% 
  summarise(
    ACSC_Hosp = sum(`ACSC Hospitalization`, na.rm = TRUE),
    Acute_Hosp = sum(`Acute Hospitalization`, na.rm = TRUE),
    Chronic_Hosp = sum(`Chronic Hospitalization`, na.rm = TRUE),
    Diabetes_Hosp = sum(`Diabetes Hospitalization`, na.rm = TRUE),
  ) %>%
  ungroup()
# Step 2: Calculate insurance-specific rates
ins_specific_ratesw <- cases_by_insurance_countyw %>%
  mutate( 
    ins_specific_rate_acsc_c = ACSC_Hosp / pop_ins * 1000,
    ins_specific_rate_acute_c = Acute_Hosp / pop_ins * 1000,
    ins_specific_rate_chronic_c = Chronic_Hosp / pop_ins * 1000,
    ins_specific_rate_diabetes_c= Diabetes_Hosp / pop_ins * 1000
  ) %>%
  select(GEOID, County, State, Year, `Insurance Type`, ins_specific_rate_acsc_c, ins_specific_rate_acute_c, 
         ins_specific_rate_chronic_c, ins_specific_rate_diabetes_c, pop_ins)
# Step 6: Calculate crude rates at the county level 
ins_rates_crudew <- cases_by_insurance_countyw %>%
  group_by(GEOID, County, State, Year) %>%
  summarise(
    Crude_ACSC_Rate_ins_c = sum(ACSC_Hosp, na.rm = TRUE) / sum(pop_ins, na.rm = TRUE) * 1000,
    Crude_Acute_Rate_ins_c = sum(Acute_Hosp, na.rm = TRUE) / sum(pop_ins, na.rm = TRUE) * 1000,
    Crude_Chronic_Rate_ins_c = sum(Chronic_Hosp, na.rm = TRUE) / sum(pop_ins, na.rm = TRUE) * 1000,
    Crude_Diabetes_Rate_ins_c = sum(Diabetes_Hosp, na.rm = TRUE) / sum(pop_ins, na.rm = TRUE) * 1000
  ) %>%
  ungroup()

# Step 7: Combine ins-specific rates, crude rates, and adjusted rates
final_rates_county_insw <- ins_rates_crudew %>%
  left_join(ins_specific_ratesw, by = c("GEOID", "County", "State", "Year"))

#YEAR
# Create a list to store data
raw_acs_insur_list_state_working <- lapply(years, function(acs_year) {
  # Use survey_used logic based on the year
  acs_survey <- "acs5" 
  
  # GET ACS VAR LIST/CODEBOOK (IT'S HANDY TO LOOK AT)
  acs_vars <- load_variables(year = acs_year, dataset = acs_survey)
  
  # CREATE A HELPER FUNCTION FOR MAKING A LIST OF VARIABLES NAMES FOR 
  # ACS TABLE B27010
  
  # DOWNLOAD RAW DATA
  data_state <- get_acs(
    geography = "state",
    year = acs_year,
    survey = acs_survey,
    state = states,
    variables = b27010_(c(1, 18 , 20:25, 27:34, 
                          36:41, 43:50))
  )
  
  # Add the year to the dataset
  data_state$Year <- acs_year
  
  return(data_state)
})

# Combine all the years' data into one data frame
raw_acs_insur_state_working <- bind_rows(raw_acs_insur_list_state_working)

state_insurance_working<- raw_acs_insur_state_working %>%
  dplyr::mutate(insurance_type = 
                  dplyr::case_when(
                    # TOTAL (DENOMINATOR)
                    variable %in% b27010_(c(18,34)) ~ "ins_denom",
                    # NONE = NO INSURANCE
                    variable %in% b27010_(c(33,50)) ~ "ins_none",
                    # MEDICAID ALONE
                    variable %in% 
                      b27010_(c(23,39))     ~ "ins_medicaid_alone",
                    # MEDICAID+MEDICARE
                    variable %in% 
                      b27010_(c(29,46)) ~ "ins_both_medicaid_care",
                    # MEDICARE ALONE
                    variable %in% 
                      b27010_(c(22,38))  ~ "ins_medicare_alone",
                    # PRIVATE = EMPLOYER (ALONE OR COMBINATION), 
                    #           DIRECT PURCHASE (ALONE OR COMBINATION)
                    #           PRIVATE ONLY COMBINATIONS
                    #           "OTHER COVERAGE COMBINATIONS" (PUBLIC + PRIVATE)
                    variable %in% b27010_(c(20:21, 27:28,
                                            30,32,36:37, 43:45, 47,49)) ~ "ins_private",
                    # OTHER (PUBLIC) = MILITARY & OTHER PUBLIC ONLY COMBINATIONS
                    variable %in% b27010_(c(24:25,31,40:41,48)) ~ "ins_other")) %>%
  dplyr::group_by(GEOID, NAME, insurance_type, Year) %>%
  dplyr::summarize(estimate = sum(estimate, na.rm = F)) %>%
  tidyr::pivot_wider(id_cols = c(GEOID, NAME,Year),
                     names_from = insurance_type,
                     values_from = estimate) %>%
  dplyr::mutate(ins_medicaid_2  = ins_medicaid_alone + ins_both_medicaid_care, 
                check_total = ins_denom - 
                  (ins_none + ins_medicaid_alone +  ins_both_medicaid_care +
                     ins_medicare_alone + ins_private + ins_other))

summary(state_insurance_working$check_total)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#       0       0       0       0       0       0      87
# REMOVE THE EXTRA CHECK_TOTAL COLUMN
state_insurance_working<- state_insurance_working%>% dplyr::select(-check_total)

## HERE, COMBINE INSURANCE TYPES INTO APPROPRIATE CATEGORIES
temp_insurance_state_working <- state_insurance_working %>%
  select(GEOID,NAME,Year,
         pop_ins_Medicare = ins_medicare_alone,
         pop_ins_Medicaid = ins_medicaid_2,
         pop_ins_Private  = ins_private,
         pop_ins_None     = ins_none,
         pop_ins_Other    = ins_other) %>%
  pivot_longer(cols = c(pop_ins_Medicaid, pop_ins_Medicare,
                        pop_ins_Private, pop_ins_None,
                        pop_ins_Other),
               names_prefix = "pop_ins_",
               names_to = "insurance_ses",
               values_to = "pop_ins_state")

cleaned_data_4w <- cleaned_data_3w %>%
  left_join(temp_insurance_state_working, by = c("State"="NAME","Year", "Insurance Type"="insurance_ses"))



final_rates_insurance_state_distinct_working <- cleaned_data_4w %>%
  distinct(State, Year, `Insurance Type`, .keep_all = TRUE)

final_rates_insurance_state_distinct_working1<-final_rates_insurance_state_distinct_working%>%
  filter(!`Insurance Type`=="NA")%>%
  group_by(Year, `Insurance Type`)%>%
  mutate(pop_ins_year = sum(pop_ins_state, na.rm=TRUE))%>% ungroup()

pop_ins_summaryw <- cleaned_data_4w %>%
  left_join(final_rates_insurance_state_distinct_working1, by=c("GEOID.x", "County",
                                                        "State","Year",
                                                        "Insurance Type",
                                                        "ACSC Hospitalization",
                                                        "Acute Hospitalization",
                                                        "Chronic Hospitalization",
                                                        "Diabetes Hospitalization",
                                                        "pop_ins", "GEOID.y",
                                                        "pop_ins_state"))%>%
  group_by(Year, `Insurance Type`) %>%
  mutate(pop_ins_year = first(na.omit(pop_ins_year))) %>%  # Fill missing values
  ungroup()

pop_ins_summaryw<-pop_ins_summaryw%>%
  filter(!`Insurance Type`=="NA")%>%
  select(
    -GEOID.y
  )%>%
  rename(
    "GEOID"=GEOID.x
  )
#Rate Calculations
# Step 1: Calculate hospitalizations and insurance populations at the state level
cases_by_insurance_statew <- pop_ins_summaryw %>%
  group_by(State, Year, `Insurance Type`,pop_ins_state) %>% 
  summarise(
    ACSC_Hosp = sum(`ACSC Hospitalization`, na.rm = TRUE),
    Acute_Hosp = sum(`Acute Hospitalization`, na.rm = TRUE),
    Chronic_Hosp = sum(`Chronic Hospitalization`, na.rm = TRUE),
    Diabetes_Hosp = sum(`Diabetes Hospitalization`, na.rm = TRUE),
  ) %>%
  ungroup()
# Step 2: Calculate insurance-specific rates
ins_specific_rates_statew <- cases_by_insurance_statew %>%
  mutate( 
    ins_specific_rate_acsc_s = ACSC_Hosp / pop_ins_state * 1000,
    ins_specific_rate_acute_s = Acute_Hosp / pop_ins_state * 1000,
    ins_specific_rate_chronic_s = Chronic_Hosp / pop_ins_state * 1000,
    ins_specific_rate_diabetes_s= Diabetes_Hosp / pop_ins_state * 1000
  ) %>%
  select(State, Year, `Insurance Type`, ins_specific_rate_acsc_s, ins_specific_rate_acute_s, 
         ins_specific_rate_chronic_s, ins_specific_rate_diabetes_s, pop_ins_state)
# Step 6: Calculate crude rates at the state level 
ins_rates_crude_statew <- cases_by_insurance_statew %>%
  group_by(State, Year) %>%
  summarise(
    Crude_ACSC_Rate_ins_s= sum(ACSC_Hosp, na.rm = TRUE) / sum(pop_ins_state, na.rm = TRUE) * 1000,
    Crude_Acute_Rate_ins_s = sum(Acute_Hosp, na.rm = TRUE) / sum(pop_ins_state, na.rm = TRUE) * 1000,
    Crude_Chronic_Rate_ins_s = sum(Chronic_Hosp, na.rm = TRUE) / sum(pop_ins_state, na.rm = TRUE) * 1000,
    Crude_Diabetes_Rate_ins_s = sum(Diabetes_Hosp, na.rm = TRUE) / sum(pop_ins_state, na.rm = TRUE) * 1000
  ) %>%
  ungroup()

# Step 7: Combine ins-specific rates, crude rates, and adjusted rates
final_rates_state_insw <- ins_rates_crude_statew %>%
  left_join(ins_specific_rates_statew, by = c("State", "Year"))

# Step 1: Calculate hospitalizations and insurance populations at the year level
cases_by_insurance_yearw <- pop_ins_summaryw %>%
  group_by(Year, `Insurance Type`,pop_ins_year) %>% 
  summarise(
    ACSC_Hosp = sum(`ACSC Hospitalization`, na.rm = TRUE),
    Acute_Hosp = sum(`Acute Hospitalization`, na.rm = TRUE),
    Chronic_Hosp = sum(`Chronic Hospitalization`, na.rm = TRUE),
    Diabetes_Hosp = sum(`Diabetes Hospitalization`, na.rm = TRUE),
  ) %>%
  ungroup()
# Step 2: Calculate insurance-specific rates
ins_specific_rates_yearw <- cases_by_insurance_yearw %>%
  mutate( 
    ins_specific_rate_acsc_y = ACSC_Hosp / pop_ins_year * 1000,
    ins_specific_rate_acute_y = Acute_Hosp / pop_ins_year * 1000,
    ins_specific_rate_chronic_y = Chronic_Hosp / pop_ins_year * 1000,
    ins_specific_rate_diabetes_y= Diabetes_Hosp / pop_ins_year * 1000
  ) %>%
  select(Year, `Insurance Type`, ins_specific_rate_acsc_y, ins_specific_rate_acute_y, 
         ins_specific_rate_chronic_y, ins_specific_rate_diabetes_y, pop_ins_year)
# Step 6: Calculate crude rates at the year level 
ins_rates_crude_yearw <- cases_by_insurance_yearw %>%
  group_by(Year) %>%
  summarise(
    Crude_ACSC_Rate_ins_y= sum(ACSC_Hosp, na.rm = TRUE) / sum(pop_ins_year, na.rm = TRUE) * 1000,
    Crude_Acute_Rate_ins_y = sum(Acute_Hosp, na.rm = TRUE) / sum(pop_ins_year, na.rm = TRUE) * 1000,
    Crude_Chronic_Rate_ins_y = sum(Chronic_Hosp, na.rm = TRUE) / sum(pop_ins_year, na.rm = TRUE) * 1000,
    Crude_Diabetes_Rate_ins_y = sum(Diabetes_Hosp, na.rm = TRUE) / sum(pop_ins_year, na.rm = TRUE) * 1000
  ) %>%
  ungroup()

# Step 7: Combine ins-specific rates, crude rates, and adjusted rates
final_rates_year_insw <- ins_rates_crude_yearw %>%
  left_join(ins_specific_rates_yearw, by = c("Year"))


#MERGE DATA
colnames(final_rates_county_insw)
colnames(final_rates_state_insw)
colnames(final_rates_year_insw)

duplicates_county_insw <- final_rates_county_insw %>%
  group_by(State, Year, `Insurance Type`) %>%
  filter(n() > 1)


duplicates_state_insw <- final_rates_state_insw %>%
  group_by(State, Year, `Insurance Type`) %>%
  filter(n() > 1)

final_rates_county_ins_distinctw <- final_rates_county_insw %>%
  distinct(County, State, Year, `Insurance Type`, .keep_all = TRUE)
final_rates_state_ins_distinctw <- final_rates_state_insw %>%
  distinct(State, Year, `Insurance Type`, .keep_all = TRUE)
final_data_ins_ratesw <- final_rates_county_ins_distinctw %>%
  left_join(final_rates_state_ins_distinctw, by = c("State", "Year", "Insurance Type"))

duplicates_year_insw <- final_rates_year_insw %>%
  group_by( Year, `Insurance Type`) %>%
  filter(n() > 1)

final_rates_year_ins_distinctw <- final_rates_year_insw %>%
  distinct(Year, `Insurance Type`, .keep_all = TRUE)

cleaned_data_ins_rates_finw<-final_data_ins_ratesw%>%
  left_join(final_rates_year_ins_distinctw, by=c("Year","Insurance Type"))

save(cleaned_data_ins_rates_finw, file="cleaned_data_ins_finalw.RData")
load("cleaned_data_ins_finalw.RData")

#GRAPHS- years (TABLE 1)
#ACSC
ggplot(cleaned_data_ins_rates_finw %>% 
         filter(!is.na(ins_specific_rate_acsc_s), !is.na(`Insurance Type`)), 
       aes(x = Year, y = ins_specific_rate_acsc_s, color = `Insurance Type`)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ State) +  # Separate plots for each state
  labs(
    title = "Insurance-Specific ACSC Hospitalization Rates from 2016-2020",
    subtitle = "Total Working Population (18-66)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Insurance Category"
  ) +
  theme_minimal()


#ACUTE
ggplot(cleaned_data_ins_rates_finw %>% 
         filter(!is.na(ins_specific_rate_acute_s), !is.na(`Insurance Type`)), 
       aes(x = Year, y = ins_specific_rate_acute_s, color = `Insurance Type`)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ State) +  # Separate plots for each state
  labs(
    title = "Insurance-Specific Acute Hospitalization Rates from 2016-2020",
    subtitle = "Total Working Population (18-66)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Insurance Category"
  ) +
  theme_minimal()


#CHRONIC
ggplot(cleaned_data_ins_rates_finw %>% 
         filter(!is.na(ins_specific_rate_chronic_s), !is.na(`Insurance Type`)), 
       aes(x = Year, y = ins_specific_rate_chronic_s, color =`Insurance Type`)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ State) +  # Separate plots for each state
  labs(
    title = "Insurance-Specific Chronic Hospitalization Rates from 2016-2020",
    subtitle = "Total Working Population (18-66)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Insurance Category"
  ) +
  theme_minimal()


#DIABETES
ggplot(cleaned_data_ins_rates_finw %>% 
         filter(!is.na(ins_specific_rate_diabetes_s), !is.na(`Insurance Type`)), 
       aes(x = Year, y = ins_specific_rate_diabetes_s, color = `Insurance Type`)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ State) +  # Separate plots for each state
  labs(
    title = "Insurance-Specific Diabetes Hospitalization Rates from 2016-2020",
    subtitle = "Total Working Population (18-66)",
    x = "Year",
    y = "Rate per 1,000 Population",
    color = "Insurance Category"
  ) +
  theme_minimal()


#Year total (TABLE 1)
cleaned_data_ins_rates_finw%>% 
  filter(!is.na(ins_specific_rate_acsc_y), !is.na(`Insurance Type`))%>%
  filter(!is.na(ins_specific_rate_acute_y), !is.na(`Insurance Type`))%>%
  filter(!is.na(ins_specific_rate_chronic_y), !is.na(`Insurance Type`))%>%
  filter(!is.na(ins_specific_rate_diabetes_y), !is.na(`Insurance Type`))%>%
  ggplot(aes(x = Year)) +
  geom_line(aes(y=ins_specific_rate_acsc_y,color="ACSC"),linewidth=1) +
  geom_line(aes(y=ins_specific_rate_acute_y,color="Acute"),linewidth=1) +
  geom_line(aes(y=ins_specific_rate_chronic_y,color="Chronic"),linewidth=1) +
  geom_line(aes(y=ins_specific_rate_diabetes_y,color="Diabetes"),linewidth=1) +
  geom_point(aes(y=ins_specific_rate_acsc_y,color="ACSC"),size=1.5) +
  geom_point(aes(y=ins_specific_rate_acute_y,color="Acute"),size=1.5) +
  geom_point(aes(y=ins_specific_rate_chronic_y,color="Chronic"),size=1.5) +
  geom_point(aes(y=ins_specific_rate_diabetes_y,color="Diabetes"),size=1.5) +
  facet_wrap(~`Insurance Type`)+
  labs(
    title = "Hospitalization Specific Rates Over Time",
    subtitle = "Working Population (18-66), Stratified by Insurance Type",
    x = "Year",
    y = "Rate (per 1,000)",
    color = "Rate Type"
  ) + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  
  )

##MERGE DATASETS 
cleaned_data_age_race_sex_ins_mergedw<-cleaned_data_age_race_sex_mergedw%>%
  left_join(cleaned_data_ins_rates_finw, by=c("GEOID","County","State","Year","Insurance Type"))
save(cleaned_data_age_race_sex_ins_mergedw,file="cleaned_data_age_race_sex_ins_mergedw.RData")
load("cleaned_data_age_race_sex_mergedw.RData")
