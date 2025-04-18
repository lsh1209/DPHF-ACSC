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
#CODE FROM HEATHER:
# FOLDER PATH FOR LEXI:
hcup_project_path <- paste0("//files.drexel.edu/encrypted/SOPH/UHC/",
                            "HCUP_read_only_access/Lexi_Haws_hcup/")

## DEFINE THE DATA BASE
db_pqi <- 
  arrow::open_dataset(paste0(hcup_project_path, 
                             "discharges_pqi.parquet"))
# VIEW COLUMN NAMES 
db_names <- db_pqi$schema$names
print(db_names)
##CLEANED DATA & ADD AGE CATEGORIES
cleaned_query <- db_pqi %>%
  mutate(miss_county = (is.na(patient_county)| patient_county %in% "NA")) %>%
  mutate(popagecat = 
           case_when( AGE %in% 18:29 ~ "18-29",
                      AGE %in% 30:44 ~ "30-44",
                      AGE %in% 45:66 ~ "45-66",
                      AGE %in% 67:74 ~ "67-74",
                      AGE >=   75 ~ "75+"))%>%
  filter(!(miss_county == T | (miss_county == F  & hospst_match_county == F)))%>%
  group_by(HOSPST, YEAR, AYEAR, AMONTH, DQTR, patient_county,
           miss_county,hospst_match_county,
           AGE,popagecat, sexcat,race_label,pay_label, tapq01, tapq03,
           tapq05, tapq07, tapq08, tapq11, tapq12,
           tapq14, tapq15, tapq16, tapq90, tapq91, tapq92, tapq93)%>% 
  count()
cleaned_data = cleaned_query %>%
  collect()

####FURTHER CLEAN
cleaned_data= cleaned_data%>%
  mutate(Year1 = ifelse(is.na(AYEAR), YEAR,AYEAR))%>%
  group_by(HOSPST, Year1, YEAR, AYEAR, AMONTH, DQTR, patient_county,
           miss_county,hospst_match_county,
           AGE,popagecat, sexcat,race_label,pay_label, tapq01, tapq03,
           tapq05, tapq07, tapq08, tapq11, tapq12,
           tapq14, tapq15, tapq16, tapq90, tapq91, tapq92, tapq93)%>% 
  count()
cleaned_data=cleaned_data%>%
  collect()
save(cleaned_data, file="cleaned_data_main.RData")
#####YEARS and OUTCOMES
cleaned_data_filtered <- cleaned_data %>%
  ungroup() %>% 
  filter(Year1 %in% 2016:2021) %>%
  filter(tapq90 == 1 | tapq91 == 1 | tapq92 == 1 | tapq93 == 1) %>%
  select(patient_county, HOSPST, Year1, AGE, popagecat, sexcat, race_label, 
         pay_label, tapq90, tapq91, tapq92, tapq93)
cleaned_data_filtered=cleaned_data_filtered%>%
  mutate(
    Age= as.numeric(AGE))

cleaned_data_filtered1 = cleaned_data_filtered%>%
  rename(
    "State"= HOSPST,
    "Year" = Year1,
    "FIPS"= patient_county,
    "Age"= Age,
    "Age Cat."= popagecat,
    "Sex"=sexcat,
    "Race"=race_label,
    "Insurance Type" = pay_label,
    "ACSC Hospitalization"= tapq90,
    "Acute Hospitalization"=tapq91,
    "Chronic Hospitalization"=tapq92,
    "Diabetes Hospitalization"=tapq93)%>%
  select(
    - AGE)
save(cleaned_data_filtered1, file="cleaned_data_1.RData")
