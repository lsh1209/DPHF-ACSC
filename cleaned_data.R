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

###TABLE 1 INFORMATION (FINISH for all years and hospitalizations) 
acsc_2016<- cleaned_data_filtered1%>%
  filter(Year==2016, `ACSC Hospitalization`==1)%>%
  summarize(Total_Persons= n(),
            Mean_age = mean(Age, na.rm = TRUE),
            SD_Age = sd(Age, na.rm = TRUE))
view(acsc_2016)
acsc_2017<- cleaned_data_filtered1%>%
  filter(Year==2017, `ACSC Hospitalization`==1)%>%
  summarize(Total_Persons= n(),
            Mean_age = mean(Age, na.rm = TRUE),
            SD_Age = sd(Age, na.rm = TRUE))
view(acsc_2017)
acsc_2018<- cleaned_data_filtered1%>%
  filter(Year==2018, `ACSC Hospitalization`==1)%>%
  summarize(Total_Persons= n(),
            Mean_age = mean(Age, na.rm = TRUE),
            SD_Age = sd(Age, na.rm = TRUE))
view(acsc_2018)
acsc_2019<- cleaned_data_filtered1%>%
  filter(Year==2019, `ACSC Hospitalization`==1)%>%
  summarize(Total_Persons= n(),
            Mean_age = mean(Age, na.rm = TRUE),
            SD_Age = sd(Age, na.rm = TRUE))
view(acsc_2019)
acsc_2020<- cleaned_data_filtered1%>%
  filter(Year==2020, `ACSC Hospitalization`==1)%>%
  summarize(Total_Persons= n(),
            Mean_age = mean(Age, na.rm = TRUE),
            SD_Age = sd(Age, na.rm = TRUE))
view(acsc_2020)

###ACUTE
acute_2016<- cleaned_data_filtered1%>%
  filter(Year==2016, `Acute Hospitalization`==1)%>%
  summarize(Total_Persons= n(),
            Mean_age = mean(Age, na.rm = TRUE),
            SD_Age = sd(Age, na.rm = TRUE))
view(acute_2016)
acute_2017<- cleaned_data_filtered1%>%
  filter(Year==2017, `Acute Hospitalization`==1)%>%
  summarize(Total_Persons= n(),
            Mean_age = mean(Age, na.rm = TRUE),
            SD_Age = sd(Age, na.rm = TRUE))
view(acute_2017)
acute_2018<- cleaned_data_filtered1%>%
  filter(Year==2018, `Acute Hospitalization`==1)%>%
  summarize(Total_Persons= n(),
            Mean_age = mean(Age, na.rm = TRUE),
            SD_Age = sd(Age, na.rm = TRUE))
view(acute_2018)
acute_2019<- cleaned_data_filtered1%>%
  filter(Year==2019, `Acute Hospitalization`==1)%>%
  summarize(Total_Persons= n(),
            Mean_age = mean(Age, na.rm = TRUE),
            SD_Age = sd(Age, na.rm = TRUE))
view(acute_2019)
acute_2020<- cleaned_data_filtered1%>%
  filter(Year==2020, `Acute Hospitalization`==1)%>%
  summarize(Total_Persons= n(),
            Mean_age = mean(Age, na.rm = TRUE),
            SD_Age = sd(Age, na.rm = TRUE))
view(acute_2020)

###CHRONIC
chronic_2016<- cleaned_data_filtered1%>%
  filter(Year==2016, `Chronic Hospitalization`==1)%>%
  summarize(Total_Persons= n(),
            Mean_age = mean(Age, na.rm = TRUE),
            SD_Age = sd(Age, na.rm = TRUE))
view(chronic_2016)
chronic_2017<- cleaned_data_filtered1%>%
  filter(Year==2017, `Chronic Hospitalization`==1)%>%
  summarize(Total_Persons= n(),
            Mean_age = mean(Age, na.rm = TRUE),
            SD_Age = sd(Age, na.rm = TRUE))
view(chronic_2017)
chronic_2018<- cleaned_data_filtered1%>%
  filter(Year==2018, `Chronic Hospitalization`==1)%>%
  summarize(Total_Persons= n(),
            Mean_age = mean(Age, na.rm = TRUE),
            SD_Age = sd(Age, na.rm = TRUE))
view(chronic_2018)
chronic_2019<- cleaned_data_filtered1%>%
  filter(Year==2019, `Chronic Hospitalization`==1)%>%
  summarize(Total_Persons= n(),
            Mean_age = mean(Age, na.rm = TRUE),
            SD_Age = sd(Age, na.rm = TRUE))
view(chronic_2019)
chronic_2020<- cleaned_data_filtered1%>%
  filter(Year==2020, `Chronic Hospitalization`==1)%>%
  summarize(Total_Persons= n(),
            Mean_age = mean(Age, na.rm = TRUE),
            SD_Age = sd(Age, na.rm = TRUE))
view(chronic_2020)

###DIABETES
diabetes_2016<- cleaned_data_filtered1%>%
  filter(Year==2016, `Diabetes Hospitalization`==1)%>%
  summarize(Total_Persons= n(),
            Mean_age = mean(Age, na.rm = TRUE),
            SD_Age = sd(Age, na.rm = TRUE))
view(diabetes_2016)
diabetes_2017<- cleaned_data_filtered1%>%
  filter(Year==2017, `Diabetes Hospitalization`==1)%>%
  summarize(Total_Persons= n(),
            Mean_age = mean(Age, na.rm = TRUE),
            SD_Age = sd(Age, na.rm = TRUE))
view(diabetes_2017)
diabetes_2018<- cleaned_data_filtered1%>%
  filter(Year==2018, `Diabetes Hospitalization`==1)%>%
  summarize(Total_Persons= n(),
            Mean_age = mean(Age, na.rm = TRUE),
            SD_Age = sd(Age, na.rm = TRUE))
view(diabetes_2018)
diabetes_2019<- cleaned_data_filtered1%>%
  filter(Year==2019, `Diabetes Hospitalization`==1)%>%
  summarize(Total_Persons= n(),
            Mean_age = mean(Age, na.rm = TRUE),
            SD_Age = sd(Age, na.rm = TRUE))
view(diabetes_2019)
diabetes_2020<- cleaned_data_filtered1%>%
  filter(Year==2020, `Diabetes Hospitalization`==1)%>%
  summarize(Total_Persons= n(),
            Mean_age = mean(Age, na.rm = TRUE),
            SD_Age = sd(Age, na.rm = TRUE))
view(diabetes_2020)


#MERGE 
load("pcp_combined.RData")
pcp_combined <- pcp_combined %>%
  mutate(Year = as.numeric(Year))
cleaned_data_all<-cleaned_data_rates_ars%>%
  left_join(pcp_combined, by=c("GEOID","County","State","Year"))
save(cleaned_data_all, file="cleaned_data_all.RData")