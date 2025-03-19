#MERGE PCP DATA
library(readxl)
library(arrow)
library(dplyr)
library(writexl)
library(openxlsx)

#2016
pcp_2016 <- read.csv("analytic_data2016.csv", stringsAsFactors = FALSE)

# Define the states of interest
states <- c("AZ", "FL", "GA", "KY", "MA", "MN", "NE", "NC", "NJ", "NY", "OR", "WA", "WI")

# Rename columns to avoid space issues (Optional)
colnames(pcp_2016) <- make.names(colnames(pcp_2016))

# Select the relevant columns
pcp_2016 <- pcp_2016 %>%
  filter(`State.Abbreviation` %in% states) %>%  # Use backticks for columns with spaces
  select(
    `X5.digit.FIPS.Code`, `State.Abbreviation`, `Name`, `Release.Year`, 
    `Primary.care.physicians.raw.value`, `Primary.care.physicians.numerator`, 
    `Primary.care.physicians.denominator`,`Ratio.of.population.to.primary.care.physicians`
  )
pcp_2016<- pcp_2016 %>%
  filter(!Name %in% c("Arizona","Florida","Georgia","Kentucky","Massachusetts",
                  "Minnesota","Nebraska","North Carolina", "New Jersey", "New York",
                  "Oregon","Washington","Wisconsin"))%>%
  mutate(County= str_remove(Name, "(County|Co|Coun)$"))%>%
  select(
    X5.digit.FIPS.Code,
    County,
    State.Abbreviation,
    Release.Year,
    Primary.care.physicians.raw.value,
    Primary.care.physicians.numerator,
    Primary.care.physicians.denominator,
    Ratio.of.population.to.primary.care.physicians
  )
pcp_2016=pcp_2016%>%
  rename(
    "GEOID"=X5.digit.FIPS.Code,
    "State"= State.Abbreviation,
    "Year"=Release.Year
  )
#2017
pcp_2017 <- read.csv("analytic_data2017.csv", stringsAsFactors = FALSE)

# Select the relevant columns
pcp_2017 <- pcp_2017 %>%
  filter(`State.Abbreviation` %in% states) %>%  # Use backticks for columns with spaces
  select(
    `X5.digit.FIPS.Code`, `State.Abbreviation`, `Name`, `Release.Year`, 
    `Primary.care.physicians.raw.value`, `Primary.care.physicians.numerator`, 
    `Primary.care.physicians.denominator`,`Ratio.of.population.to.primary.care.physicians`
  )
pcp_2017<- pcp_2017 %>%
  filter(!Name %in% c("Arizona","Florida","Georgia","Kentucky","Massachusetts",
                      "Minnesota","Nebraska","North Carolina", "New Jersey", "New York",
                      "Oregon","Washington","Wisconsin"))%>%
  mutate(County= str_remove(Name, "(County|Co|Coun)$"))%>%
  select(
    X5.digit.FIPS.Code,
    County,
    State.Abbreviation,
    Release.Year,
    Primary.care.physicians.raw.value,
    Primary.care.physicians.numerator,
    Primary.care.physicians.denominator,
    Ratio.of.population.to.primary.care.physicians
  )
pcp_2017=pcp_2017%>%
  rename(
    "GEOID"=X5.digit.FIPS.Code,
    "State"= State.Abbreviation,
    "Year"=Release.Year
  )
#2018
pcp_2018 <- read.csv("analytic_data2018_0.csv", stringsAsFactors = FALSE)

# Select the relevant columns
pcp_2018 <- pcp_2018 %>%
  filter(`State.Abbreviation` %in% states) %>%  # Use backticks for columns with spaces
  select(
    `X5.digit.FIPS.Code`, `State.Abbreviation`, `Name`, `Release.Year`, 
    `Primary.care.physicians.raw.value`, `Primary.care.physicians.numerator`, 
    `Primary.care.physicians.denominator`,`Ratio.of.population.to.primary.care.physicians`
  )
pcp_2018<- pcp_2018 %>%
  filter(!Name %in% c("Arizona","Florida","Georgia","Kentucky","Massachusetts",
                      "Minnesota","Nebraska","North Carolina", "New Jersey", "New York",
                      "Oregon","Washington","Wisconsin"))%>%
  mutate(County= str_remove(Name, "(County|Co|Coun)$"))%>%
  select(
    X5.digit.FIPS.Code,
    County,
    State.Abbreviation,
    Release.Year,
    Primary.care.physicians.raw.value,
    Primary.care.physicians.numerator,
    Primary.care.physicians.denominator,
    Ratio.of.population.to.primary.care.physicians
  )
pcp_2018=pcp_2018%>%
  rename(
    "GEOID"=X5.digit.FIPS.Code,
    "State"= State.Abbreviation,
    "Year"=Release.Year
  )
#2019
pcp_2019 <- read.csv("analytic_data2019.csv", stringsAsFactors = FALSE)

# Select the relevant columns
pcp_2019 <- pcp_2019 %>%
  filter(`State.Abbreviation` %in% states) %>%  # Use backticks for columns with spaces
  select(
    `X5.digit.FIPS.Code`, `State.Abbreviation`, `Name`, `Release.Year`, 
    `Primary.care.physicians.raw.value`, `Primary.care.physicians.numerator`, 
    `Primary.care.physicians.denominator`,`Ratio.of.population.to.primary.care.physicians.`
  )
pcp_2019<- pcp_2019 %>%
  filter(!Name %in% c("Arizona","Florida","Georgia","Kentucky","Massachusetts",
                      "Minnesota","Nebraska","North Carolina", "New Jersey", "New York",
                      "Oregon","Washington","Wisconsin"))%>%
  mutate(County= str_remove(Name, "(County|Co|Coun)$"))%>%
  select(
    X5.digit.FIPS.Code,
    County,
    State.Abbreviation,
    Release.Year,
    Primary.care.physicians.raw.value,
    Primary.care.physicians.numerator,
    Primary.care.physicians.denominator,
    Ratio.of.population.to.primary.care.physicians.
  )
pcp_2019=pcp_2019%>%
  rename(
    "GEOID"=X5.digit.FIPS.Code,
    "State"= State.Abbreviation,
    "Year"=Release.Year,
    "Ratio.of.population.to.primary.care.physicians"=Ratio.of.population.to.primary.care.physicians.
  )

#2020
pcp_2020 <- read.csv("analytic_data2020_0.csv", stringsAsFactors = FALSE)

# Select the relevant columns
pcp_2020 <- pcp_2020 %>%
  filter(`State.Abbreviation` %in% states) %>%  # Use backticks for columns with spaces
  select(
    `X5.digit.FIPS.Code`, `State.Abbreviation`, `Name`, `Release.Year`, 
    `Primary.care.physicians.raw.value`, `Primary.care.physicians.numerator`, 
    `Primary.care.physicians.denominator`,`Ratio.of.population.to.primary.care.physicians.`
  )
pcp_2020<- pcp_2020 %>%
  filter(!Name %in% c("Arizona","Florida","Georgia","Kentucky","Massachusetts",
                      "Minnesota","Nebraska","North Carolina", "New Jersey", "New York",
                      "Oregon","Washington","Wisconsin"))%>%
  mutate(County= str_remove(Name, "(County|Co|Coun)$"))%>%
  select(
    X5.digit.FIPS.Code,
    County,
    State.Abbreviation,
    Release.Year,
    Primary.care.physicians.raw.value,
    Primary.care.physicians.numerator,
    Primary.care.physicians.denominator,
    Ratio.of.population.to.primary.care.physicians.
  )
pcp_2020=pcp_2020%>%
  rename(
    "GEOID"=X5.digit.FIPS.Code,
    "State"= State.Abbreviation,
    "Year"=Release.Year,
    "Ratio.of.population.to.primary.care.physicians"=Ratio.of.population.to.primary.care.physicians.
  )

pcp_combined<-bind_rows(pcp_2016,pcp_2017,pcp_2018,pcp_2019,pcp_2020)

save(pcp_combined, file="pcp_combined.RData")
load("pcp_combined.RData")
pcp_combined <- pcp_combined %>%
  mutate(Year = as.numeric(Year))%>% 
  mutate(Ratio.of.population.to.primary.care.physicians=as.numeric(Ratio.of.population.to.primary.care.physicians),
         Primary.care.physicians.raw.value=as.numeric(Primary.care.physicians.raw.value),
         Primary.care.physicians.numerator=as.numeric(Primary.care.physicians.numerator),
         Primary.care.physicians.denominator=as.numeric(Primary.care.physicians.denominator))%>%
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
  pcp_combined <- pcp_combined %>%
  mutate(GEOID = str_pad(GEOID, width = 5, side = "left", pad = "0"))

#MERGING 
table(trimws(cleaned_data_age_race_sex_ins_reg_merged$County) != cleaned_data_age_race_sex_ins_reg_merged$County)
table(trimws(pcp_combined$County) != pcp_combined$County)
cleaned_data_age_race_sex_ins_reg_merged$County <- trimws(cleaned_data_age_race_sex_ins_reg_merged$County)
pcp_combined$County <- trimws(pcp_combined$County)

cleaned_data_all_final <- cleaned_data_age_race_sex_ins_reg_merged %>%
  left_join(pcp_combined, by = c("GEOID" = "GEOID", "County" = "County", "State" = "State", "Year" = "Year"))

save(cleaned_data_all_final, file="cleaned_data_all_final.RData")

##Working POPULATION 
cleaned_data_age_race_sex_ins_reg_mergedw$County <- trimws(cleaned_data_age_race_sex_ins_reg_mergedw$County)
pcp_combined$County <- trimws(pcp_combined$County)
cleaned_data_working_final <- cleaned_data_age_race_sex_ins_reg_mergedw %>%
  left_join(pcp_combined, by = c("GEOID" = "GEOID", "County" = "County", "State" = "State", "Year" = "Year"))

save(cleaned_data_working_final, file="cleaned_data_working_final.RData")
