#Table 1 
install.packages("tableone")
library(tableone)

load("cleaned_merged_final.RData")
data<-cleaned_merged_final%>%
mutate(Region = case_when(
  State %in% c("North Carolina", "Kentucky", "Georgia", "Florida") ~ "Southeast",
  State %in% c("Massachusetts", "New York", "New Jersey") ~ "Northeast",
  State %in% c("Arizona") ~ "Southwest",
  State %in% c("Washington", "Oregon") ~ "West",
  State %in% c("Wisconsin", "Minnesota", "Iowa", "Missouri","Nebraska") ~ "Midwest"))

# Ensure hospitalization variables are correctly formatted as factors with Yes/No labels
data$`ACSC Hospitalization` <- factor(data$`ACSC Hospitalization`, levels = c(0,1), labels = c("No", "Yes"))
data$`Acute Hospitalization` <- factor(data$`Acute Hospitalization`, levels = c(0,1), labels = c("No", "Yes"))
data$`Chronic Hospitalization` <- factor(data$`Chronic Hospitalization`, levels = c(0,1), labels = c("No", "Yes"))
data$`Diabetes Hospitalization` <- factor(data$`Diabetes Hospitalization`, levels = c(0,1), labels = c("No", "Yes"))

# Ensure Year is a factor for stratification
data$Year <- as.factor(data$Year)

# Define variables for Table 1
vars <- c("Age", "Age Cat.", "Sex", "Race", "Insurance Type", "ACSC Hospitalization",
          "Acute Hospitalization", "Chronic Hospitalization", "Diabetes Hospitalization")

# Create Table 1 stratified by Year
table1_year <- CreateTableOne(vars = vars, strata = "Year", data = data, 
                              factorVars = c("Sex", "Race", "Insurance Type", "ACSC Hospitalization",
                                             "Acute Hospitalization", "Chronic Hospitalization", "Diabetes Hospitalization"))

# Print Table 1 stratified by Year
print(table1_year, showAllLevels = TRUE, format = "p")

write.csv(print(table1_year, format = "p",showAllLevels=TRUE,quote=F, NoSpaces=T),"Table1_year.csv")

##WORKING POPULATION

dataw<-cleaned_merged_final%>%
  filter(`Age Cat.`%in% c("18-29","30-44","45-66"))%>%
  mutate(Region = case_when(
    State %in% c("North Carolina", "Kentucky", "Georgia", "Florida") ~ "Southeast",
    State %in% c("Massachusetts", "New York", "New Jersey") ~ "Northeast",
    State %in% c("Arizona") ~ "Southwest",
    State %in% c("Washington", "Oregon") ~ "West",
    State %in% c("Wisconsin", "Minnesota", "Iowa", "Missouri","Nebraska") ~ "Midwest"))

vars<-c("Age","Sex","Race","Insurance Type","Region","Year","ACSC Hospitalization",
        "Acute Hospitalization","Chronic Hospitalization","Diabetes Hospitalization")



# Ensure hospitalization variables are correctly formatted as factors with Yes/No labels
dataw$`ACSC Hospitalization` <- factor(dataw$`ACSC Hospitalization`, levels = c(0,1), labels = c("No", "Yes"))
dataw$`Acute Hospitalization` <- factor(dataw$`Acute Hospitalization`, levels = c(0,1), labels = c("No", "Yes"))
dataw$`Chronic Hospitalization` <- factor(dataw$`Chronic Hospitalization`, levels = c(0,1), labels = c("No", "Yes"))
dataw$`Diabetes Hospitalization` <- factor(dataw$`Diabetes Hospitalization`, levels = c(0,1), labels = c("No", "Yes"))

# Ensure Year is a factor for stratification
dataw$Year <- as.factor(dataw$Year)

# Define variables for Table 1
vars <- c("Age", "Age Cat.", "Sex", "Race", "Insurance Type", "ACSC Hospitalization",
          "Acute Hospitalization", "Chronic Hospitalization", "Diabetes Hospitalization")

# Create Table 1 stratified by Year
table1_yearw <- CreateTableOne(vars = vars, strata = "Year", data = dataw, 
                              factorVars = c("Sex", "Race", "Insurance Type", "ACSC Hospitalization",
                                             "Acute Hospitalization", "Chronic Hospitalization", "Diabetes Hospitalization"))

# Print Table 1 stratified by Year
print(table1_yearw, showAllLevels = TRUE, format = "p")

write.csv(print(table1_yearw, format = "p",showAllLevels=TRUE,quote=F, NoSpaces=T),"Table1_year_working.csv")
