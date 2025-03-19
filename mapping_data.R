library(sf)
library(tidycensus)
library(tidyverse)
library(tigris)
options(tigris_use_cache=TRUE)
library(arrow)

#Load Rates
load("cleaned_data_age_race_sex_ins_merged.RData")

##2016
# Load county shapefiles for 2016
counties_sf <- counties(year = 2016, cb = TRUE)

# Ensure GEOID is character in both datasets for a proper join
county_rates_data <- cleaned_data_age_race_sex_ins_merged %>%
  mutate(GEOID = as.character(GEOID)) %>%
  select(GEOID, County, State, Year, Age_Adjusted_ACSC_Rate_c,
         Age_Adjusted_Acute_Rate_c, Age_Adjusted_Chronic_Rate_c, 
         Age_Adjusted_Diabetes_Rate_c)
county_rates<-county_rates_data%>%
  distinct(County, State, Year, .keep_all = TRUE)
county_rate16<-county_rates%>%
  filter(Year == 2016)
# Convert GEOID in spatial data to character for joining
counties_sf <- counties_sf %>%
  mutate(GEOID = as.character(GEOID))

# Join datasets
data_16 <- counties_sf %>%
  left_join(county_rate16, by = c("GEOID", "NAME" = "County"))
data_16 <- data_16 %>% 
  filter(!STATEFP %in% c("60","78",
                       "72","66","15","02", "74","69")) ##REMOVING TO JUST HAVE CONTINENTAL US
# Plot map
ggplot(data_16) +
  geom_sf(aes(fill = Age_Adjusted_ACSC_Rate_c), color = "black", size = 0.1) +
  scale_fill_viridis_c(
    name = "Adjusted Rate (per 1,000)",  
    breaks = seq(0, 90, 20),  
    labels = seq(0, 90, 20),  
    limits = c(0, 90),  
    na.value = "white"
  ) + 
  labs(
    title="County-Level Age-Adjusted ACSC Rates",
    subtitle= "2016 Total Population (18-66)",
    caption="Data Source: Rates Standardized to the 2000 US Census"
  )+
  theme_minimal() +
  theme(legend.position = "right",
        plot.caption = element_text(hjust = 0.5, face = "italic"))

##2017
# Load county shapefiles for 2017
counties_sf17 <- counties(year = 2017, cb = TRUE)

# Ensure GEOID is character in both datasets for a proper join
county_rate17<-county_rates%>%
  filter(Year == 2017)
# Convert GEOID in spatial data to character for joining
counties_sf17 <- counties_sf17 %>%
  mutate(GEOID = as.character(GEOID))

# Join datasets
data_17 <- counties_sf17 %>%
  left_join(county_rate17, by = c("GEOID", "NAME" = "County"))
data_17 <- data_17 %>% 
  filter(!STATEFP %in% c("60","78",
                         "72","66","15","02", "74","69")) ##REMOVING TO JUST HAVE CONTINENTAL US
# Plot map
ggplot(data_17) +
  geom_sf(aes(fill = Age_Adjusted_ACSC_Rate_c), color = "black", size = 0.1) +
  scale_fill_viridis_c(
    name = "Adjusted Rate (per 1,000)",
    breaks = seq(0, 110, 20),  
    labels = seq(0, 110, 20),  
    limits = c(0, 110), 
    na.value = "white"
  ) + 
  labs(
    title="County-Level Age-Adjusted ACSC Rates",
    subtitle= "2017 Total Population (18-66)",
    caption="Data Source: Rates Standardized to the 2000 US Census"
  )+
  theme_minimal() +
  theme(legend.position = "right",
        plot.caption = element_text(hjust = 0.5, face = "italic"))


##2018
# Load county shapefiles for 2018
counties_sf18 <- counties(year = 2018, cb = TRUE)

# Ensure GEOID is character in both datasets for a proper join
county_rate18<-county_rates%>%
  filter(Year == 2018)
# Convert GEOID in spatial data to character for joining
counties_sf18 <- counties_sf18 %>%
  mutate(GEOID = as.character(GEOID))

# Join datasets
data_18 <- counties_sf18 %>%
  left_join(county_rate18, by = c("GEOID", "NAME" = "County"))
data_18 <- data_18 %>% 
  filter(!STATEFP %in% c("60","78",
                         "72","66","15","02", "74","69")) ##REMOVING TO JUST HAVE CONTINENTAL US
# Plot map
ggplot(data_18) +
  geom_sf(aes(fill = Age_Adjusted_ACSC_Rate_c), color = "black", size = 0.1) +
  scale_fill_viridis_c(
    name = "Adjusted Rate (per 1,000)",  
    breaks = seq(0, 100, 20),  
    labels = seq(0, 100, 20),  
    limits = c(0, 100),  
    na.value = "white"
  ) + 
  labs(
    title="County-Level Age-Adjusted ACSC Rates",
    subtitle= "2018 Total Population (18-66)",
    caption="Data Source: Rates Standardized to the 2000 US Census"
  )+
  theme_minimal() +
  theme(legend.position = "right",
        plot.caption = element_text(hjust = 0.5, face = "italic"))


##2019
# Load county shapefiles for 2019
counties_sf19 <- counties(year = 2019, cb = TRUE)

# Ensure GEOID is character in both datasets for a proper join
county_rate19<-county_rates%>%
  filter(Year == 2019)
# Convert GEOID in spatial data to character for joining
counties_sf19 <- counties_sf19 %>%
  mutate(GEOID = as.character(GEOID))

# Join datasets
data_19 <- counties_sf19 %>%
  left_join(county_rate19, by = c("GEOID", "NAME" = "County"))
data_19 <- data_19 %>% 
  filter(!STATEFP %in% c("60","78",
                         "72","66","15","02", "74","69")) ##REMOVING TO JUST HAVE CONTINENTAL US
# Plot map
ggplot(data_19) +
  geom_sf(aes(fill = Age_Adjusted_ACSC_Rate_c), color = "black", size = 0.1) +
  scale_fill_viridis_c(
    name = "Adjusted Rate (per 1,000)",  
    breaks = seq(0, 270, 50),  
    labels = seq(0, 270, 50),  
    limits = c(0, 270),  
    na.value = "white"
  ) + 
  labs(
    title="County-Level Age-Adjusted ACSC Rates",
    subtitle= "2019 Total Population (18-66)",
    caption="Data Source: Rates Standardized to the 2000 US Census"
  )+
  theme_minimal() +
  theme(legend.position = "right",
        plot.caption = element_text(hjust = 0.5, face = "italic"))


##2020
# Load county shapefiles for 2020
counties_sf20 <- counties(year = 2020, cb = TRUE)

# Ensure GEOID is character in both datasets for a proper join
county_rate20<-county_rates%>%
  filter(Year == 2020)
# Convert GEOID in spatial data to character for joining
counties_sf20 <- counties_sf20 %>%
  mutate(GEOID = as.character(GEOID))

# Join datasets
data_20<- counties_sf20 %>%
  left_join(county_rate20, by = c("GEOID", "NAME" = "County"))
data_20 <- data_20 %>% 
  filter(!STATEFP %in% c("60","78",
                         "72","66","15","02", "74","69")) ##REMOVING TO JUST HAVE CONTINENTAL US
# Plot map
ggplot(data_20) +
  geom_sf(aes(fill = Age_Adjusted_ACSC_Rate_c), color = "black", size = 0.1) +
  scale_fill_viridis_c(
    name = "Adjusted Rate (per 1,000)",  
    limits = c(0, 75),  
    na.value = "white"
  ) + 
  labs(
    title="County-Level Age-Adjusted ACSC Rates",
    subtitle= "2020 Total Population (18-66)",
    caption="Data Source: Rates Standardized to the 2000 US Census"
  )+
  theme_minimal() +
  theme(legend.position = "right",
        plot.caption = element_text(hjust = 0.5, face = "italic"))
