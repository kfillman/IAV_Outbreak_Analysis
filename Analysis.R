# New Test

# Imports ----
library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(sf)
library(cluster)
library(factoextra)


banding <- read.csv("Outbreak_data/Banding_recov_.csv")
FIPS_codes <- read.csv("Outbreak_data/FIPS_Region_Codes.csv")
world_outbreak_data <- read_excel("Outbreak_data/Outbreak_H5_H5N1_2020_Sept2023_World.xlsx", sheet = "outbreaks")
shapefile <- read_sf("C:/Users/Kath/Documents/IAV_Outbreak_Analysis/Outbreak_data/admin_unit/Admin_unit.shp")


# Data Cleaning ----
world_outbreak_data$Outbreak_start_date <- as_date(world_outbreak_data$Outbreak_start_date)
world_outbreak_data$Outbreak_end_date <- as_date(world_outbreak_data$Outbreak_end_date)
world_outbreak_data$outbreak_dates <- interval(world_outbreak_data$Outbreak_start_date, world_outbreak_data$Outbreak_end_date)

banding <- banding %>% select(-Group_spec, -Total) %>% distinct()

shapefile <- shapefile %>% distinct(FIPS_ADMIN, .keep_all = TRUE) %>% select(FIPS_ADMIN, GMI_CNTRY, CNTRY_NAME, ADMIN_NAME)

# First US Cases ----
First_US_Cases <- world_outbreak_data %>%
  filter(iso_code=='USA') %>%
  summarise(date=min(Outbreak_start_date), .by = level1_name) %>%
  arrange(date)


# US Connections ----
US_connections <- banding %>%
  filter(ï..B_COUNTRY_=='US' | E_COUNTRY_=='US') %>%
  filter(!(FIPS_ADMIN_banding==FIPS_ADMIN_Recov)) %>% 
  left_join(shapefile, by=c('FIPS_ADMIN_banding'='FIPS_ADMIN'), relationship = 'many-to-one') %>%
  left_join(shapefile, by=c('FIPS_ADMIN_Recov'='FIPS_ADMIN'), relationship = 'many-to-one')

# Find Potential Sources (singular event) ----
# date is yyyy-dd-mm
event_source <- function(state_name, date) {
  date <- as.Date(date)
  infectious_date <- date-3
  state_connections <- US_connections %>% filter(ADMIN_NAME.x==state_name | ADMIN_NAME.y==state_name)
  candidate_sources <- world_outbreak_data %>%
    filter(((date-31) <= Outbreak_start_date) & (Outbreak_start_date < (date-3))) %>%
    filter(iso_code %in% state_connections$GMI_CNTRY.x | iso_code %in% state_connections$GMI_CNTRY.y)
  View(candidate_sources)
}

# List of OG Potential Sources ----
OG_candidate_sources <- data.frame(matrix(ncol = 12, nrow = 0))
OG_sources <- function(state_name) {
  date <- as_date(as.numeric(First_US_Cases[First_US_Cases$level1_name==state_name, "date"]))
  state_connections <- US_connections %>% filter(ADMIN_NAME.x==state_name | ADMIN_NAME.y==state_name)
  state_sources <- world_outbreak_data %>%
    filter(((date-31) <= Outbreak_start_date) & (Outbreak_start_date < (date-3))) %>%
    filter(iso_code %in% state_connections$GMI_CNTRY.x | iso_code %in% state_connections$GMI_CNTRY.y) %>%
    mutate(state=state_name, first_infection=date) %>%
    select(state, first_infection, iso_code, country, region, Outbreak_id, Latitude, Longitude, Outbreak_start_date, level3_name, level2_name, level1_name)
  OG_candidate_sources <<- rbind(OG_candidate_sources, state_sources)
}
OG_sources('South Carolina')
for (level1_name in First_US_Cases$level1_name) {
  OG_sources(level1_name)
}

# Analysis ----
## Banding ----
# Countries total connections
US_connections %>% count(GMI_CNTRY.y) %>% arrange(desc(n)) %>% head()

## OG Potential Source Connections ----
# number of potential sources (including and excluding US)
OG_candidate_sources %>% distinct(Outbreak_id, .keep_all = TRUE)%>% nrow()
OG_candidate_sources %>% filter(iso_code!='USA') %>% distinct(Outbreak_id, .keep_all = TRUE) %>% nrow()

#potential source countries & their number of distinct events
OG_candidate_sources %>% filter(iso_code!='USA') %>% distinct(Outbreak_id, .keep_all = TRUE) %>% count(country) %>% arrange(desc(n))
OG_candidate_sources %>% filter(iso_code!='USA') %>% count(country) %>% arrange(desc(n))

#Average time between first infection and potential source
OG_candidate_sources %>% filter(iso_code!='USA') %>% summarise(time_to_infection=mean(first_infection-Outbreak_start_date)) %>% arrange(time_to_infection)
## Japan
OG_candidate_sources %>% filter(iso_code=='JPN') %>% filter(level1_name=='Iwate') %>% filter(level2_name=='Kuji') %>% summarise(time_to_infection=mean(first_infection-Outbreak_start_date)) %>% arrange(time_to_infection)
## France
OG_candidate_sources %>% filter(level1_name=='Pays de la Loire') %>% filter(level2_name=='Maine-et-Loire') %>% summarise(time_to_infection=mean(first_infection-Outbreak_start_date)) %>% arrange(time_to_infection)
## Canada
OG_candidate_sources %>% filter(level1_name=='Manitoba') %>% filter(level2_name=='Division No. 2') %>% summarise(time_to_infection=mean(first_infection-Outbreak_start_date)) %>% arrange(time_to_infection)
OG_candidate_sources %>% filter(level1_name=='Ontario') %>% filter(level2_name=='York') %>% summarise(time_to_infection=mean(first_infection-Outbreak_start_date)) %>% arrange(time_to_infection)
OG_candidate_sources %>% filter(level1_name=='Alberta') %>% filter(level2_name=='Division No. 11') %>% summarise(time_to_infection=mean(first_infection-Outbreak_start_date)) %>% arrange(time_to_infection)
OG_candidate_sources %>% filter(level1_name=='Nova Scotia') %>% filter(level2_name=='Halifax') %>% summarise(time_to_infection=mean(first_infection-Outbreak_start_date)) %>% arrange(time_to_infection)
## Russia
OG_candidate_sources %>% filter(level1_name=="Stavropol'",) %>% filter(level2_name=="Izobil'nenskiy rayon") %>% summarise(time_to_infection=mean(first_infection-Outbreak_start_date)) %>% arrange(time_to_infection)
## Mexico
OG_candidate_sources %>% filter(level1_name=="Yucatán",) %>% filter(level2_name=="Umán") %>% summarise(time_to_infection=mean(first_infection-Outbreak_start_date)) %>% arrange(time_to_infection)
## Netherlands
OG_candidate_sources %>% filter(level1_name=="Flevoland",) %>% filter(level2_name=="Almere") %>% summarise(time_to_infection=mean(first_infection-Outbreak_start_date)) %>% arrange(time_to_infection)
## UK
OG_candidate_sources %>% filter(iso_code=='JPN') %>% filter(level1_name=='Iwate') %>% filter(level2_name=='Kuji') %>% summarise(time_to_infection=mean(first_infection-Outbreak_start_date)) %>% arrange(time_to_infection)










# Clustering ----
condensed_candidates <- OG_candidate_sources %>%
  filter(iso_code!='USA') %>%
  distinct(Outbreak_id, .keep_all = TRUE) %>%
  add_count(level1_name) %>%
  group_by(country, level1_name) %>%
  summarise(lat=mean(as.numeric(Latitude)), long=mean(as.numeric(Longitude)), n=mean(n))
colnames(condensed_candidates) <- c('country','level1_name', 'Latitude', 'Longitude', 'n')

elbow <- fviz_nbclust(condensed_candidates[,-3:-1], kmeans, method = "wss")
elbow
#calculate gap statistic based on number of clusters
gap_stat <- clusGap(condensed_candidates[,-3:-1],
                    FUN = kmeans,
                    nstart = 25,
                    K.max = 20,
                    B = 50)

# plot number of clusters vs. gap statistic
fviz_gap_stat(gap_stat)

head(condensed_candidates)

## Choosing monitoring locations ----
## France
OG_candidate_sources %>% filter(iso_code=='FRA') %>% distinct(Outbreak_id, .keep_all = TRUE) %>% group_by(level1_name, level2_name) %>% count() %>% arrange(desc(n)) %>% head()
## Japan
OG_candidate_sources %>% filter(iso_code=='JPN') %>% distinct(Outbreak_id, .keep_all = TRUE) %>% group_by(level1_name, level2_name) %>% count() %>% arrange(desc(n)) %>% head()
## UK
OG_candidate_sources %>% filter(iso_code=='GBR') %>% distinct(Outbreak_id, .keep_all = TRUE) %>% group_by(level1_name, level2_name) %>% count() %>% arrange(desc(n)) %>% head()
## Netherlands
OG_candidate_sources %>% filter(iso_code=='NLD') %>% distinct(Outbreak_id, .keep_all = TRUE) %>% group_by(level1_name, level2_name) %>% count() %>% arrange(desc(n)) %>% head()
## Russia
OG_candidate_sources %>% filter(iso_code=='RUS') %>% distinct(Outbreak_id, .keep_all = TRUE) %>% group_by(level1_name, level2_name) %>% count() %>% arrange(desc(n)) %>% head()
## Canada
OG_candidate_sources %>% filter(iso_code=='CAN') %>% distinct(Outbreak_id, .keep_all = TRUE) %>% group_by(level1_name, level2_name) %>% count() %>% arrange(desc(n))
OG_candidate_sources %>% filter(iso_code=='CAN') %>% filter(level1_name=='New Brunswick'|level1_name=='Nova Scotia'|level1_name=='Newfoundland and Labrador') %>% distinct(Outbreak_id, .keep_all = TRUE) %>% group_by(level1_name, level2_name) %>% count() %>% arrange(desc(n))
OG_candidate_sources %>% filter(iso_code=='CAN') %>% filter(level1_name=='Manitoba') %>% distinct(Outbreak_id, .keep_all = TRUE) %>% group_by(level1_name, level2_name) %>% count() %>% arrange(desc(n)) %>% head()
OG_candidate_sources %>% filter(iso_code=='CAN') %>% filter(level1_name=='Ontario'|level1_name=='Quebec') %>% distinct(Outbreak_id, .keep_all = TRUE) %>% group_by(level1_name, level2_name) %>% count() %>% arrange(desc(n)) %>% head()
## Mexico
OG_candidate_sources %>% filter(iso_code=='MEX') %>% distinct(Outbreak_id, .keep_all = TRUE) %>% group_by(level1_name, level2_name) %>% count() %>% arrange(desc(n))

## Finding US connections from monitoring Locations  ----
## Japan
banding %>% filter((ï..B_COUNTRY_=='US' & E_COUNTRY_=='JA')|(ï..B_COUNTRY_=='JA' & E_COUNTRY_=='Us')) %>% View()
## France
banding %>% filter((ï..B_COUNTRY_=='US' & E_COUNTRY_=='FR')|(ï..B_COUNTRY_=='FR' & E_COUNTRY_=='Us'))
## Canada
# Manitoba
banding %>% filter((ï..B_COUNTRY_=='US' & banding$FIPS_ADMIN_Recov=='CA03')|(banding$FIPS_ADMIN_banding=='CA03' & E_COUNTRY_=='Us')) %>% View()
# Ontario
banding %>% filter((ï..B_COUNTRY_=='US' & banding$FIPS_ADMIN_Recov=='CA08')|(banding$FIPS_ADMIN_banding=='CA08' & E_COUNTRY_=='Us')) %>% View()
# Alberta
banding %>% filter((ï..B_COUNTRY_=='US' & banding$FIPS_ADMIN_Recov=='CA01')|(banding$FIPS_ADMIN_banding=='CA01' & E_COUNTRY_=='Us')) %>% View()
# Nova Scotia
banding %>% filter((ï..B_COUNTRY_=='US' & banding$FIPS_ADMIN_Recov=='CA07')|(banding$FIPS_ADMIN_banding=='CA07' & E_COUNTRY_=='Us')) %>% View()
## Russia
banding %>% filter((ï..B_COUNTRY_=='US' & E_COUNTRY_=='RS')|(ï..B_COUNTRY_=='RS' & E_COUNTRY_=='Us')) %>% View()
## Mexico
banding %>% filter((ï..B_COUNTRY_=='US' & banding$FIPS_ADMIN_Recov=='MX31')|(banding$FIPS_ADMIN_banding=='MX31' & E_COUNTRY_=='Us')) %>% View()
## Netherlands
banding %>% filter((ï..B_COUNTRY_=='US' & E_COUNTRY_=='NL')|(ï..B_COUNTRY_=='NL' & E_COUNTRY_=='Us')) %>% View()
## UK
banding %>% filter((ï..B_COUNTRY_=='US' & E_COUNTRY_=='UK')|(ï..B_COUNTRY_=='UK' & E_COUNTRY_=='Us')) %>% View()




# Export Data ----
write.csv(OG_candidate_sources,"Candidate_Source_List.csv", row.names = FALSE)
write.csv(First_US_Cases,"US_First_Outbreaks.csv", row.names = FALSE)
write.csv(condensed_candidates, "condensed_candidates.csv")
