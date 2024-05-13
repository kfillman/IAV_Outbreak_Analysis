# Visuals

# load libraries & data (if not already loaded)
candidate_sources <- read.csv("C:/Users/Kath/Documents/IAV_Outbreak_Analysis/Candidate_Source_List.csv")
First_US_Cases <- read.csv("C:/Users/Kath/Documents/IAV_Outbreak_Analysis/US_First_Outbreaks.csv")
library(geosphere)
library(maps)
library(sf)
library(tidyverse)


# Data Transformation -------------------------------

candidates <- candidate_sources %>%
  mutate(time_to_infection=(state_first_infection-Outbreak_start_date)) %>%
  filter(time_to_infection<=31) %>%
  group_by(state, country, level1_name) %>% 
  distinct(Outbreak_id, .keep_all = TRUE)

# Connection Map ------------------------------------
## Create lists of points ---------------------------

### List of all points ------------------------------
# Outside US Sources by country
sources1 <- candidate_sources %>%
  filter(country_unique_code!='USA') %>%
  select(country, level1_name, Latitude, Longitude) %>%
  group_by(country) %>%
  summarise(Lat=mean(as.numeric(Latitude)), Long=mean(as.numeric(Longitude)))
sources <- data.frame(long= sources1$Long,
                     lat= sources1$Lat)
rownames(sources) <- sources1$country
# Outside US Sources by lvl 1 name
sources1 <- candidate_sources %>%
  filter(country_unique_code!='USA') %>%
  select(country, level1_name, Latitude, Longitude) %>%
  group_by(level1_name) %>%
  summarise(Lat=mean(as.numeric(Latitude)), Long=mean(as.numeric(Longitude)))
sources_lvl1 <- data.frame(long= sources1$Long,
                      lat= sources1$Lat)
rownames(sources_lvl1) <- sources1$level1_name
## State locations
states <- data.frame(long= First_US_Cases$Longitude,
                 lat= First_US_Cases$Latitude)
rownames(states) <- First_US_Cases$level1_name
# Merge two
locations <- rbind(sources, states) %>% as.data.frame()
rownames(locations) <- gsub(" ", "_", rownames(locations))
locations$lat <- as.numeric(locations$lat)
locations$long <- as.numeric(locations$long)

### Create list of connected locations ---------------
connections <- candidate_sources %>%
  select(state, country, level1_name, state_long, Longitude, state_lat, Latitude) %>%
  group_by(state, level1_name) %>%
  summarise(state_long=mean(state_long), Long=mean(as.numeric(Longitude)), state_lat=mean(state_lat), Lat=mean(as.numeric(Latitude))) %>% 
  ungroup %>% 
  select(-state, -level1_name)

## Mapping -------------------------------------------

# A function to plot connections
plot_my_connection=function( dep_lon, dep_lat, arr_lon, arr_lat, ...){
  inter <- gcIntermediate(c(dep_lon, dep_lat), c(arr_lon, arr_lat), n=50, addStartEnd=TRUE, breakAtDateLine=F)             
  inter=data.frame(inter)
  diff_of_lon=abs(dep_lon) + abs(arr_lon)
  if(diff_of_lon > 180){
    lines(subset(inter, lon>=0), ...)
    lines(subset(inter, lon<0), ...)
  }else{
    lines(inter, ...)
  }
}

# No margin
par(mar=c(0,0,0,0))

# Basic world map
map('world',
    col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05,
    mar=rep(0,4),border=0, ylim=c(-80,80) 
)

# Run function
for(i in 1:nrow(connections)){
  plot_my_connection(connections$state_long[i], connections$state_lat[i], connections$Long[i], connections$Lat[i], col="skyblue", lwd=1)
}
# Generate all pairs of coordinates
points(x=sources_lvl1$long, y=sources_lvl1$lat, col="slateblue", cex=1, pch=20)
text(rownames(sources), x=sources$long, y=sources$lat,  col="slateblue", cex=1, pos=4)

# Choropleth --------------------------------------------
# read in data
my_sf <- read_sf("C:/Users/Kath/Documents/IAV_Outbreak_Analysis/Outbreak_data/admin_unit/Admin_unit.shp")
#plot world map
par(mar = c(0, 0, 0, 0))
ggplot(my_sf) +
  geom_sf(fill = "white", color = "black", linewidth=0.1) +
  theme_void()
# population reads from shapefile
my_sf %>%
  ggplot(aes(x = POP_ADMIN)) +
  geom_histogram(bins = 10, fill = "skyblue", color = "black") +
  scale_x_log10()
