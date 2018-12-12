library(tidyverse)
library(rgeos)
library(rgdal)


#Loading in arrests dataset:
nyc_arrests<- read_csv("/Users/ekanta/Downloads/NYPD_Arrests_Data__Historic_.csv")


#No. of unique precincts are 77:
length(unique(nyc_arrests$ARREST_PRECINCT))


#Loading in the merged stop&frisk and county-level population data:
stops_pop<- read_csv("/Users/ekanta/Downloads/Stop_Frisk 3/OverallStopsPop2012.csv")


#Calculating the population for each precinct: 
stops_pop= stops_pop %>% group_by(precinct) %>% summarise(Pop=sum(D001, na.rm=T)) %>% select(precinct, Pop)


#Merging the nyc_arrests and stops dataset:
crime_precinct_pop= merge(nyc_arrests, stops_pop, by.x='ARREST_PRECINCT', by.y='precinct')


#Checking for missing precincts: It seems like precinct 121 is missing from the stops_pop dataset..
setdiff(nyc_arrests$ARREST_PRECINCT, stops_pop$precinct)
nyc_arrests %>% filter (nyc_arrests$ARREST_PRECINCT==121)


#Calculating the arrest rate for each precinct:
crime_precinct_pop= crime_precinct_pop %>% group_by(ARREST_PRECINCT, Pop) %>% count(n=n()) %>% mutate(nn/Pop)


#Reading in graduation dataset: (file loaded after running till like 286 in the Graduation Outcomes between 2016-2017.R):
school_safety<- read_csv("/Users/ekanta/gradmdml.csv", na= c("N/A", "NA", "#N/A"))

#Filter for 2013:
school_safety13= school_safety %>% filter(Cohort_Year==2013)

#Checking for missing lat/long values: (None missing)
colSums(is.na(school_safety13))


#Loading the shapefile for precincts:
filepath= "/Users/ekanta/Downloads/Police Precincts 4"
precinct_shape= readOGR(dsn=filepath, layer="geo_export_eb0eaa6c-3c5d-43fb-9f53-0834474da7e8")


#Extracting coordinates and converting into a SpatialPointsDataFrame object:
coords= data.frame(school_safety$Longitude, school_safety$Latitude)
coords_df <- SpatialPointsDataFrame(coords, data=school_safety,  proj4string = CRS("+proj=longlat +ellps=WGS84 +no_defs"))



#Overalying coordinates from the graduation dataset onto the precinct shapefile to get matches: 
pts=over(coords_df, precinct_shape[,"precinct"])
coords_df$precinct= pts$precinct


#Merge crime_precinct_pop and coords_df to add in precinct features:
crime_school= merge(crime_precinct_pop, coords_df@data, by.x='ARREST_PRECINCT', by.y='precinct')
crime_school$n=NULL
crime_school= crime_school %>% dplyr::rename(Population=Pop, Number_of_Arrests=nn, Arrest_Rate=`nn/Pop`)

write.csv(crime_school, "arrest_rate_graduation.csv")







