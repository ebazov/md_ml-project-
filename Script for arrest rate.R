library(tidyverse)
library(rgeos)
library(rgdal)
library(lubridate)

rm(list=ls())

#Loading in arrests dataset:
nyc_arrests<- read_csv("/Users/ekanta/Downloads/NYPD_Arrests_Data__Historic_.csv")

#Filtering for year 2013: 
nyc_arrests$ARREST_DATE= as.Date(nyc_arrests$ARREST_DATE, format = "%m/%d/%Y")
nyc_arrests$ARREST_YEAR= year(nyc_arrests$ARREST_DATE)
nyc_arrests= nyc_arrests %>% filter(ARREST_YEAR==2013)  


#No. of unique precincts are 77:
length(unique(nyc_arrests$ARREST_PRECINCT))




#Loading in precinct pop dataset:
precinct_pop<- read_csv("/Users/ekanta/Downloads/NYC_Blocks_2010CensusData_Plus_Precincts (1).csv")


#Calculating the population for each precinct: 
precinct_pop= precinct_pop %>% group_by(precinct) %>% summarise(Pop= sum(P0010001))


#Merging the nyc_arrests and stops dataset:
crime_precinct_pop= merge(nyc_arrests, precinct_pop, by.x='ARREST_PRECINCT', by.y='precinct')



#Checking for missing precincts: It seems like precinct 121 is missing from the stops_pop dataset..
setdiff(nyc_arrests$ARREST_PRECINCT, precinct_pop$precinct)


#Calculating the arrest rate for each precinct:
crime_precinct_pop= crime_precinct_pop %>% group_by(ARREST_PRECINCT, Pop) %>% count(n=n()) %>% mutate(nn/Pop)


#Reading in graduation dataset: (file loaded after running till like 286 in the Graduation Outcomes between 2016-2017.R):
school_safety<- read_csv("/Users/ekanta/grad_2013.csv", na= c("N/A", "NA", "#N/A"))

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

write.csv(crime_school, "arrest_rate_graduation_data.csv")

