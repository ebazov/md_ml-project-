#setwd("Desktop/A3SR/APSTA-2047/md_ml-project-")

#Training Years: 2008 to 2012 Incoming
#Test Year: 2013 Incoming

#Need Demographics for 2012 to 2013 
#Need School Safety reports for 2008 to 2012 

#Formula
'
Graduation ~ Demographics_School + Avg_Income_Neighborhood +  Safety_School + 
Class_Size_School + Crime_Neighborhood + Funding_School + %ESL_School + %Special_Ed_School s
'

library(tidyverse)
graduation <- read_csv('Data/Input/2016-2017_Graduation_Outcomes_School.csv')


graduation <- graduation %>%  select(DBN, `School Name`, `Demographic Variable`, `Cohort Year`, `Cohort`, `Total Cohort #`, `Total Grads #`, 
                                     `Total Grads % of cohort`, `Still Enrolled #`, `Still Enrolled % of cohort`, `Dropped Out #`, `Dropped Out % of cohort`)



#Rename Columns
names(graduation)[3] <- "Demographic"
names(graduation) <- gsub(" ", "_", names(graduation))
names(graduation) <- gsub("#", "num", names(graduation))



#Filter Out Cohorts that did not graduate by End of Summer of their senior year (4 years)
graduation <- graduation %>%  filter(Cohort == "4 year August" ) %>%  select(- Cohort)
graduation <- graduation %>%  filter(Demographic == "All Students" ) %>%  select(- Demographic)
graduation <- graduation %>%  select(DBN, School_Name, Cohort_Year, `Total_Grads_%_of_cohort` )

#Look at Closed Schools
graduation %>%  
  group_by(DBN, School_Name) %>%  
  summarize(latest_cohort = max(Cohort_Year)) %>% 
  filter(latest_cohort < 2013) %>% 
  as.data.frame()
#NOTE: y ou can google search these names.  It appears that most of them were closed.

#Pull DBNs from Close Schools and store as character vector
closed_schools <- graduation %>%  
                      group_by(DBN, School_Name) %>%  
                      summarize(latest_cohort = max(Cohort_Year)) %>% 
                      filter(latest_cohort < 2013) %>% 
                      pull(DBN)

#Restrict Analysis to schools currently in operation
graduation <- graduation %>%  filter(DBN %in% closed_schools == F)

#### Merge Address Data ####

#Load Data from school_saftey
school_safety <- read_csv("Data/Input/2010_-_2016_School_Safety_Report.csv",
                          na =  c("N/A","NA", "#N/A"))

#Remove Empty  Spaces from colnames
names(school_safety) <- gsub(pattern = " ", replacement = "_",
                             x = names(school_safety))

#Select Cols with Location Info
location_info <- select(school_safety, DBN, Borough, Latitude, Longitude,
       NTA) 

#Remove Duplicate DBNs
location_info <- distinct(location_info , DBN, .keep_all = T)


#Merge Location Data with Graduation data
graduation <- graduation %>%  
                  left_join(location_info, by = "DBN") 

#Check Missing Schools
graduation %>%  
  group_by(DBN) %>%  
  summarize( missing_lat = sum(is.na(Latitude))) %>% 
  filter(missing_lat > 0)
  
#Fill in Two Gaps from Two Missing Schools
address_2018 <- read.csv("Data/Input/2018_DOE_High_School_Directory.csv")
M552_locate <- address_2018 %>%  
                      filter(dbn == "06M552") %>% 
                      select(dbn, Latitude, Longitude, Borough, neighborhood)
X321_locate <- address_2018 %>%  
  filter(dbn == "12X321") %>% 
  select(dbn, Latitude, Longitude, Borough, neighborhood)

#Fill in M552 Data
graduation[which(graduation$DBN=="06M552") , ]$Latitude <- M552_locate$Latitude
graduation[which(graduation$DBN=="06M552") , ]$Longitude <- M552_locate$Longitude
graduation[which(graduation$DBN=="06M552") , ]$Borough <- M552_locate$Borough
graduation[which(graduation$DBN=="06M552") , ]$NTA <- M552_locate$neighborhood


#Fill in X321 Data
graduation[which(graduation$DBN=="12X321") , ]$Latitude <- X321_locate$Latitude
graduation[which(graduation$DBN=="12X321") , ]$Longitude <- X321_locate$Longitude
graduation[which(graduation$DBN=="12X321") , ]$Borough <- X321_locate$Borough
graduation[which(graduation$DBN=="12X321") , ]$NTA <- X321_locate$neighborhood

#Check Missing Data
colSums(is.na(graduation))

### Merge Demographic Data ###

### NOT SURE IF WE NEED THIS GIVEN WE HAVE DEMOGRAPHIC VARIABLES IN PREVIOUS DATASET

#Cut academic school to fall year
demographics$schoolyear <- demographics %>% pull(schoolyear) %>% as.character() %>%   substr(start = 1, stop = 4) %>% as.numeric()

#Join education and demographics
education <- left_join(x=graduation , y=demographics, by=c('DBN'='DBN', 'Cohort Year'='schoolyear'))







#### Split Data ####
education_train <- education %>% filter(`Cohort Year` %in% c(2008, 2009, 2010, 2011, 2012)) %>% filter( Cohort %in% c('4 year August', '4 year June'))
write_csv(education_train, 'Data/Output/Graduation Outcomes for students: train data set.csv')
education_test <- education %>% filter(`Cohort Year` %in% c(2013)) %>% filter( Cohort %in% c('4 year August', '4 year June'))
write_csv(education_test, 'Data/Output/Graduation Outcomes for students: test data set.csv')




#### Extra Code ####
#safety <- read.csv('Data/Input/2010_-_2016_School_Safety_Report.csv')

#Change School.Year to Numeric
#safety$School.Year <- as.numeric(as.character(safety$School.Year))
#Merge safety and previous combined
#merged_schools <- left_join(grad_combined, safety, by=c('DBN'='Location.Code', 'Cohort Year'='School.Year'))




