#William Spagnola


#Final Project 
library(tidyverse)

#setwd("Desktop/A3SR/APSTA-2047/md_ml-project-")

#Load in Data from 2003 Cohort: Students who enrolled in ninth grade in 2003; graduate in 2007? 
cohort_05 <- read_csv("../Data/cohort_05.csv")
 dim(cohort_05) 
names(cohort_05) <- gsub(pattern = " ", replacement = "_", names(cohort_05))


##NYC OPEN DATA
#https://data.cityofnewyork.us/Education/2005-2011-Graduation-Outcomes-School-Level-Classes/cma4-zi8m/data
# FILTER Cohort Year = 2005
#Cohort Category = 4-year August

#Rename Graduation Rate Column and Convert Data to Numeric 
cohort_05 <- rename(cohort_05, graduation_rate = Total_Grads_Pct_of_cohort)
cohort_05$graduation_rate <- gsub(pattern = "%", replacement = "", cohort_05$graduation_rate)
cohort_05 <- cohort_05 %>%  mutate( graduation_rate = as.numeric(graduation_rate))


#Create Borough Col
cohort_05$borough <- NA
cohort_05[grepl(pattern= "M", x=cohort_05$DBN) , ]$borough <- "Manhattan"
cohort_05[grepl(pattern= "Q", x=cohort_05$DBN) , ]$borough <- "Queens"
cohort_05[grepl(pattern= "X", x=cohort_05$DBN) , ]$borough <- "Bronx"
cohort_05[grepl(pattern= "K", x=cohort_05$DBN) , ]$borough <- "Brooklyn"
cohort_05[grepl(pattern= "R", x=cohort_05$DBN) , ]$borough <- "Staten Island"
cohort_05$borough <- as.factor(cohort_05$borough)


#Linear Regression (Equivalent to ANOVA)
mod1 <- lm(graduation_rate~borough, data = cohort_05)
summary(mod1)
anova1 <- aov(graduation_rate~borough, data = cohort_05)
summary(mod1)


#https://data.cityofnewyork.us/Education/2010-2016-School-Safety-Report/qybk-bjjc/data
#Get Addresses
safety <- read.csv("../Data/school_safety_10_to_16.csv")
safety_15 <- filter(safety, School.Year == "2015-16") # Get 2015 schooly year info
location_info <- select(safety_15, DBN, Address, Latitude, Longitude)
merge <- left_join(cohort_05,location_info ) 

#Missing Schools 54 school addresses
merge %>%  select(Address) %>% is.na() %>% sum()


'
NEXT STEPS:
1) Is there a way to merge schools with police precincts using addresses?  Maybe with ArcGIS ?
2) There is different ways to calculate graduate rates.  Cohorts drop out or move in earlier years.
May be we need Ravi to help us with this.
3) Things to Merge 
     a) Police Precinct Crimes Data
     b) Free Lunch
     c) Teacher/Student Ratio 

'
