
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
education <- read_csv('2016-2017_Graduation_Outcomes_School.csv')

demographics <- read.csv('2006_-_2012_School_Demographics_And_Accountability_Snapshot.csv')



#Cut academic school to fall year
demographics$schoolyear <- demographics %>% pull(schoolyear) %>% as.character() %>%   substr(start = 1, stop = 4) %>% as.numeric()

#Join education and demographics
grad_combined <- left_join(x=education, y=demographics, by=c('DBN'='DBN', 'Cohort Year'='schoolyear'))


#Merge safety and previous combined
merged_schools <- left_join(grad_combined, safety, by=c('DBN'='Location.Code', 'Cohort Year'='School.Year'))

#### Split Data ####
education_train <- education %>% filter(`Cohort Year` %in% c(2008, 2009, 2010, 2011, 2012)) %>% filter( Cohort %in% c('4 year August', '4 year June'))
write_csv(education_train, 'Graduation Outcomes for students: train data set.csv')
education_test <- education %>% filter(`Cohort Year` %in% c(2013)) %>% filter( Cohort %in% c('4 year August', '4 year June'))
write_csv(education_test, 'Graduation Outcomes for students: test data set.csv')


#Check Years 
safety %>% select(School.Year) %>%  table()
demographics %>% select(schoolyear) %>%  table()

grad_combined %>% filter(`Cohort Year`< 2012) %>%  sapply(function(x) 1- mean(is.na(x)))



