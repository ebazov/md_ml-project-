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



#Filter Out Cohorts that did not graduate by End of Summer of their senior year (4 years)
graduation <- graduation %>%  filter(Cohort == "4 year August" ) %>%  select(- Cohort)

#Rename Columns
names(graduation)[3] <- "Demographic"
names(graduation) <- gsub(" ", "_", names(graduation))
names(graduation) <- gsub("#", "num", names(graduation))


#### Tidy Demographic Data####

#Rename Multi-Racial as Multi_Racial
graduation$Demographic <- gsub("-", "_", graduation$Demographic) 

#This section tidies dataframe to include  Demographic Data on Same Row for each School  

#Split Graduation by Demographic Factor into a list of dataframes 
demographic_list <- split(graduation, graduation$Demographic)


prepare_demo_data <- function(df){
  
  #This Function takes a dataframe ane renames cols (that are not keys) to include demographic variable 
  
  #Extract Name from Demographic Column 
  name_df <- unique(df[, 3]) %>%  as.character()
  name_df <- gsub(" ", "_", name_df)
  
  #Get Rid of Demographic Col
  df  <- df[, -3]
  
  #Paste Demo Name to Columns
  names(df)[4:10] <- paste(name_df, names(df)[4:10], sep = "_")
  
  return(df)
  
}

#Apply prepare_demo_data() function to each data frame in demographic list
named_demographic_list <- lapply(demographic_list, prepare_demo_data)


#Join Data from list back into one dataframe (NOTE: MAYBE A FOR LOOP could accomplish this)
graduation_tidy <- named_demographic_list[[1]] %>%  left_join(named_demographic_list[[2]]) %>%  left_join(named_demographic_list[[3]]) %>% left_join(named_demographic_list[[4]]) %>% 
                                 left_join(named_demographic_list[[5]]) %>%  left_join(named_demographic_list[[6]]) %>% left_join(named_demographic_list[[7]]) %>% 
                                  left_join(named_demographic_list[[8]]) %>%  left_join(named_demographic_list[[9]]) %>% left_join(named_demographic_list[[10]]) %>% 
                                    left_join(named_demographic_list[[11]]) %>%  left_join(named_demographic_list[[12]]) %>% left_join(named_demographic_list[[13]]) %>% 
                                    left_join(named_demographic_list[[14]]) %>%  left_join(named_demographic_list[[15]]) %>% left_join(named_demographic_list[[16]]) %>% 
                                    left_join(named_demographic_list[[17]])

#### Check Data ####

#By Gender
 mean(graduation_tidy$All_Students_Total_Cohort_num == graduation_tidy$Male_Total_Cohort_num + graduation_tidy$Female_Total_Cohort_num, na.rm = T)
 
 #By English Language Learner (ELL)
 mean(graduation_tidy$All_Students_Total_Cohort_num == graduation_tidy$SWD_Total_Cohort_num+ graduation_tidy$Not_SWD_Total_Cohort_num, na.rm = T)graduation_tidy
 
 #By English Language Learner (ELL)
 mean(graduation_tidy$All_Students_Total_Cohort_num == graduation_tidy$ELL_Total_Cohort_num+ graduation_tidy$Not_ELL_Total_Cohort_num,  na.rm = T)
 mean(graduation_tidy$All_Students_Total_Cohort_num == graduation_tidy$Former_ELL_Total_Cohort_num + graduation_tidy$Never_ELL_Total_Cohort_num + graduation_tidy$Current_ELL_Total_Cohort_num + graduation_tidy$Ever_ELL_Total_Cohort_num, na.rm= T) 
 #Note ELL refers to students who do not speak English as a native language and may need addition help 
#There seems to be something wrong with this
 
#Also Former and  
 
 #Ethnic Variables 
(graduation_tidy$All_Students_Total_Cohort_num == ifelse(is.na(graduation_tidy$Asian_Total_Cohort_num)==T , 0, graduation_tidy$Asian_Total_Cohort_num) +
                                      ifelse(is.na(graduation_tidy$Black_Total_Cohort_num)==T, 0, graduation_tidy$Black_Total_Cohort_num) + 
                                      ifelse(is.na(graduation_tidy$White_Total_Cohort_num)==T, 0, graduation_tidy$White_Total_Cohort_num) + 
                                      ifelse(is.na(graduation_tidy$Hispanic_Total_Cohort_num)==T, 0, graduation_tidy$Hispanic_Total_Cohort_num) +
                                      ifelse(is.na(graduation_tidy$Multi_Racial_Total_Cohort_num)==T, 0, graduation_tidy$Multi_Racial_Total_Cohort_num) +
                                       ifelse(is.na(graduation_tidy$Native_American_Total_Cohort_num)==T, 0, graduation_tidy$Native_American_Total_Cohort_num) ) %>%  mean()

#Seventeen Unique Demographic Variables
graduation$Demographic  %>%  unique()
 

#Check Class of Each Column
sapply(graduation_tidy, class)

#NOTE: we have to figure out what to do with "s", suppressed values.  Code as zero or generate random numbers ????
#NOTE: Also should NAs be coded as zero?  Most likely explanation. 

#### Merge Address Data ####

address_2013 <- read.csv("Data/Input/DOE_High_School_Directory_2013-2014.csv" )

#Check Missing SCchools
graduation_tidy %>% 
  left_join(address_2013, by = c("DBN" = "DBN")) %>%
  select(DBN, Cohort_Year, Location.1) %>% 
  filter(is.na(Location.1)==T) %>% 
  count(DBN) %>% View()

address_2013 <- read.csv("Data/Input/DOE_High_School_Directory_2013-2014.csv" )
address_2016 <- read.csv("Data/Input/Archived_DOE_High_School_Directory_2016.csv")



graduation_tidy %>%  
  anti_join(address_2013, by = c("DBN" = "DBN")) %>% 
  pull(DBN) %>% unique()

missing_2013 <- graduation_tidy %>%  
  anti_join(address_2013, by = c("DBN" = "DBN")) %>% 
  select(DBN) %>% 
  count(DBN) %>% 
  select(DBN)

nrow(missing_2013)


missing_2016 <- graduation_tidy %>%  
  anti_join(address_2016, by = c("DBN" = "dbn")) %>% 
  select(DBN) %>% 
  count(DBN) %>% 
  select(DBN) %>% 

nrow(missing_2016)


missing_2013 %>%  anti_join(missing_2016)

missing_2013 %>%  inner_join(missing_2016)  %>%   View()

graduation_tidy$Cohort_Year

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




