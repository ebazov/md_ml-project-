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
x <- named_demographic_list[[1]] %>%  left_join(named_demographic_list[[2]]) %>%  left_join(named_demographic_list[[3]]) %>% left_join(named_demographic_list[[4]]) %>% 
                                 left_join(named_demographic_list[[5]]) %>%  left_join(named_demographic_list[[6]]) %>% left_join(named_demographic_list[[7]]) %>% 
                                  left_join(named_demographic_list[[8]]) %>%  left_join(named_demographic_list[[9]]) %>% left_join(named_demographic_list[[10]]) %>% 
                                    left_join(named_demographic_list[[11]]) %>%  left_join(named_demographic_list[[12]]) %>% left_join(named_demographic_list[[13]]) %>% 
                                    left_join(named_demographic_list[[14]]) %>%  left_join(named_demographic_list[[15]]) %>% left_join(named_demographic_list[[16]]) %>% 
                                    left_join(named_demographic_list[[17]])

#### Check Data ####

#By Gender
 mean(x$All_Students_Total_Cohort_num == x$Male_Total_Cohort_num + x$Female_Total_Cohort_num, na.rm = T)
 
 #By English Language Learner (ELL)
 mean(x$All_Students_Total_Cohort_num == x$SWD_Total_Cohort_num+ x$Not_SWD_Total_Cohort_num, na.rm = T)x
 
 #By English Language Learner (ELL)
 mean(x$All_Students_Total_Cohort_num == x$ELL_Total_Cohort_num+ x$Not_ELL_Total_Cohort_num,  na.rm = T)
 mean(x$All_Students_Total_Cohort_num == x$Former_ELL_Total_Cohort_num + x$Never_ELL_Total_Cohort_num + x$Current_ELL_Total_Cohort_num + x$Ever_ELL_Total_Cohort_num, na.rm= T) 
 #Note ELL refers to students who do not speak English as a native language and may need addition help 
#There seems to be something wrong with this
 
#Also Former and  
 
 #Ethnic Variables 
(x$All_Students_Total_Cohort_num == ifelse(is.na(x$Asian_Total_Cohort_num)==T , 0, x$Asian_Total_Cohort_num) +
                                      ifelse(is.na(x$Black_Total_Cohort_num)==T, 0, x$Black_Total_Cohort_num) + 
                                      ifelse(is.na(x$White_Total_Cohort_num)==T, 0, x$White_Total_Cohort_num) + 
                                      ifelse(is.na(x$Hispanic_Total_Cohort_num)==T, 0, x$Hispanic_Total_Cohort_num) +
                                      ifelse(is.na(x$Multi_Racial_Total_Cohort_num)==T, 0, x$Multi_Racial_Total_Cohort_num) +
                                       ifelse(is.na(x$Native_American_Total_Cohort_num)==T, 0, x$Native_American_Total_Cohort_num) ) %>%  mean()

#Seventeen Unique Demographic Variables
graduation$Demographic  %>%  unique()
 

#Check Class of Each Column
sapply(x, class)

#NOTE: we have to figure out what to do with "s", suppressed values.  Code as zero or generate random numbers ????
#NOTE: Also should NAs be coded as zero?  Most likely explanation. 

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




