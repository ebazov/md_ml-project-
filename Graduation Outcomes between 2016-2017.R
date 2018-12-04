
#### Project INFO ####
#Formula

# Graduation ~ Demographics_School + Avg_Income_Neighborhood +  Safety_School +
# Class_Size_School + Crime_Neighborhood + Funding_School + %ESL_School + %Special_Ed_School s


#Training Years: 2008 to 2012 Incoming
#Test Year: 2013 Incoming

#### Load Packages ####
library(broom)
library(tidyverse)


#### Load Graduation Data ####
graduation <- read_csv('Data/Input/2016-2017_Graduation_Outcomes_School.csv')


graduation <- graduation %>%  select(DBN, `School Name`, `Demographic Variable`, `Cohort Year`, 
                                     `Cohort`, `Total Cohort #`, `Total Grads #`, 
                                     `Total Grads % of cohort`)

#Rename Columns
names(graduation)[3] <- "Demographic"
names(graduation) <- gsub(" ", "_", names(graduation))
names(graduation) <- gsub("#", "", names(graduation))
names(graduation) <- gsub("_$", "", names(graduation))
names(graduation) <- gsub("%", "per", names(graduation))

#Filter Out Cohorts that did not graduate by End of Summer of their senior year (4 years)
graduation <- graduation %>%  filter(Cohort == "4 year August" ) %>%  select(- Cohort)
graduation <- graduation %>%  filter(Demographic == "All Students" ) %>%  select(- Demographic)

#Convert Number Variables from Character to Numeric
graduation <- graduation %>% 
                    mutate(Cohort_Total = as.numeric(Total_Cohort),
                           Grad_Total = as.numeric(Total_Grads)) %>% 
                    select(-Total_Cohort, - Total_Grads)  


#Convert Grad Rate to a Percentage
graduation <- graduation %>%  
                        mutate(Grad_Rate = as.numeric(Total_Grads_per_of_cohort) / 100 ) %>%  
                        select(-Total_Grads_per_of_cohort)

#Look at Closed Schools
graduation %>%  
  group_by(DBN, School_Name) %>%  
  summarize(latest_cohort = max(Cohort_Year)) %>% 
  filter(latest_cohort < 2013) %>% 
  as.data.frame()
#NOTE: you can google search these names.  It appears that most of them were closed.

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

#Convert Borough to Factor with Manhattan as Reference
location_info$Borough <- as.factor(location_info$Borough)
location_info$Borough <- relevel(location_info$Borough, ref = "M")

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
                      select(dbn, Latitude, Longitude, boro, neighborhood)
X321_locate <- address_2018 %>%  
  filter(dbn == "12X321") %>% 
  select(dbn, Latitude, Longitude, boro, neighborhood)

#Fill in M552 Data
graduation[which(graduation$DBN=="06M552") , ]$Latitude <- M552_locate$Latitude
graduation[which(graduation$DBN=="06M552") , ]$Longitude <- M552_locate$Longitude
graduation[which(graduation$DBN=="06M552") , ]$Borough <- as.character(M552_locate$boro)
graduation[which(graduation$DBN=="06M552") , ]$NTA <- M552_locate$neighborhood


#Fill in X321 Data
graduation[which(graduation$DBN=="12X321") , ]$Latitude <- X321_locate$Latitude
graduation[which(graduation$DBN=="12X321") , ]$Longitude <- X321_locate$Longitude
graduation[which(graduation$DBN=="12X321") , ]$Borough <- as.character(X321_locate$boro)
graduation[which(graduation$DBN=="12X321") , ]$NTA <- X321_locate$neighborhood

#Check Missing Data
colSums(is.na(graduation))

#Rename columns
names(graduation)[which(names(graduation)=='NTA')] <- "Neighborhood"

#Linear Regression of Grad Rate on Borough (Equivalent to ANOVA)
lm(Grad_Rate ~ Borough, data = graduation) %>%  summary()


#### Merge Demographic Data ####

#Read in Data from 2005 to 2011
demo_data_pre_2013 <- read_csv("Data/Input/2006_-_2012_School_Demographics_and_Accountability_Snapshot.csv",
                               na = c("n/a", "NA"))

#Select Relevant Cols
demo_data_pre_2013 <- demo_data_pre_2013 %>%  
                        select(DBN, Name, schoolyear, total_enrollment, 
                               male_per, female_per,
                               ell_percent, sped_percent,
                               asian_per, black_per, hispanic_per, white_per)

#Cut academic school to fall year
demo_data_pre_2013$schoolyear  <- demo_data_pre_2013 %>%
                                      pull(schoolyear) %>% 
                                      as.character() %>%   
                                      substr(start = 1, stop = 4) %>%
                                      as.numeric()

colSums(is.na(demo_data_pre_2013)) #Some Missing ELL data


#Read in Data from 2012 and 2013
demo_data_2012_2013 <- read_csv("Data/Input/2013_-_2018_Demographic_Snapshot_School.csv")

names(demo_data_2012_2013) <- gsub(pattern = " ",
                                   replacement = "_",
                                   x = names(demo_data_2012_2013))
names(demo_data_2012_2013) <- gsub(pattern = "%",
                                   replacement = "percent",
                                   x = names(demo_data_2012_2013))


#Select Relevant Cols
demo_data_2012_2013 <- demo_data_2012_2013 %>%  
                          select(DBN, School_Name, Year, Total_Enrollment,
                                  percent_Male, percent_Female, 
                                 percent_English_Language_Learners,percent_Students_with_Disabilities,
                                 percent_Asian, percent_Black, percent_Hispanic, percent_White)
 
#Recode School Year as Fall Year    
demo_data_2012_2013$Year <- demo_data_2012_2013%>%
                                          pull(Year) %>% 
                                          as.character() %>%   
                                          substr(start = 1, stop = 4) %>%
                                          as.numeric()

#Subset rows from 2012 to 2013 school years
demo_data_2012_2013 <- demo_data_2012_2013 %>% 
                            filter(Year %in% c(2012,2013) )

#Rename "Year" as "School_Year"
names(demo_data_2012_2013)[which(names(demo_data_2012_2013)=="Year") ] <- "School_Year"

#Check that Columns Match
names(demo_data_pre_2013)
names(demo_data_2012_2013)

#Rename pre_2013 cols
names(demo_data_pre_2013) <- names(demo_data_2012_2013)

#Bind Datasets
demographics <- rbind(demo_data_pre_2013, demo_data_2012_2013)

#Change Names and Convert to percents
demographics <- demographics %>% 
                        mutate(per_Male = percent_Male ,
                               per_Female = percent_Female ,
                               per_ELL = percent_English_Language_Learners ,
                               per_SWD = percent_Students_with_Disabilities ,
                               per_Asian = percent_Asian ,
                               per_Black = percent_Black ,
                               per_Hispanic = percent_Hispanic ,
                               per_White = percent_White ) %>% 
                         select(-percent_Male, - percent_Female, 
                                -percent_Students_with_Disabilities,
                                -percent_Asian, -percent_Black, -percent_Hispanic, -percent_White)

#Merge Demographic Data with
graduation <- graduation %>% 
                  left_join(demographics, c("DBN", "Cohort_Year"= "School_Year"))



#NOTE: 2012 Demographic data is not available !!!!

#Transfer High Schools 
transfer_schools <- c("02M544", "02M586", "08X537", "01M650", 
                      "03M505", "04M310","05M285", "07X379", 
                      "10X319", "12X480", "13K616", "15K529",
                      "15K698", "16K669", "17K568", "18K673", 
                      "21K728", "22K630","23K643", "23K646", 
                      "23K647", "32K564", "24Q744", "25Q540",
                      "25Q792", "27Q261", "31R470", "01M515",
                      "02M394", "02M550","10X397", "01M458", 
                      "02M313", "02M432", "02M560", "02M565",
                      "02M570", "2M575", "02M605", "03M404", 
                      "07X381", "08X377","12X446", "18K578",
                      "18K635", "28Q338", "15K423", "09X350",
                      "06M423") 


graduation$Transfer <- ifelse(graduation$DBN %in% transfer_schools,
                              1, 0 )


#### Build Linear Model ####

grad_2011 <- graduation %>%  filter(Cohort_Year == 2011) 


mod1 <- lm(Grad_Rate ~ Borough + per_Female + per_SWD + per_Asian + per_Black + per_Hispanic, 
   data = grad_2011 ) 
summary(mod1)

#Add per_ELL
mod2 <- lm(Grad_Rate ~ Borough + per_Female + per_SWD + per_Asian + per_Black + per_Hispanic +
     per_ELL, data = grad_2011 ) 
summary(mod2)
#Note: After controlling for ELL status, per_Asian no longer significant

     
#Compare Models 
anova(mod1, mod2)

#Use Neighborhoods instead of Boroughs
mod3 <- lm(Grad_Rate~ Neighborhood + per_Female + per_SWD + per_Asian + per_Black + per_Hispanic +
     per_ELL, data = grad_2011 )
summary(mod3)  
tidy(mod3) %>% filter(p.value <= .05) 


#Note: Should we use grouped binomial regression 
#Marc has done this before, so maybe he will help us. 

predict(mod1) %>%  range()
predict(mod2) %>%  range() #Predictions greater than 100
predict(mod3) %>%  range() #Predictions greater than 100

#Test Data
grad_2013 <-  graduation %>%  filter(Cohort_Year == 2013) 
pred_2013 <- predict(mod2, newdata = grad_2013 )


#### Build Grouped Logistic Model ####

grad_2011 <- graduation %>%  filter(Cohort_Year == 2011) 

grad_2011 <- grad_2011[complete.cases(grad_2011) , ]


mod <- glm(Grad_Rate ~  per_Female+ per_SWD + 
             per_Asian+ per_Black+ per_Hispanic,    
    data = grad_2011, weight =  Cohort_Total, 
    family = binomial(link = "logit")) 
summary(mod)

mod_check <- glm(cbind(Grad_Total, Cohort_Total - Grad_Total) ~
                   per_Female+ per_SWD + per_Asian+ per_Black+ per_Hispanic,
                 data = grad_2011,
                 family = binomial(link = "logit")) 
summary(mod_check)




pred_2011 <- predict(mod, type = "response")
#Check that Predictions are between 0  & 1
range(pred_2011 )

#Plot Actual Graduation Rate Over Predicted Rate
plot(pred_2011 , grad_2011$Grad_Rate, 
     xlim = c(0,1), 
     col = c("orange", "green")[grad_2011$Transfer + 1] )
abline(a =0, b= 1, col = "red", lty = "dashed" )
abline(h= .35, col = "blue", lty = "dashed")


#Model 2: Add Transfer as Indicator Variable 
mod2 <- glm(Grad_Rate ~  per_Female+ per_SWD + 
             per_Asian+ per_Black+ per_Hispanic + Transfer,    
           data = grad_2011, weight =  Cohort_Total, 
           family = binomial(link = "logit")) 
summary(mod2)
coef(mod2)%>%  exp()
coef(mod2) %>%  exp() - 1

plot(grad_2011$Grad_Rate~ grad_2011$per_SWD,
     col = c("blue", "red")[grad_2011$Transfer + 1])
abline(h = mean(grad_2011$Grad_Rate), lty = "dashed", col = "orange", lwd = 2)
abline(v = mean(grad_2011$per_SWD), lty = "dashed", col = "orange", lwd = 2)


plot(grad_2011$Grad_Rate~ grad_2011$per_Hispanic )
plot(grad_2011$Grad_Rate~ grad_2011$per_White )

plot(grad_2011$Grad_Rate~ grad_2011$per_Asian )

plot(grad_2011$Grad_Rate~ grad_2011$per_Black, 
     col = c("blue", "red")[grad_2011$Transfer + 1])
abline(h = mean(grad_2011$Grad_Rate), lty = "dashed", col = "orange", lwd = 2)
abline(v = mean(grad_2011$per_Black), lty = "dashed", col = "orange", lwd = 2)

plot(grad_2011$Grad_Rate~ grad_2011$per_ELL )
boxplot(grad_2011$Grad_Rate~ grad_2011$Borough )
plot(grad_2011$Grad_Rate~ grad_2011$Total_Enrollment )
plot(grad_2011$Grad_Rate~ factor(grad_2011$Transfer))



pred_2011_mod_2 <- predict(mod2, type = "response")

#Plot Actual Graduation Rate Over Predicted Rate
plot(pred_2011_mod_2, grad_2011$Grad_Rate, 
     xlim = c(0,1), 
     col = c("orange", "green")[grad_2011$Transfer + 1] )
abline(a =0, b= 1, col = "red", lty = "dashed" )
abline(h= .35, col = "blue", lty = "dashed")

range(pred_2011_mod_2)
summary(mod2)

anova(mod2, mod1, test = "Chisq")

tapply(grad_2011$Grad_Rate, grad_2011$Transfer, mean)
(0.2423256 / (1 - 0.2423256 )) / ( 0.7470154  / (1 - .7470154 ))
mod3 <- glm(Grad_Rate ~ Transfer,    
            data = grad_2011, weight =  Cohort_Total, 
            family = binomial(link = "logit")) 
summary(mod3)
coef(mod3) %>%  exp()



#NOTE: These schools that do not fit model all have graduation rates under 35
#Google Search indicates they many are "transfer" schools 
#"Transfer" schools are specifically for students at risk of dropping out
#We should be able to solve this by adding an indicator variable
#They seem to have the same slope but a different intercept
grad_2011 %>%  filter(Grad_Rate < 35) %>%  View()

#Next Steps:
## Identify Transfer Schools
## (Maybe?) Identifty Specialized Schools (e.g. Bronx Science)
## Add Free/Reduced Lunch Data
## Add School Safety Report Data
## (Maybe?) Add Neighborhood Violent Crime Data
## Add Class Size Data or Teacher/Pupil Ratio
## (Maybe?) Find school funding data
## (Maybe?) Add district as variable (may need to be MLM model )



#### Split Data ####
# education_train <- education %>% filter(`Cohort Year` %in% c(2008, 2009, 2010, 2011, 2012)) %>% filter( Cohort %in% c('4 year August', '4 year June'))
# write_csv(education_train, 'Data/Output/Graduation Outcomes for students: train data set.csv')
# education_test <- education %>% filter(`Cohort Year` %in% c(2013)) %>% filter( Cohort %in% c('4 year August', '4 year June'))
# write_csv(education_test, 'Data/Output/Graduation Outcomes for students: test data set.csv')
# 

