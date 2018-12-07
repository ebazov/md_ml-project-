
#### Project INFO ####
#Formula

# Graduation ~ Demographics_School + Avg_Income_Neighborhood +  Safety_School +
# Class_Size_School + Crime_Neighborhood + Funding_School + %ESL_School + %Special_Ed_School s


#Training Years: 2008 to 2012 Incoming
#Test Year: 2013 Incoming

#### Load Packages ####
library(broom)
library(tidyverse)
library(ROCR)
library(glmnet)

#### Load Graduation Data ####
graduation <- read_csv('Data/Input/2016-2017_Graduation_Outcomes_School.csv')

#Select Cols
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
#lm(Grad_Rate ~ Borough, data = graduation) %>%  summary()


#### Merge School-level Crime Data ####

#Select Major Crimes, Violent Crimes, and Property Crimes  (and DBN and School Year as keys)

#Recode Year to just Fall year
school_safety$School_Year <- school_safety %>% pull(School_Year) %>% 
                                  as.character() %>%   
                                  substr(start = 1, stop = 4) %>%
                                  as.numeric()

#Extract Building Level Crime Data (Many schools are co-located in New York.)
building_crime_stats <- school_safety %>% 
                                  mutate(Register = replace_na(Register, 0),
                                         Vio_N = replace_na(Vio_N, 0),
                                         Prop_N = replace_na(Prop_N, 0),
                                         Major_N = replace_na(Major_N, 0),
                                         NoCrim_N = replace_na(NoCrim_N, 0)) %>% 
                                  group_by(Building_Code, School_Year) %>%  
                                  summarize(Register = max(Register),
                                            Violent_Rate = max(Vio_N),
                                            Property_Crime_Rate = max(Prop_N),
                                            Major_Crime_Rate = max(Major_N),
                                            Behavior_Rate = max(NoCrim_N)) %>% 
                                  mutate( Violent_Rate = Violent_Rate / Register*100,
                                          Property_Crime_Rate = Property_Crime_Rate / Register*100,
                                          Major_Crime_Rate =  Major_Crime_Rate / Register*100,
                                          Behavior_Rate = Behavior_Rate / Register*100)
  


#Select DBN and School Year as IDs and Build Coding as key. Remove Empty DBN (Representing Buildings)
school_crime <- school_safety %>% 
                      select(DBN, School_Year, Building_Code)  %>% 
                      filter(DBN != "")  

#Merge Building Crime Data with Schools
school_crime <- school_crime %>%
                      left_join(building_crime_stats, by = c("Building_Code", "School_Year"))

graduation <- graduation %>% left_join(school_crime, by=c('DBN', 'Cohort_Year'='School_Year'))

#### Merge Demographic Data ####

#Read in Data from 2005 to 2011
demo_data_pre_2013 <- read_csv("Data/Input/2006_-_2012_School_Demographics_and_Accountability_Snapshot.csv",
                               na = c("n/a", "NA"))

#Select Relevant Cols
demo_data_pre_2013 <- demo_data_pre_2013 %>% 
                        mutate(per_Poverty = case_when(
                                                is.na(fl_percent)== F ~ fl_percent,
                                                is.na(frl_percent)== F ~ frl_percent)) %>% 
                        select(DBN, Name, schoolyear, total_enrollment, 
                               male_per, female_per,
                               ell_percent, sped_percent,
                               asian_per, black_per, hispanic_per, white_per,
                               per_Poverty)

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
                                   replacement = "per",
                                   x = names(demo_data_2012_2013))


#Select Relevant Cols
demo_data_2012_2013 <- demo_data_2012_2013 %>%  
                          select(DBN, School_Name, Year, Total_Enrollment,
                                  per_Male, per_Female, 
                                  per_English_Language_Learners, per_Students_with_Disabilities,
                                  per_Asian, per_Black, per_Hispanic, per_White, 
                                  per_Poverty) 
 
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

#Rename pre_2013 cols
names(demo_data_pre_2013) <- names(demo_data_2012_2013)

#Bind Datasets
demographics <- rbind(demo_data_pre_2013, demo_data_2012_2013)

#Abbreviate ELL and SWD
demographics <- demographics %>% 
                        rename(per_ELL = per_English_Language_Learners ,
                               per_SWD = per_Students_with_Disabilities ) 

#Merge Demographic Data with
graduation <- graduation %>% 
                  left_join(demographics, c("DBN", "Cohort_Year"= "School_Year"))


#### Code Transfer High Schools  ####
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


#### Add District Variable ####

graduation$District <- graduation %>%
  pull(DBN) %>% 
  substr(start = 1, stop = 2)  %>% as.factor()

#### Filter 2013 Data ####


grad_2013 <-  graduation %>%  filter(Cohort_Year == 2013) 

#Check Total Number of High Schools in 2013 
nrow(grad_2013)

#### Missing Data Analysis of 2013 ####

#Examine Missing Item Data
colSums(is.na(grad_2013))

#Look at Missing Data Listwise 

#Eight were missing Grad Rate  (8 Missing Total)
missing_data <- grad_2013[complete.cases(grad_2013)==F, ]
missing_data[is.na(missing_data$Grad_Rate)==T , ] %>% nrow()

#One Missing Crime Rate but not Grad Rate (7 Missing Total )
missing_data %>% filter( is.na(missing_data$Major_Crime_Rate)==T  & 
                          is.na(missing_data$Grad_Rate)== F  &
                           is.na(missing_data$per_ELL)== F ) %>% nrow()

#9 Observations Dropped because they were missing ELL (16 missing Total )
missing_data %>%  filter(is.na(missing_data$per_ELL)==T & 
                          is.na(missing_data$Major_Crime_Rate)==F &
                           is.na(missing_data$Grad_Rate)==F ) %>%  nrow()
#18 Obsevations Missing Total 
nrow(missing_data) / nrow(grad_2013) * 100
# 3.8% Observations Dropped due to missing Data

####Complete Cases Only####

grad_2013 <- grad_2013[complete.cases(grad_2013),]

#### Train/Test Split ####
set.seed(9999)
train <- grad_2013 %>% sample_frac(0.8) 
test <- grad_2013 %>% filter(!(DBN %in% train$DBN))

#### Fit GLM Model on Training Data ####

#Model with Demographic, ELL and SWD
mod <- glm(Grad_Rate ~ per_Female + 
             per_Asian+ per_Black+ per_Hispanic+
             per_ELL + per_SWD +
             Transfer,
           data = train, weight =  Cohort_Total,
           family = binomial(link = "logit"))
summary(mod)

#Model with Poverty
mod_poverty <- glm(Grad_Rate ~ per_Female+ 
                     per_Asian+ per_Black+ per_Hispanic + 
                     per_ELL + per_SWD +
                     per_Poverty + 
                     Transfer,
                   data = train, weight =  Cohort_Total,
                   family = binomial(link = "logit"))
summary(mod_poverty)

#Model with  Crime Data 
mod_crimes <- glm(Grad_Rate ~ per_Female + 
                    per_Asian+ per_Black+ per_Hispanic +
                    per_ELL + per_SWD +
                    per_Poverty +
                    Property_Crime_Rate + Violent_Rate + Behavior_Rate +
                    Transfer, 
                  data = train, weight =  Cohort_Total,
                  family = binomial(link = "logit"))
summary(mod_crimes)
anova(mod, mod_poverty,  test="Chisq")

#Fixed Effects Model with District
mod_district <- glm(Grad_Rate ~ District +per_Female + 
                      per_Asian+ per_Black+ per_Hispanic +
                      per_ELL + per_SWD + 
                      per_Poverty +
                      Property_Crime_Rate + Violent_Rate + Behavior_Rate +
                      Transfer,
                    data = train, weight =  Cohort_Total,
                    family = binomial(link = "logit"))
summary(mod_district)
coef(mod_district) %>%  exp()


mod_diff <- glm(cbind(Grad_Total, Cohort_Total - Grad_Total) ~ per_Female + 
                      per_Asian+ per_Black+ per_Hispanic +
                      per_ELL + per_SWD + 
                      per_Poverty +
                      Property_Crime_Rate + Violent_Rate + Behavior_Rate +
                      Transfer,
                    data = train,
                    family = binomial(link = "logit"))
summary(mod_diff)
summary(mod_crimes)


anova(mod_crimes, mod_district,  test='Chisq')

#Full Model Without Poverty Variable
mod_drop_Poverty <- glm(Grad_Rate ~ District +per_Female + 
                      per_Asian+ per_Black+ per_Hispanic +
                      per_ELL + per_SWD + 
                      Property_Crime_Rate + Violent_Rate + Behavior_Rate +
                      Transfer,
                    data = train, weight =  Cohort_Total,
                    family = binomial(link = "logit"))
summary(mod_drop_Poverty)


anova(mod_drop_Poverty, mod_district,  test='Chisq')

#Plot Residuals/ 
plot(mod_district)

#Look at Outliers
train %>% slice(c(10, 126, 138, 182, 299, 347)) %>%  as.data.frame()

##Lasso regression


y <- as.matrix(cbind(train$Cohort_Total - train$Grad_Total, train$Grad_Total))
x <- model.matrix(~District + per_Female + 
                    per_Asian+ per_Black + per_Hispanic +
                    per_ELL + per_SWD + 
                    per_Poverty + Major_Crime_Rate + 
                    Transfer-1, data=train)
test_matrix <- model.matrix(~District + per_Female + 
                              per_Asian+ per_Black + per_Hispanic +
                              per_ELL + per_SWD + 
                              per_Poverty + Major_Crime_Rate + 
                              Transfer-1, data=test)
lasso.reg <- glmnet(x, y, family='binomial', alpha=1, lambda=0.01)
coef(lasso.reg)


#####Generating predictions from model with district#######


#Generate Predictions
test$predicted.prob.log <- predict(mod_district, newdata = test, type='response')  

#Calculate MSE 
sqrt(mean((test$predicted.prob.log - test$Grad_Rate)^2, na.rm = T))*100

#Plot Observed Grad Rate OVER Predicted Grad Rate
plot(test$predicted.prob.log, test$Grad_Rate)
abline(a = 0, b = 1, lty = "dashed", col = "red")

#### Predict lasso regression ####
test$predicted.prob.lasso <- predict(lasso.reg, newx = test_matrix, type='response')  

#Plot Observed Grad Rate OVER Predicted Grad Rate
plot(test$predicted.prob.lasso, test$Grad_Rate)
abline(a = 0, b = 1, lty = "dashed", col = "red")


#Calculate MSE 
sqrt(mean((test$predicted.prob.lasso- test$Grad_Rate)^2, na.rm = T))*100


#### Build Grouped Logistic Model ####
# 
# grad_2011 <- graduation %>%  filter(Cohort_Year == 2011) 
# 
# grad_2011 <- grad_2011[complete.cases(grad_2011) , ]
# 
# 
# mod <- glm(Grad_Rate ~  per_Female+ per_SWD + 
#              per_Asian+ per_Black+ per_Hispanic,    
#     data = grad_2011, weight =  Cohort_Total, 
#     family = binomial(link = "logit")) 
# summary(mod)
# 
# mod_check <- glm(cbind(Grad_Total, Cohort_Total - Grad_Total) ~
#                    per_Female+ per_SWD + per_Asian+ per_Black+ per_Hispanic,
#                  data = grad_2011,
#                  family = binomial(link = "logit")) 
# summary(mod_check)
# 
# 
# 
# 
# pred_2011 <- predict(mod, type = "response")
# #Check that Predictions are between 0  & 1
# range(pred_2011 )
# 
# #Plot Actual Graduation Rate Over Predicted Rate
# plot(pred_2011 , grad_2011$Grad_Rate, 
#      xlim = c(0,1), 
#      col = c("orange", "green")[grad_2011$Transfer + 1] )
# abline(a =0, b= 1, col = "red", lty = "dashed" )
# abline(h= .35, col = "blue", lty = "dashed")
# 
# 
# #Model 2: Add Transfer as Indicator Variable 
# mod2 <- glm(Grad_Rate ~  per_Female+ per_SWD + 
#              per_Asian+ per_Black+ per_Hispanic + Transfer,    
#            data = grad_2011, weight =  Cohort_Total, 
#            family = binomial(link = "logit")) 
# summary(mod2)
# coef(mod2)%>%  exp()
# coef(mod2) %>%  exp() - 1
# 
# #Look at Grad Rates of SWD students in 2011
# plot(grad_2011$Grad_Rate~ grad_2011$per_SWD,
#      col = c("blue", "red")[grad_2011$Transfer + 1])
# abline(h = mean(grad_2011$Grad_Rate), lty = "dashed", col = "orange", lwd = 2)
# abline(v = mean(grad_2011$per_SWD), lty = "dashed", col = "orange", lwd = 2)
# 
# #Look at Grad Rates of Black students in 2011
# plot(grad_2011$Grad_Rate~ grad_2011$per_Black, 
#      col = c("blue", "red")[grad_2011$Transfer + 1])
# abline(h = mean(grad_2011$Grad_Rate), lty = "dashed", col = "orange", lwd = 2)
# abline(v = mean(grad_2011$per_Black), lty = "dashed", col = "orange", lwd = 2)
# 
# plot(grad_2011$Grad_Rate~ grad_2011$per_ELL )
# boxplot(grad_2011$Grad_Rate~ grad_2011$Borough )
# plot(grad_2011$Grad_Rate~ grad_2011$Total_Enrollment )
# plot(grad_2011$Grad_Rate~ factor(grad_2011$Transfer))
# 
# 
# 
# pred_2011_mod_2 <- predict(mod2, type = "response")
# 
# #Plot Actual Graduation Rate Over Predicted Rate
# plot(pred_2011_mod_2, grad_2011$Grad_Rate, 
#      xlim = c(0,1), 
#      col = c("orange", "green")[grad_2011$Transfer + 1] )
# abline(a =0, b= 1, col = "red", lty = "dashed" )
# abline(h= .35, col = "blue", lty = "dashed")
# 
# range(pred_2011_mod_2)
# summary(mod2)
# 
# anova(mod2, mod1, test = "Chisq")
# 
# tapply(grad_2011$Grad_Rate, grad_2011$Transfer, mean)
# (0.2423256 / (1 - 0.2423256 )) / ( 0.7470154  / (1 - .7470154 ))
# mod3 <- glm(Grad_Rate ~ Transfer,    
#             data = grad_2011, weight =  Cohort_Total, 
#             family = binomial(link = "logit")) 
# summary(mod3)
# coef(mod3) %>%  exp()
# 
# 
# 
# #NOTE: These schools that do not fit model all have graduation rates under 35
# #Google Search indicates they many are "transfer" schools 
# #"Transfer" schools are specifically for students at risk of dropping out
# #We should be able to solve this by adding an indicator variable
# #They seem to have the same slope but a different intercept
# grad_2011 %>%  filter(Grad_Rate < 35) %>%  View()
# 
# #Next Steps:
# ## (Maybe?) Add Neighborhood Violent Crime Data
# ## Add Class Size Data or Teacher/Pupil Ratio
# ## (Maybe?) Find school funding data
# ## (Maybe?) Add district as variable (may need to be MLM model )
# 
# 
# 
# 
