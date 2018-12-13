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
library(randomForest)
library(rgeos)
library(rgdal)
library(lubridate)

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


#Calculate Number of Schools with Suppressed Graduation Info
sum(graduation$Total_Cohort  == 's')
sum(graduation[graduation$Cohort_Year==2013, ]$Total_Grads  == 's')
sum(graduation[graduation$Cohort_Year==2013, ]$Total_Grads_per_of_cohort == 's')
sum(graduation[graduation$Cohort_Year==2013, ]$Total_Grads_per_of_cohort == 's' &
      graduation[graduation$Cohort_Year==2013, ]$Total_Grads_per_of_cohort != 's')
# 8 schools were missing 

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




#Loading in arrests dataset:
nyc_arrests<- read_csv("Data/Input/NYPD_Arrests_Data__Historic_.csv")

#Filtering for year 2013: 
nyc_arrests$ARREST_DATE= as.Date(nyc_arrests$ARREST_DATE, format = "%m/%d/%Y")
nyc_arrests$ARREST_YEAR= year(nyc_arrests$ARREST_DATE)
nyc_arrests= nyc_arrests %>% filter(ARREST_YEAR==2013)  


#No. of unique precincts are 77:
length(unique(nyc_arrests$ARREST_PRECINCT))




#Loading in precinct pop dataset:
precinct_pop<- read_csv("Data/Input/NYC_Blocks_2010CensusData_Plus_Precincts (1).csv")


#Calculating the population for each precinct: 
precinct_pop= precinct_pop %>% group_by(precinct) %>% summarise(Pop= sum(P0010001))


#Merging the nyc_arrests and precinct_pop dataset:
crime_precinct_pop= merge(nyc_arrests, precinct_pop, by.x='ARREST_PRECINCT', by.y='precinct')



#Checking for missing precincts: It seems like precinct 121 is missing from the stops_pop dataset..
setdiff(nyc_arrests$ARREST_PRECINCT, precinct_pop$precinct)


#Calculating the arrest rate for each precinct:
crime_precinct_pop= crime_precinct_pop %>% group_by(ARREST_PRECINCT, Pop) %>% count(n=n()) %>% mutate(nn/Pop)



View(crime_precinct_pop)


#Checking for missing lat/long values: (None missing)
colSums(is.na(grad_2013))



#Loading the shapefile for precincts:
filepath= "Data/Input/Police Precincts 4"
precinct_shape= readOGR(dsn=filepath, layer="geo_export_eb0eaa6c-3c5d-43fb-9f53-0834474da7e8")


#Extracting coordinates and converting into a SpatialPointsDataFrame object:
coords= data.frame(grad_2013$Longitude, grad_2013$Latitude)
coords_df <- SpatialPointsDataFrame(coords, data=grad_2013,  proj4string = CRS("+proj=longlat +ellps=WGS84 +no_defs"))



#Overalying coordinates from the graduation dataset onto the precinct shapefile to get matches: 
pts=over(coords_df, precinct_shape[,"precinct"])
coords_df$precinct= pts$precinct

#Merge the arrest rate features with grad_2013:
grad_2013= merge(crime_precinct_pop, coords_df@data, by.x='ARREST_PRECINCT', by.y='precinct')
grad_2013$n=NULL
grad_2013= grad_2013 %>% dplyr::rename(Population=Pop, Number_of_Arrests=nn, Arrest_Rate=`nn/Pop`)



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
set.seed(1994)
train <- grad_2013 %>% sample_frac(0.5) 
test <- grad_2013 %>% filter(!(DBN %in% train$DBN))

#### Fit: Grouped Binomial Regression Model ####
mod_arrests <- glm(Grad_Rate ~ District + per_Female + 
                     per_Asian+ per_Black+ per_Hispanic +
                     per_ELL + per_SWD +
                     per_Poverty +
                     Property_Crime_Rate + Violent_Rate + Behavior_Rate +
                     Transfer+Arrest_Rate, 
                   data = train, weight =  Cohort_Total,
                   family = binomial(link = "logit"))
summary(mod_arrests)

#Create Table for Coefficients from Final Regression Model
final_mod_coefs <- tidy(mod_arrests)
final_mod_coefs <- final_mod_coefs %>%  
                      filter(grepl(patter = 'District', final_mod_coefs$term)==F)
final_mod_coefs %>%  as.data.frame()

#### Fit: Lasso Model ####


#Extract Y Values for Training Data
## Y Values are stored in2 Columns: Number graduated, number not graduate
## NOTE: This is equivalent to the proportions we used for regular regression model.  
## NOTE: However, proportions did not work with glmnet lasso algorithms
y_train <- as.matrix(cbind(train$Cohort_Total - train$Grad_Total, train$Grad_Total))

#Create Design Matrix for Training Data
x_train <- model.matrix(~District + per_Female + 
                    per_Asian+ per_Black + per_Hispanic +
                    per_ELL + per_SWD + 
                    per_Poverty + 
                    Property_Crime_Rate + Violent_Rate + Behavior_Rate +
                    + Arrest_Rate+
                    Transfer, data=train)[, -1]

#Create Design Matrix for Test Data
x_test <- model.matrix(~District + per_Female + 
                         per_Asian+ per_Black + per_Hispanic +
                         per_ELL + per_SWD + 
                         per_Poverty + 
                         Property_Crime_Rate + Violent_Rate + Behavior_Rate +
                         + Arrest_Rate+
                         Transfer, data=test)[, -1]


#Fit Lasso Model
lasso.reg <- glmnet(x = x_train, y = y_train, family='binomial', alpha=1, lambda=0.01)

#Explore Coefficients
coef(lasso.reg)

#Create Table for LASSO coefficients
tidy(lasso.reg) %>%  as.data.frame()

#### Fit: Random Forest #####
mod_forest <- randomForest(Grad_Rate ~ District +per_Female + 
                             per_Asian+ per_Black+ per_Hispanic +
                             per_ELL + per_SWD + 
                             per_Poverty +
                             Property_Crime_Rate + Violent_Rate + Behavior_Rate +
                             Transfer+Arrest_Rate,
                           data = train, weight =  Cohort_Total, ntree = 1000)

#Show Important Features 
mod_forest_names <- importance(mod_forest) %>% rownames()
node_purity <- importance(mod_forest) %>% as.numeric() 
import_coefs_rf <- data.frame(Features = mod_forest_names,
                         IncNodePurity = node_purity ) %>% 
                         arrange(desc(IncNodePurity))
print(import_coefs_rf)

#### Training Performance: Grouped Logistic Model ####
par(mfrow=c(1,2))

#Calculate Prediction Group logistic Regression Model 
train$predicted.prob.log <- predict(mod_arrests, type = 'response')

#Calculate RMSE 
log_train_rmse <- sqrt(mean((train$predicted.prob.log - train$Grad_Rate)^2))*100

#Plot Logistic Regression  Model Predictions Versus 
plot(train$predicted.prob.log, train$Grad_Rate, 
     xlab='Predicted Graduation Rate', 
     ylab='Observed Graduation Rate',
     xlim = 0:1,
     ylim = 0:1,
     main = 'Logistic Model: Train')
abline(a= 0, b = 1, lty = 'dashed', col = 'red')


#### Test Performance: Logistic Regression  ####

#Generate Predictions
test$predicted.prob.log <- predict(mod_arrests, newdata = test, type='response')  

#Calculate RMSE 
log_test_rmse <- sqrt(mean((test$predicted.prob.log - test$Grad_Rate)^2))*100

#Plot Observed Grad Rate OVER Predicted Grad Rate
plot(test$predicted.prob.log, test$Grad_Rate, 
     xlab='Predicted Graduation Rate', 
     ylab='Observed Graduation Rate',
     xlim = 0:1,
     ylim = 0:1,
     main = 'Logistic Model: Test')
abline(a = 0, b = 1, lty = "dashed", col = "red")



#### Training Performance: Lasso Model ####

#Predict Grad Rates for Training Data
train$predicted.prob.lasso <- predict(lasso.reg,  newx = x_train, type='response')

#Calculate RMSE
lasso_train_rmse <- sqrt(mean((train$predicted.prob.lasso- train$Grad_Rate)^2))*100

#Plot Predicted Predictions Versus Actual Grad Rates 
plot(train$predicted.prob.lasso, train$Grad_Rate, 
     xlab='Predicted Graduation Rate', 
     ylab='Observed Graduation Rate',
     xlim = 0:1,
     ylim = 0:1,
     main = 'Lasso Model: Train')
abline(a = 0, b = 1, lty = "dashed", col = "red")

#### Test Performance: Lasso Regression ####

#Predict Test Data
test$predicted.prob.lasso <- predict(lasso.reg, newx = x_test, type='response')  

#Calculate Test RMSE
lasso_test_rmse <- sqrt(mean((test$predicted.prob.lasso- test$Grad_Rate)^2))*100

#Plot Observed Grad Rate OVER Predicted Grad Rate
plot(test$predicted.prob.lasso, test$Grad_Rate, 
     xlab='Predicted Graduation Rate', 
     ylab='Observed Graduation Rate',
     xlim = 0:1,
     ylim = 0:1,
     main = 'Lasso Model: Test')
abline(a = 0, b = 1, lty = "dashed", col = "red")

#### Training Performance: Random Forest #####

#Predict Grad Rates for Training Data
train$predicted.prob.forest <- predict(mod_forest)

#Calculate RMSE
forest_train_rmse <- sqrt(mean((train$predicted.prob.forest- train$Grad_Rate)^2))*100

#Plot Predicted Predictions Versus Actual Grad Rates 
plot(train$predicted.prob.forest, train$Grad_Rate, 
     xlab='Predicted Graduation Rate', 
     ylab='Observed Graduation Rate',
     xlim = 0:1,
     ylim = 0:1,
     main = 'Random Forest: Train')
abline(a = 0, b = 1, lty = "dashed", col = "red")

#### Test Performance: Random Forest ####

#Predict Grad Rates in test data
test$predicted.prob.forest <- predict(mod_forest, newdata = test)

#Calculate RMSE 
forest_test_rmse <- sqrt(mean((test$predicted.prob.forest- test$Grad_Rate)^2))*100

#Plot Prediction
plot(test$predicted.prob.forest, test$Grad_Rate, 
     xlab='Predicted Graduation Rate', 
     ylab='Observed Graduation Rate',
     xlim = 0:1,
     ylim = 0:1,
     main = 'Random Forest: Test')
abline(a = 0, b = 1, lty = "dashed", col = "red")

#### Compare Performances: Bar Plots ####

#Compare  Performance on TRAINING data 
compare_rmse_train <- data.frame(
                        rmse =c(forest_train_rmse, lasso_train_rmse, log_train_rmse),
                        model = c('Random Forest', "Lasso", 'Logistic') )
compare_rmse_train$order <- sort.int(compare_rmse_train$rmse, index.return =T)$ix


color_vec <- c("green", "yellow", "red")
color_vec_train <- color_vec[compare_rmse_train$order]


par(mfrow=c(1,2))

#PLOT Training data
barplot(compare_rmse_train$rmse[compare_rmse_train$order], 
        names.arg= compare_rmse_train$model[compare_rmse_train$order], 
        xlab="Model RMSE on TRAINING data", 
        ylab = 'RMSE',
        col = color_vec[compare_rmse_train$order],
        ylim = c(0,16))


#Compare  Performance on TRAINING data 
compare_rmse_test <- data.frame(
                      rmse =c(forest_test_rmse, lasso_test_rmse, log_test_rmse),
                      model = c('Random Forest', "Lasso", 'Logistic') )
compare_rmse_test$order <- sort.int(compare_rmse_test$rmse, index.return =T)$ix


#PLOT Performance on TEST data 
barplot(compare_rmse_test$rmse[compare_rmse_test$order], 
        names.arg = compare_rmse_test$model[compare_rmse_test$order], 
        xlab = "Model RMSE on TEST data",
        ylab = 'RMSE',
        col = color_vec[compare_rmse_test$order],
        ylim = c(0,16))

#Compare Actual RMSE values 
compare_rmse_train
compare_rmse_test



#### Appendix: Model Exploration ####

# #Model with Demographic, ELL and SWD
# mod <- glm(Grad_Rate ~ per_Female + 
#              per_Asian+ per_Black+ per_Hispanic+
#              per_ELL + per_SWD +
#              Transfer,
#            data = train, weight =  Cohort_Total,
#            family = binomial(link = "logit"))
# summary(mod)
# 
# #Model with Poverty
# mod_poverty <- glm(Grad_Rate ~ per_Female+ 
#                      per_Asian+ per_Black+ per_Hispanic + 
#                      per_ELL + per_SWD +
#                      per_Poverty + 
#                      Transfer,
#                    data = train, weight =  Cohort_Total,
#                    family = binomial(link = "logit"))
# summary(mod_poverty)
# 
# #Model with  Crime Data 
# mod_crimes <- glm(Grad_Rate ~ per_Female + 
#                     per_Asian+ per_Black+ per_Hispanic +
#                     per_ELL + per_SWD +
#                     per_Poverty +
#                     Property_Crime_Rate + Violent_Rate + Behavior_Rate +
#                     Transfer, 
#                   data = train, weight =  Cohort_Total,
#                   family = binomial(link = "logit"))
# summary(mod_crimes)
# anova(mod, mod_poverty,  test="Chisq")
# 
# 
# 
# #Fixed Effects Model with District
# mod_district <- glm(Grad_Rate ~ District +per_Female + 
#                       per_Asian+ per_Black+ per_Hispanic +
#                       per_ELL + per_SWD + 
#                       per_Poverty +
#                       Property_Crime_Rate + Violent_Rate + Behavior_Rate + 
#                       Transfer,
#                     data = train, weight =  Cohort_Total,
#                     family = binomial(link = "logit"))
# summary(mod_district)
# 
# 
# #Model with arrest data
# mod_arrests <- glm(Grad_Rate ~ District + per_Female + 
#                      per_Asian+ per_Black+ per_Hispanic +
#                      per_ELL + per_SWD +
#                      per_Poverty +
#                      Property_Crime_Rate + Violent_Rate + Behavior_Rate +
#                      Transfer+Arrest_Rate, 
#                    data = train, weight =  Cohort_Total,
#                    family = binomial(link = "logit"))
# summary(mod_arrests)
# 
# anova(mod_district, mod_arrests, test = 'Chisq')
# 

