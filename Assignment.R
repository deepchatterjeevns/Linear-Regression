install.packages("Metrics")
library(Metrics)
library(ggplot2)
library(MASS)
library(car)
library(stringr)
library(dplyr)

#Loading the dataset 
car_data <- read.csv("CarPrice_Assignment.csv",header = TRUE,stringsAsFactors = TRUE)
View(car_data)
str(car_data)

#Removal of car ID column which is considered as insignificant to modelling
car_data <- car_data[,-1]

#Since the dataset has low number of observation we are splitting test and train dataset
#we will be keeping the dataset as such
#Checking for missing values
which(is.na(car_data))

#Extracting the car company name
#Merging the number of cars with correct spelling
car_data$CarName <- str_replace(car_data$CarName,"\\s+.*","")
car_data$CarName <- ifelse((car_data$CarName == 'vokswagen' | car_data$CarName == 'vw'),'volkswagen',car_data$CarName)
car_data$CarName <- ifelse((car_data$CarName == 'toyouta'),'toyota',car_data$CarName)
car_data$CarName <- ifelse((car_data$CarName == 'Nissan'),'nissan',car_data$CarName)
car_data$CarName <- ifelse((car_data$CarName == 'porcshce'),'porsche',car_data$CarName)
car_data$CarName <- ifelse((car_data$CarName == 'maxda'),'mazda',car_data$CarName)

#Checking for CarName
table(car_data$CarName)
summary(car_data)
str(car_data)

############# PLOTTING BEGINS EXPLORATORY DATA ANALYSIS ##################################
#Plotting the frequency of car names 
#To know the popularity of the car brand
#This Plot shows us the that only toyota,subaru,audi manfacture 4wd
#Only Toyota manfacture all three types of vehicle
ggplot(car_data,aes(x=CarName))+geom_bar(aes(fill = factor(drivewheel)))+
  xlab("CarName") + ylab("Frequency")+ggtitle(paste("Plot of ","CarName Vs Drivewheel"))+geom_text(stat='count',aes(label=..count..),hjust=0)+coord_flip()


#To know which engine type fulesystem is used in engine
#The below plot gives OHC engine type is the most convient enginetype
#As it uses most kind of fuelsystem
ggplot(car_data_new,aes(x=enginetype,fill = fuelsystem))+geom_bar()+
  xlab("Enginetype") + ylab("Frequency")+ggtitle(paste("Plot of ","Enginetype Vs Fuelsystem"))


#OHC engine ia also prefered by most car companies
ggplot(car_data_new,aes(x=CarName,fill = enginetype))+geom_bar()+
  xlab("Enginetype") + ylab("Frequency")+ggtitle(paste("Plot of ","CarName Vs Enginetype"))+coord_flip()


#To Find which Fuelsystem gives peakrpm
#Fuelsystem mpfi gives maximum rpm
ggplot(car_data,aes(factor(fuelsystem),peakrpm))+geom_point()+
  xlab("Peakrpm") + ylab("Fuelsystem")+ggtitle(paste("Plot of ","Peakrpm Vs Fuelsystem"))+coord_flip()


#This plot shows that weigth of the car increases with increases in weight of engine
ggplot(car_data,aes(curbweight,enginesize))+geom_point()+
  xlab("curbweight") + ylab("enginesize")+ggtitle(paste("Plot of ","curbweight Vs enginesize"))


#This Plot shows that the lower the horsepower will higher mileage in highway
ggplot(car_data,aes(x=horsepower,y=highwaympg))+geom_point()+
  xlab("horsepower") + ylab("highwaympg")+ggtitle(paste("Plot of ","horsepower Vs highwaympg"))


#This Plot shows that porsche uses maximum horsepower in its engine
#higer the horsepower lower will be the mileage and higher will be engine weight
#This evident from the previous plots
ggplot(car_data,aes(x = CarName,y = horsepower))+geom_point()+coord_flip()+
  xlab("CarName") + ylab("horsepower")+ggtitle(paste("Plot of ","CarName Vs horsepower"))

#This Plot gives us the most of car makers perfers hatchback and sedan
#Some car companies manufacture only sedan
ggplot(car_data,aes(x = CarName,fill = carbody))+geom_bar()+coord_flip()+
  xlab("CarName") + ylab("carbody")+ggtitle(paste("Plot of ","CarName Vs carbody"))

#This Plot that except 4wd, the remaining fwd and rwd uses all types of fuelsystem
ggplot(car_data,aes(x=drivewheel,fill = factor(fuelsystem)))+geom_bar()+
  xlab("drivewheel") + ylab("fuelsystem")+ggtitle(paste("Plot of ","drivewheel Vs fuelsystem"))

#This Plot shows us that the cars with 4 cylinder would be best for cars
#Since it gives wide range of peakrpm
ggplot(car_data,aes(peakrpm,cylindernumber))+geom_point()+
  xlab("peakrpm") + ylab("cylindernumber")+ggtitle(paste("Plot of ","peakrpm Vs cylindernumber"))

#This Plot that cars with 4 cylinder maintains the optimum engine weight
#The car with 2 cylinder shows high peakrpm it might be due to reduced engine weight
#The car with 4 cylinder shows Average peakrpm it might be due to high engine weight and high thrust
ggplot(car_data,aes(x=enginesize,y=cylindernumber))+geom_point()+
  xlab("enginesize") + ylab("cylindernumber")+ggtitle(paste("Plot of ","enginesize Vs cylindernumber"))

############# PLOTTING ENDS ##################################


#########Assinging the level of the categorical variable with numbers##########
#fueltype,enginelocation,aspiration 2 level categorical variable

levels(car_data[,"aspiration"]) <- c(0,1)
car_data[,"aspiration"] <- as.numeric(levels(car_data[,"aspiration"]))[car_data[,"aspiration"]]


levels(car_data[,"fueltype"]) <- c(0,1)
car_data[,"fueltype"] <- as.numeric(levels(car_data[,"fueltype"]))[car_data[,"fueltype"]]

levels(car_data[,"enginelocation"]) <- c(0,1)
car_data[,"enginelocation"] <- as.numeric(levels(car_data[,"enginelocation"]))[car_data[,"enginelocation"]]

######### ASSIGING OF LEVELS COMPELETE ###################

#Replacing the cyclindernumber column from character to number
car_data$cylindernumber <- as.character(car_data$cylindernumber)
car_data$cylindernumber <- ifelse(car_data$cylindernumber == "two",2,
                                  ifelse(car_data$cylindernumber == "three",3,
                                  ifelse(car_data$cylindernumber == "four",4,
                                  ifelse(car_data$cylindernumber == "five",5,
                                  ifelse(car_data$cylindernumber == "six",6,
                                  ifelse(car_data$cylindernumber == "eight",8,12
                                  ))))))

#Replacing the doornumber column to number
car_data$doornumber <- ifelse(car_data$doornumber == "four",4,2)

#Replacing the carname with its frequency count
car_data$CarName <- ifelse(car_data$CarName == "alfa-romero",3,ifelse(car_data$CarName == "audi",7,
                          ifelse(car_data$CarName == "bmw",8,ifelse(car_data$CarName == "buick",8,ifelse(car_data$CarName == "chevrolet",3,
                            ifelse(car_data$CarName == "dodge",9,ifelse(car_data$CarName == "honda",13,
                                                                        ifelse(car_data$CarName == "isuzu",4,car_data$CarName))))))))

car_data$CarName <- ifelse(car_data$CarName == "jaguar",3,ifelse(car_data$CarName == "mazda",17,
                        ifelse(car_data$CarName == "mercury",1,ifelse(car_data$CarName == "mitsubishi",13,
                          ifelse(car_data$CarName == "nissan",18,ifelse(car_data$CarName == "peugeot",11,
                            ifelse(car_data$CarName == "plymouth",7,ifelse(car_data$CarName == "porsche",5,car_data$CarName))))))))

car_data$CarName <- ifelse(car_data$CarName == "renault",2,ifelse(car_data$CarName == "saab",6,
                      ifelse(car_data$CarName == "subaru",12,ifelse(car_data$CarName == "toyota",32,
                       ifelse(car_data$CarName == "volkswagen",12,11)))))
                                                                                                                                                             
View(car_data)
summary(car_data)

#Creation of dummy variable for carbody
dummy_carbody <- data.frame(model.matrix(~carbody,data = car_data))
View(dummy_carbody)
dummy_carbody <- dummy_carbody[,-1]
car_data_new <- cbind(car_data[,-6],dummy_carbody) 
View(car_data_new)

#Creation of dummy variable for drivewheel
dummy_drivewheel <- data.frame(model.matrix(~drivewheel,data = car_data))
View(dummy_drivewheel)
dummy_drivewheel <- dummy_drivewheel[,-1]
car_data_new <- cbind(car_data_new[,-6],dummy_drivewheel) 
View(car_data_new)


#Creation of dummy variable for enginetype
dummy_enginetype <- data.frame(model.matrix(~enginetype,data = car_data))
View(dummy_enginetype)
dummy_enginetype <- dummy_enginetype[,-1]
car_data_new <- cbind(car_data_new[,-12],dummy_enginetype) 
View(car_data_new)

#Creation of dummy variable for fuelsystem
dummy_fuelsystem <- data.frame(model.matrix(~fuelsystem,data = car_data))
View(dummy_fuelsystem)
dummy_fuelsystem <- dummy_fuelsystem[,-1]
car_data_new <- cbind(car_data_new[,-14],dummy_fuelsystem) 
View(car_data_new)

#Correaltion Matrix of numerical variables

b <- cor(car_data_new)
write.csv(b,file="correlation.csv")

#Model Building
#model 1 with all independent vaiables
model_1 <- lm(price ~ .,data = car_data_new)
summary(model_1)
#stepAIC calculation to reduce considerable amount of Independent variables
aic_calc <- stepAIC(model_1,direction = "both")

#model 2 after ruuning AIC calculation 
#The variables are taken from the last iteration of the AIC values
model_2 <- lm(price ~ CarName + fueltype + aspiration + enginelocation + carwidth + 
                carheight + curbweight + cylindernumber + enginesize + boreratio + 
                stroke + compressionratio + peakrpm + highwaympg + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypel + enginetypeohc + enginetypeohcv + enginetyperotor + 
                fuelsystemspdi,data = car_data_new)

summary(model_2)
#Checking the VIF for the model
vif(model_2)

#model 3 after checking VIF calculation
# Removing curbweight and cylindernumber
model_3 <- lm(price ~ CarName + fueltype + aspiration + enginelocation + carwidth + 
                carheight  + enginesize + boreratio + 
                stroke + compressionratio + peakrpm + highwaympg + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypel + enginetypeohc + enginetypeohcv + enginetyperotor + 
                fuelsystemspdi,data = car_data_new)
summary(model_3)
#Checking the VIF for model 3
vif(model_3)

#model 4 after checking VIF calculation of model 3
# Removing fueltype and enginetypeohcv
model_4 <- lm(price ~ CarName  + aspiration + enginelocation + carwidth + 
                carheight  + enginesize + boreratio + 
                stroke + compressionratio + peakrpm + highwaympg + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypel + enginetypeohc  + enginetyperotor + 
                fuelsystemspdi,data = car_data_new)

summary(model_4)
#Checking the VIF for model 4
vif(model_4)

#model 5 after checking VIF calculation of model 4
#Removing compressionration,fuelsystemspdi,enginetypl,highwaympg due to insignificance
model_5 <- lm(price ~ CarName  + aspiration + enginelocation + carwidth + 
                carheight  + enginesize + boreratio + 
                stroke  + peakrpm  + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                 enginetypeohc  + enginetyperotor  
                ,data = car_data_new)
summary(model_5)
#Checking the VIF for model 5
vif(model_5)

model_6 <- lm(price ~ aspiration + enginelocation + carwidth + 
                carheight  + enginesize + boreratio + 
                stroke  + peakrpm  + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypeohc  + enginetyperotor  
              ,data = car_data_new)

summary(model_6)
vif(model_6)

#Assesing the stablility of the model
#on Plotting the model 6 we could get 4 different graphs
#1.the plot shows the distribution of residuals (errors) vs fitted values shows the linear effect
#2.Normall Q-Q Plot shows the that there is no autocorrelation
#3.Scale Plot shows the no heteroskedacity pattern
#4. This plot shows that is very megere effect of outliers
#All four graph also proves the assumption of linear regression
plot(model_6)

#RMSE values for the model for model6
rmse(car_data_new$price,car_data_new$predicted_price)



#Prediction of carprice
car_data_new$predicted_price <- predict(model_6,car_data_new)

#Calculation of the error terms
car_data_new$error <- car_data_new$price - car_data_new$predicted_price


#since we have converted the carnames into their frequency of their occurence
#below is the plot of error vs carname 
ggplot(car_data_new, aes(CarName,error))+geom_point()+coord_flip()+
  ggtitle(paste("Plot of ","error Vs carname"))


#since we have converted the carnames into their frequency of their occurence
#below is the plot for Actual Vs Predicted values
#we clearly explain variance of 89.6%
ggplot(car_data_new, aes(CarName, price)) + geom_line(aes(colour = "blue" )) +
  geom_line(aes(x=CarName, y=predicted_price, colour="steelblue"))

#Below Commands are to represent the graphs with exact carnames
car_data_1 <- read.csv("CarPrice_Assignment.csv",header = TRUE,stringsAsFactors = TRUE)

car_data_1$CarName <- str_replace(car_data_1$CarName,"\\s+.*","")
car_data_1$CarName <- ifelse((car_data_1$CarName == 'vokswagen' | car_data_1$CarName == 'vw'),'volkswagen',car_data_1$CarName)
car_data_1$CarName <- ifelse((car_data_1$CarName == 'toyouta'),'toyota',car_data_1$CarName)
car_data_1$CarName <- ifelse((car_data_1$CarName == 'Nissan'),'nissan',car_data_1$CarName)
car_data_1$CarName <- ifelse((car_data_1$CarName == 'porcshce'),'porsche',car_data_1$CarName)
car_data_1$CarName <- ifelse((car_data_1$CarName == 'maxda'),'mazda',car_data_1$CarName)

car_data_2 <- cbind(car_data_1,car_data_new[,c(41,42)])

#below is the plot of error vs carname 
ggplot(car_data_2, aes(CarName,error))+geom_point()+coord_flip()+
  ggtitle(paste("Plot of ","error Vs carname"))


#below is the plot for Actual Vs Predicted values
#we clearly explain variance of 89.6%
ggplot(car_data_new, aes(CarName, price)) + geom_line(aes(colour = "blue" )) +
  geom_line(aes(x=CarName, y=predicted_price, colour="steelblue"))


#Interpretation of results
# The main driver variables for price prediction are
# aspiration like standrad engine and turbo engine 
# enginelocation like front or rear, but front are most prffered
# car outer look aspects like - carwidth,carheight
# carengine aspects are focussed - enginesize,peakrpm
#drivewheelrwd - gives more stabliltiy 
#enginetype also an important driver because of mileage difference - enginetyperotor,enginetyperotor 


