setwd("C:/Users/MANJULA/datascience/linear regression/assignment")
rm(list=ls())
# Load essential libraries
library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)
library(reshape2)
install.packages("MASS")
library(MASS)
install.packages("car")
library("car")
carprices_orig <- read.csv("CarPrice_Assignment.csv",stringsAsFactors = F)
str(carprices_orig)
###Data Preparation
###Checking for NA in the original dataframe
sum(is.na(carprices_orig)) ### 0
###Checking for duplicated rows in the original dataframe
any(duplicated(carprices_orig))
###Analyzing each column
### Symboling is an interger with +3 least safe and -2 safe ###keeping it as a numeric for now as it has an ordered categorical

###Splitting the CarName into CarCompanyName and CarModelName
###Splitting at the first instance of " " into 2 columns
carprices <- separate(carprices_orig,CarName,into = c("CarCompanyName","CarModelName"),sep=" ",extra="merge",fill="right")
###Row number 142 and 139 have no CarModelName, hence NA values are filled
###Some car names have wrong spelling Eg (volkswagen,vw and vokswagen) (toyota,toyouta)
###(porsche,porcshce) (nissan,Nissan) (mazda,maxda)
carprices$CarCompanyName <- str_replace_all(carprices$CarCompanyName,"vw","volkswagen")
carprices$CarCompanyName <- str_replace_all(carprices$CarCompanyName,"vokswagen","volkswagen")
carprices$CarCompanyName <- str_replace_all(carprices$CarCompanyName,"toyouta","toyota")
carprices$CarCompanyName <- str_replace_all(carprices$CarCompanyName,"porcshce","porsche")
carprices$CarCompanyName <- str_replace_all(carprices$CarCompanyName,"Nissan","nissan")
carprices$CarCompanyName <- str_replace_all(carprices$CarCompanyName,"maxda","mazda")
###Creating variables for each company
dummy_1 <- data.frame(model.matrix( ~CarCompanyName, data = carprices))
dummy_1 <- dummy_1[,-1]
carprices <-cbind(carprices[-3],dummy_1)
###assigning 1 and 0 for diesel and gas 
carprices$fueltype <- as.factor(carprices$fueltype)
levels(carprices$fueltype)<-c(1,0)
carprices$fueltype <- as.numeric(levels(carprices$fueltype)) [carprices$fueltype]
###assigning 1 and 0 for std and turbo
carprices$aspiration <- as.factor(carprices$aspiration)
levels(carprices$aspiration)<-c(1,0)
carprices$aspiration <- as.numeric(levels(carprices$aspiration)) [carprices$aspiration]
###assigning 1 and 0 for four and two
carprices$doornumber <- as.factor(carprices$doornumber)
levels(carprices$doornumber)<-c(1,0)
carprices$doornumber <- as.numeric(levels(carprices$doornumber)) [carprices$doornumber]
###creating variables for each type of car body
dummy_1 <- data.frame(model.matrix( ~carbody, data = carprices))
dummy_1 <- dummy_1[,-1]
carprices <-cbind(carprices[-7],dummy_1)
###creating variables for drivewheel
dummy_1 <- data.frame(model.matrix( ~drivewheel, data = carprices))
dummy_1 <- dummy_1[,-1]
carprices <-cbind(carprices[-7],dummy_1)
###assigning 1 and 0 for front and rear engine location
carprices$enginelocation <- as.factor(carprices$enginelocation)
levels(carprices$enginelocation)<-c(1,0)
carprices$enginelocation <- as.numeric(levels(carprices$enginelocation)) [carprices$enginelocation]
###creating variables for enginetype
dummy_1 <- data.frame(model.matrix( ~enginetype, data = carprices))
dummy_1 <- dummy_1[,-1]
carprices <-cbind(carprices[-13],dummy_1)
###creating variables for cylindernumber
dummy_1 <- data.frame(model.matrix( ~cylindernumber, data = carprices))
dummy_1 <- dummy_1[,-1]
carprices <-cbind(carprices[-13],dummy_1)
###creating variables for fuelsystem
dummy_1 <- data.frame(model.matrix( ~fuelsystem, data = carprices))
dummy_1 <- dummy_1[,-1]
carprices <-cbind(carprices[-14],dummy_1)
###Outlier Removal ( for car features )
###curbweight
quantile(carprices$curbweight,seq(0,1,0.01))
##flooring the curbweight to 1819.72
carprices$curbweight[which(carprices$curbweight < 1819.72)] <- 1819.72
##Capping the curbweight to 3768.40
carprices$curbweight[which(carprices$curbweight > 3768.40)] <- 3768.40
quantile(carprices$carheight,seq(0,1,0.01))
quantile(carprices$carwidth,seq(0,1,0.01))
quantile(carprices$carlength,seq(0,1,0.01))
quantile(carprices$wheelbase,seq(0,1,0.01))
quantile(carprices$enginesize,seq(0,1,0.01))
###flooring the enginesize to 90
carprices$enginesize[which(carprices$enginesize < 90)] <- 90
###capping the enginesize to 256.08
carprices$enginesize[which(carprices$enginesize > 256.08)] <- 256.08
quantile(carprices$boreratio,seq(0,1,0.01))
###flooring the boreratio to 2.91
carprices$enginesize[which(carprices$enginesize < 2.91)] <- 2.91
quantile(carprices$stroke,seq(0,1,0.01))
###flooring the stroke to 2.64
carprices$stroke[which(carprices$stroke < 2.64)] <- 2.64
quantile(carprices$compressionratio,seq(0,1,0.01))
###there is a jump in compression ratio, it is in the top 10% but i will keep it as 10% cannot be capped
quantile(carprices$horsepower,seq(0,1,0.01))
###capping the horsepower to 184.00
carprices$horsepower[which(carprices$horsepower > 184.00)] <- 184.00
quantile(carprices$peakrpm,seq(0,1,0.01))

#######Data Preparation Completed#########
###Model building
###set seed to 100 and divide the train and test datasets
set.seed(100)
trainindices= sample(1:nrow(carprices), 0.7*nrow(carprices))
###Generate the train dataset
train = carprices[trainindices,]
###Generate the test dataset
test = carprices[-trainindices,]
###Building model with all variables
model_1 <- lm(price~.,data=train)
summary(model_1)
###Following the stepAIC method for variable reduction
step <- stepAIC(model_1,direction="both")
step
###taking the last function call
model_2 <- lm(formula = price ~ car_ID + symboling + CarModelName + fueltype + 
                aspiration + doornumber + enginelocation + wheelbase + carlength + 
                carwidth + carheight + curbweight + enginesize + boreratio + 
                stroke + compressionratio + horsepower + peakrpm + citympg + 
                carbodyhardtop + carbodyhatchback + carbodysedan + drivewheelfwd + 
                enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi, data = train)
summary(model_2)
###calculate VIF for model
vif(model_2)
#Removing car_ID (intuitive reasons)
model_3 <- lm(formula = price ~ symboling + CarModelName + fueltype + 
                aspiration + doornumber + enginelocation + wheelbase + carlength + 
                carwidth + carheight + curbweight + enginesize + boreratio + 
                stroke + compressionratio + horsepower + peakrpm + citympg + 
                carbodyhardtop + carbodyhatchback + carbodysedan + drivewheelfwd + 
                enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi, data = train)
summary(model_3)
vif(model_3)
#Removing CarModelName (intuitive reasons: New company introducing a new model)
model_4 <- lm(formula = price ~ symboling + fueltype + 
                aspiration + doornumber + enginelocation + wheelbase + carlength + 
                carwidth + carheight + curbweight + enginesize + boreratio + 
                stroke + compressionratio + horsepower + peakrpm + citympg + 
                carbodyhardtop + carbodyhatchback + carbodysedan + drivewheelfwd + 
                enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi, data = train)
summary(model_4)
vif(model_4)
#Removing fueltype as it has vif  155.75 and insignificant p=0.71888
model_5 <- lm(formula = price ~ symboling + 
                aspiration + doornumber + enginelocation + wheelbase + carlength + 
                carwidth + carheight + curbweight + enginesize + boreratio + 
                stroke + compressionratio + horsepower + peakrpm + citympg + 
                carbodyhardtop + carbodyhatchback + carbodysedan + drivewheelfwd + 
                enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi, data = train)
summary(model_5)
vif(model_5)
#Removing horsepower as it has vif 22.03 and p 0.28185
model_6 <- lm(formula = price ~ symboling + 
                aspiration + doornumber + enginelocation + wheelbase + carlength + 
                carwidth + carheight + curbweight + enginesize + boreratio + 
                stroke + compressionratio + peakrpm + citympg + 
                carbodyhardtop + carbodyhatchback + carbodysedan + drivewheelfwd + 
                enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi, data = train)
summary(model_6)
vif(model_6)
#Removing carlength vif= 14.272919 and p=0.450258
model_7 <- lm(formula = price ~ symboling + 
  aspiration + doornumber + enginelocation + wheelbase +  
  carwidth + carheight + curbweight + enginesize + boreratio + 
  stroke + compressionratio + peakrpm + citympg + 
  carbodyhardtop + carbodyhatchback + carbodysedan + drivewheelfwd + 
  enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
  fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi, data = train)
summary(model_7)
vif(model_7)
#Removing curbweight vif=26.001343 and p= 0.388455
model_8 <- lm(formula = price ~ symboling + 
                aspiration + doornumber + enginelocation + wheelbase +  
                carwidth + carheight + enginesize + boreratio + 
                stroke + compressionratio + peakrpm + citympg + 
                carbodyhardtop + carbodyhatchback + carbodysedan + drivewheelfwd + 
                enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi, data = train)
summary(model_8)
vif(model_8)
#Removing wheelbase vif=7.934020 and p=0.349003
model_9 <- lm(formula = price ~ symboling + 
                aspiration + doornumber + enginelocation + carwidth + carheight + enginesize + boreratio + 
                stroke + compressionratio + peakrpm + citympg + 
                carbodyhardtop + carbodyhatchback + carbodysedan + drivewheelfwd + 
                enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi, data = train)
summary(model_9)
vif(model_9)
#Removing citympg vif=5.344333  p=0.396562
model_10 <- lm(formula = price ~ symboling + 
                 aspiration + doornumber + enginelocation + carwidth + carheight + enginesize + boreratio + 
                 stroke + compressionratio + peakrpm  + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + drivewheelfwd + 
                 enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                 fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi, data = train)
summary(model_10)
vif(model_10)
#Removing doornumber vif=3.266230, p=0.0.921664
model_11 <- lm(formula = price ~ symboling + 
                 aspiration + enginelocation + carwidth + carheight + enginesize + boreratio + 
                 stroke + compressionratio + peakrpm  + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + drivewheelfwd + 
                 enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                 fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi, data = train)
summary(model_11)
vif(model_11)
#Removing carbodyhardtop p=0.810474,vif =1.725647
model_12 <- lm(formula = price ~ symboling + 
                 aspiration + enginelocation + carwidth + carheight + enginesize + boreratio + 
                 stroke + compressionratio + peakrpm  + 
                 carbodyhatchback + carbodysedan + drivewheelfwd + 
                 enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                 fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi, data = train)
summary(model_12)
vif(model_12)
#Removing drivewheelfwd vif=2.328063 , p=0.785152
model_13 <- lm(formula = price ~ symboling + 
                 aspiration + enginelocation + carwidth + carheight + enginesize + boreratio + 
                 stroke + compressionratio + peakrpm  + 
                 carbodyhatchback + carbodysedan + enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                 fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi, data = train)
summary(model_13)
vif(model_13)
#Removing carbodysedan vif=2.107283 , p=0.620834
model_14 <- lm(formula = price ~ symboling + 
                 aspiration + enginelocation + carwidth + carheight + enginesize + boreratio + 
                 stroke + compressionratio + peakrpm  + 
                 carbodyhatchback + enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                 fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi, data = train)
summary(model_14)
vif(model_14)
#Removing compressionratio vif=3.522144 , p = 0.592256
model_15 <-lm(formula = price ~ symboling + 
                aspiration + enginelocation + carwidth + carheight + enginesize + boreratio + 
                stroke +  peakrpm  + carbodyhatchback + enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi, data = train)
summary(model_15)
vif(model_15)
#Removing symboling  vif=1.700578 , p = 0.499816
model_16 <- lm(formula = price ~ aspiration + enginelocation + carwidth + carheight + enginesize + boreratio + 
                 stroke +  peakrpm  + carbodyhatchback + enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                 fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi, data = train)
summary(model_16)
vif(model_16)
#Removing enginetypeohcv vif=1.95583,p=0.214003
model_17 <- lm(formula = price ~ aspiration + enginelocation + carwidth + carheight + enginesize + boreratio + 
                 stroke +  peakrpm  + carbodyhatchback + enginetypeohc + cylindernumberfive + cylindernumberfour + 
                 fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi, data = train)
summary(model_17)
vif(model_17)
#Removing cylindernumberfive vif=2.458001 ,p=0.215763
model_18 <- lm(formula = price ~ aspiration + enginelocation + carwidth + carheight + enginesize + boreratio + 
                 stroke +  peakrpm  + carbodyhatchback + enginetypeohc + cylindernumberfour + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi, data = train)
summary(model_18)
vif(model_18)
#Removing carheight vif=1.842716  ,p=0.148105
model_19 <- lm(formula = price ~ aspiration + enginelocation + carwidth + enginesize + boreratio + 
                 stroke +  peakrpm  + carbodyhatchback + enginetypeohc + cylindernumberfour + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi, data = train)
summary(model_19)
vif(model_19)
#Removing fuelsystem2bbl vif=2.596471,p=0.113723
model_20 <- lm(formula = price ~ aspiration + enginelocation + carwidth + enginesize + boreratio + 
                 stroke +  peakrpm  + carbodyhatchback + enginetypeohc + cylindernumberfour + fuelsystemmpfi + fuelsystemspdi, data = train)
summary(model_20)
vif(model_20)
#Removing fuelsystemmpfi vif = 1.872620, p=0.149566
model_21 <- lm(formula = price ~ aspiration + enginelocation + carwidth + enginesize + boreratio + 
                 stroke +  peakrpm  + carbodyhatchback + enginetypeohc + cylindernumberfour + fuelsystemspdi, data = train)
summary(model_21) 
vif(model_21)
#Removing fuelsystemspdi vif =  1.401525, p=0.152155
model_22 <- lm(formula = price ~ aspiration + enginelocation + carwidth + enginesize + boreratio + 
                 stroke +  peakrpm  + carbodyhatchback + enginetypeohc + cylindernumberfour, data = train)
summary(model_22)
vif(model_22)
####the model has 10 significant variables

#Adjusted R2 is 0.912
test$test_price <- predict(model_22,test)
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2

##r=0.8926
##rsquared=0.7967
##The correlation between the actual and predicted is 79.67%
test$error <- test$price - test$test_price
ggplot(test,aes(y=test$error,x=car_ID)) + geom_point() + geom_smooth() + geom_hline(yintercept = 0)
##Error looks random in the plot
model_error <- lm(data=test,formula=car_ID~error)
summary(model_error)
###the intercept of the error line is 94.261 and the slope is 0.003534,hence, model looks accurate