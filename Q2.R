#######################################################
# This code is used to analyse the Auto Dataset given 
# as per HW 1 - Question 2

# Suhit Datta
#######################################################

library(ISLR)
library(lattice)

?Auto
autodata <- Auto
names(autodata)
ncol(autodata)

summary(autodata)
cor(autodata[,1:7])
###################
# selecting all data except the names column
###################

cleanData <- autodata[,1:8]
names(cleanData)
cor(cleanData)
plot(cleanData)

###################
# creating a linear model
###################

model <- lm(mpg~.,data=cleanData)
names(model)
summary(model)
print(model)

plot(cleanData$acceleration,cleanData$mpg,xlab = "acceleration",ylab = "mpg")

###################
# removing acceleration
###################
model_updated1 <- lm(mpg~.-acceleration,data=cleanData)
summary(model_updated1)
print(model_updated1)

#modified the dataset to remove the acceleration column
cleanData <- subset(cleanData,select = -acceleration)

plot(cleanData$year,cleanData$mpg,xlab = "Year", ylab = "mpg" , main ="Relation between Year and MPG")

####################
# trying different models for checking significant interactions 
####################
model_updated2 <- lm(mpg~.+displacement:cylinders,data=cleanData)
summary(model_updated2)
print(model_updated2)

model_updated3 <- lm(mpg~.+displacement:weight,data=cleanData)
summary(model_updated3)
print(model_updated3)

model_updated4 <- lm(mpg~.+year:weight,data=cleanData)
summary(model_updated4)
print(model_updated4)


model_updated5 <- lm(mpg~.+displacement:year,data=cleanData)
summary(model_updated5)
print(model_updated5)

model_updated6 <- lm(mpg~.+displacement:horsepower,data=cleanData)
summary(model_updated6)
print(model_updated6)


model_updated7 <- lm(mpg~.+displacement:horsepower:weight,data=cleanData)
summary(model_updated7)
print(model_updated7)
  