#######################################################
# This code is used to analyse the Boston Dataset given 
# as per HW 1 - Question 4
# Suhit Datta
#######################################################

library(MASS)
?Boston
bostonData <- Boston
names(bostonData)

################
#finding the minimum and maximum values 
################

min(bostonData$lstat) #1.73
max(bostonData$lstat) #37.97

min(bostonData$crim)#0.00632
max(bostonData$crim)#88.9762

min(bostonData$black)#0.32
max(bostonData$black)#396.9

hist((bostonData$crim),xlab = "variation of crime rate",main = "Distribution of Crime Rate")

################
#predicting the crime rate with different predictors
################
plot(bostonData$lstat,bostonData$crim)

plot(bostonData$black,bostonData$crim)

plot(bostonData$dis,bostonData$crim)

plot(bostonData$rad,bostonData$crim)

plot(bostonData$indus,bostonData$crim)

plot(bostonData$ptratio,bostonData$crim)

plot(bostonData$medv,bostonData$crim)

plot(bostonData$tax,bostonData$crim)

plot(bostonData$chas,bostonData$crim)

plot(bostonData$nox,bostonData$crim)

################
#predicting the nitrogen concentration with different predictors
################
hist((bostonData$nox),xlab = "variation of nitrogen oxides concentration"
     ,main = "Distribution of nitrogen oxides concentration")

plot(bostonData$zn,bostonData$nox)

plot(bostonData$chas,bostonData$nox)

plot(bostonData$age,bostonData$nox)

plot(bostonData$lstat,bostonData$nox)

plot(bostonData$medv,bostonData$nox)

################
#range of predictors
################
range(bostonData$crim)

range(bostonData$tax)

range(bostonData$ptratio)

################
#subset of more than 7 rooms per dwelling
################
morethan7rooms <- subset(bostonData,rm>7)
nrow(morethan7rooms)

################
#subset of more than 8 rooms per dwelling
################
morethan8rooms <- subset(bostonData,rm>8)
nrow(morethan8rooms)

names(morethan8rooms)

################
#analysing the subset which consists of more than 8 rooms 
################
colMeans(morethan8rooms,na.rm = TRUE)

colMeans(bostonData,na.rm=TRUE)
