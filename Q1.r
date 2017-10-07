#######################################################
# This code is used to analyse the Auto Dataset given 
# as per HW 1 - Question 1

# Suhit Datta
#######################################################
warn=1
install.packages("ISLR")
install.packages("ggplot2")
install.packages("lattice")
install.packages("corrplot")
??Auto
getwd()

#setwd("C:\\DragonBallZ\\Fall2017\\Prof.Rachel\\R_directory")

library(ISLR)
library(lattice)
library(corrplot)

?Auto
autodata <- Auto
names(autodata)
##################
## removing the names column from the dataset
##################
cleanData <- autodata[,1:8]
plot(cleanData)

#save(cleanData, file = "CleanedAutoData.RData")

#creating the correlation plot 
corrplot(cor(cleanData),method="circle")

#################
## histogram of the MPG distribution 
#################
histogram(cleanData$mpg ,main="Distribution of MPG", xlab="MPG")

#################
## density plot of the MPG distribution 
#################
plot(density(cleanData$mpg),main="Density Distribution of MPG")

#################
# Origin of car (1. American, 2. European, 3. Japanese)
#################
amData <- subset(cleanData,origin == 1)
eurodata <- subset(cleanData,origin == 2)
japdata <- subset(cleanData,origin == 3)

plot(amData)
plot(eurodata)
plot(japdata)

cor(amData[,1:7])
cor(eurodata[,1:7])
cor(japdata[,1:7])

#################
## xyplots differentiating in terms of the various types of groups(American European and Japanese car Data) 
#################
xyplot(mpg ~ horsepower, data=autodata, groups = origin, type = c("p","smooth") , auto.key = list(columns=3))

xyplot(mpg ~ displacement, data=autodata, groups = origin, type = c("p","smooth") , auto.key = list(columns=3))

xyplot(mpg ~ weight, data=autodata, groups = origin, type = c("p","smooth") , auto.key = list(columns=3))

xyplot(mpg ~ acceleration, data=autodata, groups = origin, type = c("p","smooth") , auto.key = list(columns=3))

xyplot(mpg ~ year, data=autodata, groups = origin, type = c("p","smooth") , auto.key = list(columns=3))

xyplot(mpg ~ cylinders, data=autodata, groups = origin, type = c("p","smooth") , auto.key = list(columns=3))

#################
## histogram of the mpg distribution
#################
hist(cleanData$cylinders)

#################
## computing the average  
#################
avg_mpg_Am <- mean(amData$mpg)
avg_mpg_Eur <- mean(eurodata$mpg)
avg_mpg_Jap <- mean(japdata$mpg) #jap cars have the highest mpg

print(avg_mpg_Am)
print(avg_mpg_Eur)
print(avg_mpg_Jap)
