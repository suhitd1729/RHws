#######################################################
# This code is used to analyse the Dataset given 
# as per HW 4 - Question 3

# Suhit Datta
#######################################################
rm(list = ls())
set.seed(123)

library(ISLR)
library(gbm)
library(randomForest)

?Weekly
names(Weekly)
#removing irrelevant columns Year and Today
WData <- Weekly
WData <- subset(WData, select = -c(Today,Year))

train = sample(nrow(WData), 0.7*nrow(WData))
WData$Direction = ifelse(WData$Direction == "Up", 1, 0)
WData.train = WData[train, ]
WData.test = WData[-train, ]

## Logistic Regression
logit.fit <- glm(Direction ~ ., data = WData.train, family = "binomial")
logit.probs <- predict(logit.fit, newdata = WData.test, type = "response")
logit.pred <- ifelse(logit.probs > 0.5, 1, 0)
table(WData.test$Direction, logit.pred)
mean(WData.test$Direction != logit.pred)

## Random Forest
rf.fit = randomForest(Direction ~ ., data = WData.train, mtry = 2)
rf.probs = predict(rf.fit, newdata = WData.test)
rf.pred = ifelse(rf.probs > 0.5, 1, 0)
table(WData.test$Direction, rf.pred)
mean(WData.test$Direction != rf.pred)

## Boosting
boost.fit = gbm(Direction ~ ., data = WData.train, distribution = "bernoulli", n.trees = 5000)
boost.probs= predict(boost.fit, newdata = WData.test, n.trees = 5000)
boost.pred = ifelse(boost.probs > 0.5, 1, 0)
table(WData.test$Direction, boost.pred)
mean(WData.test$Direction != boost.pred)

## Bagging
bag.fit = randomForest(Direction ~ ., data = WData.train, mtry = 6)
bag.probs = predict(bag.fit, newdata = WData.test)
bag.pred = ifelse(bag.probs > 0.5, 1, 0)
table(WData.test$Direction, bag.pred)
mean(WData.test$Direction != bag.pred)

