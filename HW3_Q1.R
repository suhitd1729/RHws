#######################################################
# This code is used to analyse the Question given 
# as per HW 3 - Question 1 in STA 545

# Suhit Datta
#######################################################

rm(list=ls())

library(ISLR)
library(MASS)
#install.packages("caret")
#install.packages("e1071")
library(caret) 
library(e1071)
BostonData <- Boston;
names(BostonData)
medCrime <- median(BostonData$crim)
#0.25651 - median value of crim
medCrime
#modified the Boston Data to set values above median to 1 and below Median to 0 
BostonData$crim[BostonData$crim<medCrime] <- 0
BostonData$crim[BostonData$crim>medCrime] <- 1

#########################################
# Creating a training and test set
#########################################
set.seed(1)
train <- sample(1:nrow(BostonData), .70*nrow(BostonData))
b_train <- BostonData[train,]
b_test <- BostonData[-train,]

crim_train <- as.numeric(b_train$crim)  #original crim training data
crim_test <- as.numeric(b_test$crim)  #original crim test data

#########################################
# Logistic Regression
#########################################
lr.fit <- glm(crim ~., data = b_train, family = "binomial")
summary(lr.fit)
names(lr.fit)

# Predicting the values
lr_pred_crim_train <- predict(lr.fit, newdata = b_train[,-1], type = "response")
lr_pred_crim_train <- round(lr_pred_crim_train)

lr_pred_crim_test <- predict(lr.fit, newdata = b_test[,-1], type = "response")
lr_pred_crim_test <- round(lr_pred_crim_test)

#  Calculate the mean sq error rates

lr_train_err <- sum((crim_train - lr_pred_crim_train)^2)/length(crim_train)
lr_test_err <- sum((crim_test - lr_pred_crim_test)^2)/length(crim_test)

lr_train_err #0.09039548
lr_test_err #0.07894737

lr_conf <- confusionMatrix(lr_pred_crim_test, crim_test)
names(lr_conf)
lr_conf$table

#########################################
# LDA 
#########################################
lda.fit <- lda(crim~., data = b_train)
lda_pred_crim_train <- predict(lda.fit, newdata = b_train[,-1])
lda_pred_crim_train <- as.numeric(lda_pred_crim_train$class)-1

lda_pred_crim_test <- predict(lda.fit, newdata = b_test[,-1])
lda_pred_crim_test <- as.numeric(lda_pred_crim_test$class)-1

#  Calculate the mean sq error rates

lda_train_err <- sum((crim_train - lda_pred_crim_train)^2)/length(crim_train)
lda_test_err <- sum((crim_test - lda_pred_crim_test)^2)/length(crim_test)

lda_train_err #0.1242938
lda_test_err #0.1315789

lda_conf <- confusionMatrix(lda_pred_crim_test, crim_test)
names(lda_conf)
lda_conf$table

#########################################
# KNN
#########################################
require(class)
knn1_pred_crim_train <- as.numeric(knn(b_train[,-1], b_train[,-1], b_train[,1], 1))-1
knn1_pred_crim_test <- as.numeric(knn(b_train[,-1], b_test[,-1], b_train[,1], 1))-1

#  Calculate the mean sq error rates

knn_train_err <- sum((crim_train - knn1_pred_crim_train)^2)/length(crim_train)
knn_test_err <- sum((crim_test - knn1_pred_crim_test)^2)/length(crim_test)

knn_train_err #0
knn_test_err #0.07894737

knn_conf <- confusionMatrix(knn1_pred_crim_test, crim_test)
names(knn_conf)
knn_conf$table
