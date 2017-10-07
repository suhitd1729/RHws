#######################################################
# This code utilizes Linear Regression and kNN for 
# different values of k for the zipcode data contained
# in the ElemStatLearn package(Q3 in HW1)

# Suhit Datta
#######################################################

ls("package:ElemStatLearn")

install.packages("ElemStatLearn")
library(ElemStatLearn)

?zip.test
?zip.train

#setwd("C:\\DragonBallZ\\Fall2017\\Prof.Rachel")

ztest <- zip.test
ztrain <- zip.train
print(ztest)
names(ztrain)

modZTrain <- as.data.frame((ztrain))
colnames(modZTrain)

modZTest <- as.data.frame((ztest))
nrow(modZTest)

#####################################
#####################################
## Linear Regression
#####################################
#####################################

############
# filtering the training set for those values where V1 value is 2 and 3
############
filztrain <- subset(modZTrain,modZTrain$V1 ==2 | modZTrain$V1 ==3)
names(filztrain)
nrow((filztrain))

############
# filtering the test set where V1 is 2 and 3
############
filztest <- subset(modZTest,modZTest$V1 ==2 | modZTest$V1 ==3)
names(filztest)
nrow((filztest))

############
# linear modelling of the training set
############
fitz <- lm(V1~.-V1,data = filztrain)
names(fitz)
summary(fitz)

############
# finding the predicted values
############
predicted_ztest <-predict(fitz, newdata = filztest[,-1])

############
# actual data is the first column 
############
actual_ztest <- filztest[,1]

nrow(actual_ztest)
plot(actual_ztest,predicted_ztest)

############
# gives the difference between the actual and the test data 
############
diff <- (actual_ztest-predicted_ztest)
lin_reg_sqrd_error <- mean(diff^2)
print(lin_reg_sqrd_error)

############
# column binding the actual and the predicted data sets to separate the data for 2 and 3
############
diffDF <- cbind(actual_ztest,predicted_ztest)
diffDF_2 <- subset(diffDF, actual_ztest == 2)
diffDF_3 <- subset(diffDF, actual_ztest == 3)

############
#plotting the density plots and the histogram plots
############
plot(density(diffDF_2),xlab = "Predicted Values",main = "Density Distribution of V1=2")
hist(diffDF_2,xlab = "Predicted Values" , main = "Histogram For V1=2")
plot(density(diffDF_3),xlab = "Predicted Values",main = "Density Distribution of V1=3")
hist(diffDF_3,xlab = "Predicted Values" , main = "Histogram For V1=3")


##############################################
##############################################
##KNN Method
##############################################
##############################################

require(class)

############
# computing predicted values for different values of K(1,3,5,7,9,11,13,15) for the test set
############

predtest_knn_1 <- knn(filztrain[,-1], filztest[,-1], filztrain[,1], 1)
predtest_knn_3 <- knn(filztrain[,-1], filztest[,-1], filztrain[,1], 3)
predtest_knn_5 <- knn(filztrain[,-1], filztest[,-1], filztrain[,1], 5)
predtest_knn_7 <- knn(filztrain[,-1], filztest[,-1], filztrain[,1], 7)
predtest_knn_9 <- knn(filztrain[,-1], filztest[,-1], filztrain[,1], 9)
predtest_knn_11 <- knn(filztrain[,-1], filztest[,-1], filztrain[,1], 11)
predtest_knn_13 <- knn(filztrain[,-1], filztest[,-1], filztrain[,1], 13)
predtest_knn_15 <- knn(filztrain[,-1], filztest[,-1], filztrain[,1], 15)

############
# function to calculate the mean squared error for 
# different values of predicted KNN values for various K
############

getMeanSqErrorForTestData <- function(x) {
  actualVal <- as.numeric(as.character(filztest[,1]))
  predictedVal <- as.numeric(as.character(x))
  mse <- mean((predictedVal - actualVal)^2)
  return(mse)
}

############
#fetching the mean squared error for different KNN values for the test set
############
ts_mse_knn1 <- getMeanSqError(predtest_knn_1)
ts_mse_knn3 <- getMeanSqError(predtest_knn_3)
ts_mse_knn5 <- getMeanSqError(predtest_knn_5)
ts_mse_knn7 <- getMeanSqError(predtest_knn_7)
ts_mse_knn9 <- getMeanSqError(predtest_knn_9)
ts_mse_knn11 <- getMeanSqError(predtest_knn_11)
ts_mse_knn13 <- getMeanSqError(predtest_knn_13)
ts_mse_knn15 <- getMeanSqError(predtest_knn_15)

testseterror<- c((ts_mse_knn1),
(ts_mse_knn3),
(ts_mse_knn5),
(ts_mse_knn7),
(ts_mse_knn9),
(ts_mse_knn11),
(ts_mse_knn13),
(ts_mse_knn15))

knnVals <- c(1,3,5,7,9,11,13,15)

testerrorTable = data.frame(K = knnVals, testError = testseterror)

############
# plotting the KNN error values for the Test Data
############
plot(errorTable,xlab = "KNN Values",ylab = "KNN errors",main = "KNN for Test Data",type = "o")

histogram(testseterror,xlab = "test set error values")

############
# computing predicted values for different values of K(1,3,5,7,9,11,13,15) for training data  
############
predtrain_knn_1 <- knn(filztrain[,-1], filztrain[,-1], filztrain[,1], 1)
predtrain_knn_3 <- knn(filztrain[,-1], filztrain[,-1], filztrain[,1], 3)
predtrain_knn_5 <- knn(filztrain[,-1], filztrain[,-1], filztrain[,1], 5)
predtrain_knn_7 <- knn(filztrain[,-1], filztrain[,-1], filztrain[,1], 7)
predtrain_knn_9 <- knn(filztrain[,-1], filztrain[,-1], filztrain[,1], 9)
predtrain_knn_11 <- knn(filztrain[,-1], filztrain[,-1], filztrain[,1], 11)
predtrain_knn_13 <- knn(filztrain[,-1], filztrain[,-1], filztrain[,1], 13)
predtrain_knn_15 <- knn(filztrain[,-1], filztrain[,-1], filztrain[,1], 15)

############
# function to calculate the mean squared error for 
# different values of predicted KNN values for various K for the training data
############
getMeanSqErrorForTrainingData <- function(x) {
  actualVal <- as.numeric(as.character(filztrain[,1]))
  predictedVal <- as.numeric(as.character(x))
  mse <- mean((predictedVal - actualVal)^2)
  return(mse)
}

############
#fetching the mean squared error for different KNN values for the training set
############
tr_mse_knn1 <- getMeanSqErrorForTrainingData(predtrain_knn_1)
tr_mse_knn3 <- getMeanSqErrorForTrainingData(predtrain_knn_3)
tr_mse_knn5 <- getMeanSqErrorForTrainingData(predtrain_knn_5)
tr_mse_knn7 <- getMeanSqErrorForTrainingData(predtrain_knn_7)
tr_mse_knn9 <- getMeanSqErrorForTrainingData(predtrain_knn_9)
tr_mse_knn11 <- getMeanSqErrorForTrainingData(predtrain_knn_11)
tr_mse_knn13 <- getMeanSqErrorForTrainingData(predtrain_knn_13)
tr_mse_knn15 <- getMeanSqErrorForTrainingData(predtrain_knn_15)

trainseterror <- c(tr_mse_knn1,
tr_mse_knn3,
tr_mse_knn5,
tr_mse_knn7,
tr_mse_knn9,
tr_mse_knn11,
tr_mse_knn13,
tr_mse_knn15)

trainerrorTable = data.frame(K = knnVals, trainError = trainseterror)

############
# plotting the KNN error values for the Training Data
############
plot(errorTable,xlab = "KNN Values",ylab = "KNN errors",main = "KNN for Train Data",type = "o")

histogram(trainseterror,xlab = "training set error values")
