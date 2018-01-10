#######################################################
# This code is used to analyse the Dataset given 
# as per HW 4 - Question 4

# Suhit Datta
#######################################################
rm (list=ls())
getwd()

library(kernlab)
library(ggplot2)
library(randomForest)
spamData = data(spam)
spamData = spam
set.seed(123)

train <- sample(1:nrow(spamData), .70*nrow(spamData))
s_train <- spamData[train,]
s_test <- spamData[-train,]

#Error holder
modelName = c()
testErrVector = c()
trainErrVector = c()

for (i in 1:57){
  spamBAGMod = randomForest(type~.,data = s_train, n.tree =1000, mtry = i)
  
  spamBAGPred_test <- predict(spamBAGMod, newdata = s_test,type='class')
  spamBAGPred_train <- predict(spamBAGMod, newdata = s_train, type='class')
  
  spamBAGPred_test <- predict(spamBAGMod, newdata = s_test,type='class')
  spamBAGPred_train <- predict(spamBAGMod, newdata = s_train, type='class')
  
  BAG_test_err <- mean(spamBAGPred_test != s_test$type)
  BAG_train_err <- mean(spamBAGPred_train != s_train$type)
  
  modelName = c(modelName,i)
  testErrVector = c(testErrVector,BAG_test_err)
  trainErrVector = c(trainErrVector,BAG_train_err)
}

errorDF = data.frame(Model_Name = modelName,Training_Error = trainErrVector,Test_Error = testErrVector)

plot(errorDF$Model_Name,errorDF$Test_Error,type='o',xlab='Model Name',ylab='Test Error',main = 'Plot of Model Name vs Test Error')


ggplot(errorDF, aes(x = Model_Name,group = 1)) + 
  geom_line(aes(y = testErrVector), colour="red") + 
  geom_line(aes(y = trainErrVector), colour = "blue") +
  geom_point(aes(y = testErrVector), colour="red") + 
  geom_point(aes(y = trainErrVector), colour = "blue") +
  labs(y = "Error",x= "Models") +
  scale_color_manual(values = c("Test Error" = 'red','Training Error' = 'blue'))



