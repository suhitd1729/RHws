install.packages("ISLR")
install.packages("leaps")
install.packages("plyr")
install.packages("glmnet")
install.packages("pls")
library(pls)
library(glmnet)
library(ISLR)
library(plyr)
library(leaps)

collegeData <- College
?College

#converting Yes to 1 and No to 0
collegeData$Private <- revalue(collegeData$Private, c("Yes"=1))
collegeData$Private <- revalue(collegeData$Private, c("No"=0))

collegeData

set.seed(123)
d = sample(nrow(collegeData), nrow(collegeData)*.7)
train <- collegeData[d,]
test <- collegeData[-d,]

names(collegeData)
#operating on the training data
Y_train_actual <- train[,2]
Y_test_actual <- test[,2]
Y_train_model <- lm(Apps~.,data = train)

#################################
## Using Linear Regression
#################################

summary(Y_train_model)
Y_train_predict <- predict(Y_train_model,newdata = train[,-2])
newData <- data.frame(Y_train_actual,Y_train_predict)
no_of_neg_entries <- nrow(subset(newData , newData$Y_train_predict < 0)) #30 entries

LM_train_MSE <- mean((Y_train_predict - Y_train_actual)^2) #987384.5

Y_test_predict <- predict(Y_train_model,newdata = test[,-2])
LM_test_MSE <- mean((Y_test_predict - Y_test_actual)^2)
LM_test_MSE #1287445 

#since some predicted values are appearing as negatives , it is better to remove non significant parameters.
#The PR value for Terminal has a very high value. (0.93). Likewise Books too has a high value (0.70)
#P.Undergrad also has 0.368 PR value.Removing the three.

Y_train_model2 <- lm(Apps~.-Terminal-Books-P.Undergrad,data = train)
summary(Y_train_model2)
Y_train_predict2 <- predict(Y_train_model2,newdata = train[,-2])
newData2 <- data.frame(Y_train_actual,Y_train_predict2)
no_of_neg_entries2 <- nrow(subset(newData2 , newData2$Y_train_predict2 < 0)) #30 entries
no_of_neg_entries2

LM_train_MSE2 <- mean((Y_train_predict2 - Y_train_actual)^2) #989207.2
#It seems that the training error has increased and the number of negative entries hasn't changed. 

Y_test_predict2<- predict(Y_train_model2,newdata = test[,-2])
LM_test_MSE2 <- mean((Y_test_predict2 - Y_test_actual)^2)
LM_test_MSE2 #1292477 

#examining using best subset 
bestsubset <- regsubsets(Apps~., data = train, nbest = 1, nvmax = 18, method = "exhaustive")
summary(bestsubset)
s <- summary(bestsubset)
names(s)
plot(s$rss)
#from this we can see that for index = 12 , the training error is reduced significantly, modifying the predictor parameters 
Y_train_model3 <- lm(Apps~.-Terminal-Books-P.Undergrad-Personal-Terminal-perc.alumni,data = train)
summary(Y_train_model3)
Y_train_predict3 <- predict(Y_train_model3,newdata = train[,-2])
newData3 <- data.frame(Y_train_actual,Y_train_predict3)
no_of_neg_entries3 <- nrow(subset(newData3 , newData3$Y_train_predict3 < 0)) #26 entries
no_of_neg_entries3
#The number of negative entries reduces to 26

LM_train_MSE3 <- mean((Y_train_predict3 - Y_train_actual)^2) #999802.3
LM_train_MSE3
#The training error has increased to 999802.3


Y_test_predict3<- predict(Y_train_model3,newdata = test[,-2])
LM_test_MSE3 <- mean((Y_test_predict3 - Y_test_actual)^2)
LM_test_MSE3 #1240482 
#The test error has reduced to 1240482

#################################
## Using Ridge Regression
#################################
collegeData["Private"] <- as.numeric(collegeData$Private)
#matrix conversion

train_inp_ridge <- as.matrix(collegeData[d,c(1,3:18)])
#train_inp_ridge <- colData[d,c(1,3:18)]
train_out_ridge <- collegeData[d,c(2)]
test_inp_ridge <- as.matrix(collegeData[-d,c(1,3:18)])
cv.out.ridge <- cv.glmnet(train_inp_ridge, train_out_ridge, alpha = 0)
plot(cv.out.ridge)

names(cv.out.ridge)
bestlam <- cv.out.ridge$lambda.min
bestlam #428.2339
ridge.mod = glmnet(train_inp_ridge,train_out_ridge, alpha=0)
ridge.pred <- predict(ridge.mod, s= bestlam, type = "coefficients")
ridge.pred2.test <- predict(ridge.mod, s = bestlam, newx = test_inp_ridge , type = "response")


Y_test_hat_ridge <- ridge.pred2.test
Y_test_ridge_actual <- collegeData[-d,c(2)]
ridge_test_error <- mean((Y_test_hat_ridge - Y_test_ridge_actual)^2)  #test_error
ridge_test_error #1289402 

ridge.pred2.train <- predict(ridge.mod, s = bestlam, newx = train_inp_ridge , type = "response")
Y_train_hat_ridge <- ridge.pred2.train
Y_train_ridge_actual <- collegeData[d,c(2)]
ridge_train_error <- mean((Y_train_hat_ridge - Y_train_ridge_actual)^2)  #test_error
ridge_train_error #1376257 

#################################
## Using Lasso Regression
#################################
collegeData["Private"] <- as.numeric(collegeData$Private)
#matrix conversion

train_inp_lasso <- as.matrix(collegeData[d,c(1,3:18)])
#train_inp_ridge <- colData[d,c(1,3:18)]
train_out_lasso <- collegeData[d,c(2)]
test_inp_lasso <- as.matrix(collegeData[-d,c(1,3:18)])
cv.out.lasso <- cv.glmnet(train_inp_lasso, train_out_lasso, alpha = 1)
plot(cv.out.lasso)

names(cv.out.lasso)
bestlam <- cv.out.lasso$lambda.min
bestlam #2.752719
lasso.mod = glmnet(train_inp_lasso,train_out_lasso, alpha=1)
lasso.pred <- predict(lasso.mod, s= bestlam, type = "coefficients")
lasso.pred
lasso.pred2.test <- predict(lasso.mod, s = bestlam, newx = test_inp_lasso , type = "response")

Y_test_hat_lasso <- lasso.pred2.test
Y_test_lasso_actual <- collegeData[-d,c(2)]
lasso_test_error <- mean((Y_test_hat_lasso - Y_test_lasso_actual)^2)  #test_error
lasso_test_error #1280908 

lasso.pred2.train <- predict(lasso.mod, s = bestlam, newx = train_inp_lasso , type = "response")
Y_train_hat_lasso <- lasso.pred2.train
Y_train_lasso_actual <- collegeData[d,c(2)]
lasso_train_error <- mean((Y_train_hat_lasso - Y_train_lasso_actual)^2)  #test_error
lasso_train_error #988407.6



#################################
## PCR 
#################################

pcr.fit = pcr(Apps ~., data = train, scale = TRUE, validation = "CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type = "MSEP")
#choosing the value of no of components as 9
pcr.pred.train = predict(pcr.fit, train, ncomp = 9)
pcr.pred.test = predict(pcr.fit, test, ncomp = 9)
pcr.train.error <- mean((pcr.pred.train-Y_train_actual)^2)
pcr.train.error #2356710
pcr.test.error <- mean((pcr.pred.test-Y_test_actual)^2)
pcr.test.error #1671682

#################################
## PLS 
#################################
pls.fit = plsr(Apps ~., data = train, scale = TRUE, validation = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP")
#choosing the value of no of components as 6
pls.pred.train = predict(pls.fit, train, ncomp = 6)
pls.pred.test = predict(pls.fit, test, ncomp = 6)
pls.train.error <- mean((pls.pred.train-Y_train_actual)^2)
pls.train.error #1026354
pls.test.error <- mean((pls.pred.test-Y_test_actual)^2)
pls.test.error #1303733
