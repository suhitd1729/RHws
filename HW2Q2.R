#######################################################
# This code is used to analyse the Question given 
# as per HW 2 - Question 2 in STA 545

# Suhit Datta
#######################################################
rm(list=ls())
library(leaps)
library(glmnet)
######################################
## Reading the Data from the txt file
######################################

#setwd("C:\\DragonBallZ\\Fall2017\\Prof.Rachel\\HW2")
train <- read.table("ticdata2000.txt", sep = "" , header = F,
                   na.strings ="", stringsAsFactors= F)
trainY <- train$V86
trainX <- train[-86]

testX <- read.table("ticeval2000.txt", sep = "" , header = F,
                    na.strings ="", stringsAsFactors= F)
testY <- read.table("tictgts2000.txt", sep = "" , header = F,
                    na.strings ="", stringsAsFactors= F)

######################################
## Linear Regression (OLS)
######################################
## for the training set
linmodel <- lm(V86~.,data = train)
print(linmodel)
summary(linmodel)
trainY_Pred <- predict(linmodel,newdata = trainX)
hist(trainY_Pred)
plot(density(trainY_Pred))
max(trainY_Pred) #0.8379708
typeof(trainY_Pred) #returns value of type double, we need to perform Round operation on it.

trainY_Pred <- round(trainY_Pred)
hist(trainY_Pred)
max(trainY_Pred) #1

lm_train_error <- mean((trainY_Pred-trainY)^2)
lm_train_error #0.05960151

## for the test set
testY_Pred <- predict(linmodel,newdata = testX)
hist(testY_Pred)
max(testY_Pred) #0.8082358
typeof(testY_Pred) #returns value of type double, we need to perform Round operation on it.

testY_Pred <- round(testY_Pred)
hist(testY_Pred)
max(testY_Pred) #1

lm_test_error <- mean((testY_Pred-testY)^2)
lm_test_error #0.05975

?regsubsets
######################################
## Forward Subset Selection 
######################################
#exhsubset <- regsubsets(V86~., data = train, nbest = 1, nvmax = 86, method = "exhaustive",really.big =TRUE)
frwdsubset <- regsubsets(V86~., data = train, nbest = 1, nvmax = 86, method = "forward")
summ.fwd <- summary(frwdsubset)
x11()
plot(frwdsubset, scale = "r2")
x11()
plot(frwdsubset, scale = "adjr2")
x11()
plot(frwdsubset, scale = "Cp")
x11()
plot(frwdsubset, scale = "bic")

plot(summ.fwd$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
plot(summ.fwd$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
plot(summ.fwd$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(summ.fwd$adjr2, xlab = "Number of Variables", ylab = "Adjusted R^2", type = "l")
which(summ.fwd$cp == min(summ.fwd$cp)) #23
which(summ.fwd$bic == min(summ.fwd$bic)) #8
which(summ.fwd$rss == min(summ.fwd$rss)) #85

#choosing 23 as the optimal value in terms of no of variables obtained as per cp

summ.frwd23 <- summary(frwdsubset)$outmat[23,]
coef.frwd23 <- coef(frwdsubset, 23)
coef.frwd23 #v 4,7,10,16,18,21,35,36,41,42,43,44,46,47,57,58,59,78,79,80,82,83,85
frwd23.linmodel <- lm(V86~V4+V7+V10+V16+V18+V21+V35+V36+V41+V42+V43+V44+V46+V47+V57+V58+V59+V78+V79+V80+V82+V83+V85,data = train)
summary(frwd23.linmodel) 

#training error for Forward Subset
trainY_Pred_frwdsubset <- predict(frwd23.linmodel,newdata = trainX)
hist(trainY_Pred_frwdsubset)
max(trainY_Pred_frwdsubset) #0.791231
typeof(trainY_Pred_frwdsubset) #returns value of type double, we need to perform Round operation on it.

trainY_Pred_frwdsubset <- round(trainY_Pred_frwdsubset)
hist(trainY_Pred_frwdsubset)
max(trainY_Pred_frwdsubset) #1

fwdsubset_train_error <- mean((trainY_Pred_frwdsubset-trainY)^2)
fwdsubset_train_error #0.05977327

#test error for Forward Subset
testY_Pred_frwdsubset <- predict(frwd23.linmodel,newdata = testX)
hist(testY_Pred_frwdsubset)
max(testY_Pred_frwdsubset) #0.7343655
typeof(testY_Pred_frwdsubset) #returns value of type double, we need to perform Round operation on it.

testY_Pred_frwdsubset <- round(testY_Pred_frwdsubset)
hist(testY_Pred_frwdsubset)
max(testY_Pred_frwdsubset) #1

fwdsubset_test_error <- mean((testY_Pred_frwdsubset-testY)^2)
fwdsubset_test_error #0.05975


######################################
## Backward Subset Selection 
######################################
bkwdsubset <- regsubsets(V86~., data = train, nbest = 1, nvmax = 86, method = "backward")
summ.bkw <- summary(bkwdsubset)
x11()
plot(bkwdsubset, scale = "r2")
x11()
plot(bkwdsubset, scale = "adjr2")
x11()
plot(bkwdsubset, scale = "Cp")
x11()
plot(bkwdsubset, scale = "bic")

plot(summ.bkw$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
plot(summ.bkw$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
plot(summ.bkw$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(summ.bkw$adjr2, xlab = "Number of Variables", ylab = "Adjusted R^2", type = "l")
which(summ.bkw$cp == min(summ.bkw$cp))  #29
which(summ.bkw$bic == min(summ.bkw$bic)) #8
which(summ.bkw$rss == min(summ.bkw$rss)) #85

#choosing 29 as the optimal value in terms of no of variables obtained as per cp

summ.bkwd29 <- summary(bkwdsubset)$outmat[29,]
coef.bkwd29 <- coef(bkwdsubset, 29)
coef.bkwd29 #v 4,6,10,17,18,21,22,28,30,35,36,41,42,44,46,47,55,57,58,59,63,76,78,79,80,82,83,84,85

bkwd29.linmodel <- lm(V86~V4+V6+V10+V17+V18+V21+V22+V28+V30+V35+V36+V41+V42+V44+V46+V47+V55+V57+V58+V59+V63+V76+V78+V79+V80+V82+V83+V84+V85,data = train)
summary(bkwd29.linmodel) 

#training error for Backward Subset
trainY_Pred_bkwdsubset <- predict(bkwd29.linmodel,newdata = trainX)
hist(trainY_Pred_bkwdsubset)
max(trainY_Pred_bkwdsubset) #0.8016704
typeof(trainY_Pred_bkwdsubset) #returns value of type double, we need to perform Round operation on it.

trainY_Pred_bkwdsubset <- round(trainY_Pred_frwdsubset)
hist(trainY_Pred_bkwdsubset)
max(trainY_Pred_bkwdsubset) #1

bkwsubset_train_error <- mean((trainY_Pred_bkwdsubset-trainY)^2)
bkwsubset_train_error #0.05977327

#test error for Forward Subset
testY_Pred_bkwdsubset <- predict(bkwd29.linmodel,newdata = testX)
hist(testY_Pred_bkwdsubset)
max(testY_Pred_bkwdsubset) #0.7363404
typeof(testY_Pred_bkwdsubset) #returns value of type double, we need to perform Round operation on it.

testY_Pred_bkwdsubset <- round(testY_Pred_bkwdsubset)
hist(testY_Pred_bkwdsubset)
max(testY_Pred_bkwdsubset) #1

bkwsubset_test_error <- mean((testY_Pred_bkwdsubset-testY)^2)
bkwsubset_test_error #0.05975

######################################
## Ridge Regression 
######################################

train_inp_ridge <- as.matrix(train[,c(1:85)])
#train_inp_ridge <- colData[d,c(1,3:18)]
train_out_ridge <- train[,c(86)]
test_inp_ridge <- as.matrix(testX)
test_out_ridge <- testY

cv.out.ridge <- cv.glmnet(train_inp_ridge, train_out_ridge, alpha = 0)
plot(cv.out.ridge)

names(cv.out.ridge)
bestlam_ridge <- cv.out.ridge$lambda.min
bestlam_ridge #0.1118244

ridge.mod = glmnet(train_inp_ridge,train_out_ridge, alpha=0)
ridge.pred <- predict(ridge.mod, s= bestlam_ridge, type = "coefficients")
ridge.pred.train <- predict(ridge.mod, s = bestlam_ridge, newx = train_inp_ridge , type = "response")
ridge.pred.train = round(ridge.pred.train) 
ridge_train_error <- mean((ridge.pred.train - train_out_ridge)^2)  #train_error
ridge_train_error #0.05960151

ridge.pred.test <- predict(ridge.mod, s = bestlam_ridge, newx = test_inp_ridge , type = "response")
ridge.pred.test = round(ridge.pred.test) 
ridge_test_error <- mean((ridge.pred.test - test_out_ridge)^2)  #test_error
ridge_test_error #0.05925

######################################
## Lasso Regression 
######################################

train_inp_lasso <- as.matrix(train[,c(1:85)])
train_out_lasso <- train[,c(86)]
test_inp_lasso <- as.matrix(testX)
test_out_lasso <- testY

cv.out.lasso <- cv.glmnet(train_inp_lasso, train_out_lasso, alpha = 1)
plot(cv.out.lasso)

names(cv.out.lasso)
bestlam_lasso <- cv.out.lasso$lambda.min
bestlam_lasso #0.003184799

lasso.mod = glmnet(train_inp_lasso,train_out_lasso, alpha=1)
lasso.pred <- predict(lasso.mod, s= bestlam_lasso, type = "coefficients")
lasso.pred
lasso.pred.train <- predict(lasso.mod, s = bestlam_lasso, newx = train_inp_lasso , type = "response")
lasso.pred.train = round(lasso.pred.train) 

lasso_train_error <- mean((lasso.pred.train - train_out_lasso)^2)  #train_error
lasso_train_error #.05977327

lasso.pred.test <- predict(lasso.mod, s = bestlam_lasso, newx = test_inp_lasso , type = "response")
lasso.pred.test = round(lasso.pred.test) 
lasso_test_error <- mean((lasso.pred.test - test_out_lasso)^2)  #test_error
lasso_test_error #0.05975
