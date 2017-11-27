#######################################################
# This code is used to analyse the Question given 
# as per HW 2 - Question 3 in STA 545

# Suhit Datta
#######################################################


rm(list=ls())

library(xlsx)
library(leaps)
#y = x * beta + error
set.seed(123)
#generating beta [20*1 Matrix] 
beta <- runif(20,0,1) #randomly generating 20 values between 0 and 1 part of a uniform distribution
beta
typeof(beta)
set.seed(123)
k <- sample(1:20, 5, replace=F)
k
for (i in k){
  beta[k] = 0 
} 

betaMatrix <-as.matrix(beta)
#write.xlsx(betaMatrix, "C:\\DragonBallZ\\Fall2017\\Prof.Rachel\\R_directory\\betaMatrix.xlsx")

#generating epsilon [1000*1 Matrix]
set.seed(123)
epsilon <- runif(1000,0,1)
errorMatrix <- as.matrix(epsilon)
#write.xlsx(errorMatrix, "C:\\DragonBallZ\\Fall2017\\Prof.Rachel\\R_directory\\errorMatrix.xlsx")

#generating values for x 
#xMatrix <-  as.matrix(replicate(20,sample(c(1:100), size=1000, replace=TRUE)))

xMatrix=matrix(rnorm(1000*20),nrow=1000,ncol=20)

#write.xlsx(xMatrix, "C:\\DragonBallZ\\Fall2017\\Prof.Rachel\\R_directory\\xMatrix.xlsx")
yMatrix <- xMatrix %*% betaMatrix + errorMatrix
#write.xlsx(yMatrix, "C:\\DragonBallZ\\Fall2017\\Prof.Rachel\\R_directory\\yMatrix.xlsx")


y <- as.data.frame(yMatrix)
x <- as.data.frame(xMatrix)
b <- as.data.frame(betaMatrix)
e <- as.data.frame(errorMatrix)
data <- cbind(y,x)
set.seed(123)

d <- sample.int(n = nrow(data), size = floor(.1*nrow(data)), replace = F)
ytrain <- y[d,] 
ytest  <- y[-d, ]

xtrain <- x[d,] 
xtest  <- x[-d, ]

trainData <- cbind(ytrain,xtrain)
names(trainData)
regfit.full <- regsubsets(ytrain~., data = trainData, nbest = 1, nvmax = 20, method = "exhaustive")
my_sum <- summary(regfit.full)
my_sum

plot(my_sum$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
plot(my_sum$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
plot(my_sum$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(my_sum$adjr2, xlab = "Number of Variables", ylab = "Adjusted R^2", type = "l")

#1 variable
linmod1 <- lm(ytrain~V20,data = trainData)
summary(linmod1)
y_pred_train_1 <- predict(linmod1,newData = xtrain)
train_mse_1 <- mean((y_pred_train_1-ytrain)^2)
train_mse_1 #4.630406

y_pred_test_1 <- predict(linmod1,newData = xtest)
test_mse_1 <- mean((y_pred_test_1-ytest)^2)
test_mse_1 #7.072964


#2 variable
linmod2 <- lm(ytrain~V5+V14,data = trainData)
y_pred_train_2 <- predict(linmod2,newData = xtrain)
train_mse_2 <- mean((y_pred_train_2-ytrain)^2)
train_mse_2 #3.80346

y_pred_test_2 <- predict(linmod2,newData = xtest)
test_mse_2 <- mean((y_pred_test_2-ytest)^2)
test_mse_2 #7.866602


#3 variable
linmod3 <- lm(ytrain~V5+V11+V14,data = trainData)
y_pred_train_3 <- predict(linmod3,newData = xtrain)
train_mse_3 <- mean((y_pred_train_3-ytrain)^2)
train_mse_3 #3.029052

y_pred_test_3 <- predict(linmod3,newData = xtest)
test_mse_3 <- mean((y_pred_test_3-ytest)^2)
test_mse_3 #8.549664


#4 variable
linmod4 <- lm(ytrain~V4+V5+V11+V14,data = trainData)
y_pred_train_4 <- predict(linmod4,newData = xtrain)
train_mse_4 <- mean((y_pred_train_4-ytrain)^2)
train_mse_4 #2.524685

y_pred_test_4 <- predict(linmod4,newData = xtest)
test_mse_4 <- mean((y_pred_test_4-ytest)^2)
test_mse_4 #8.849358


#5 variable
linmod5 <- lm(ytrain~V4+V5+V11+V14+V20,data = trainData)
y_pred_train_5 <- predict(linmod5,newData = xtrain)
train_mse_5 <- mean((y_pred_train_5-ytrain)^2)
train_mse_5 #2.02882

y_pred_test_5 <- predict(linmod5,newData = xtest)
test_mse_5 <- mean((y_pred_test_5-ytest)^2)
test_mse_5 #9.293541


#6 variable
linmod6 <- lm(ytrain~V4+V5+V9+V11+V14+V20,data = trainData)
y_pred_train_6 <- predict(linmod6,newData = xtrain)
train_mse_6 <- mean((y_pred_train_6-ytrain)^2)
train_mse_6 #1.532927

y_pred_test_6 <- predict(linmod6,newData = xtest)
test_mse_6 <- mean((y_pred_test_6-ytest)^2)
test_mse_6 #9.665741


#7 variable
linmod7 <- lm(ytrain~V4+V5+V9+V11+V13+V14+V20,data = trainData)
y_pred_train_7 <- predict(linmod7,newData = xtrain)
train_mse_7 <- mean((y_pred_train_7-ytrain)^2)
train_mse_7 #1.228811

y_pred_test_7 <- predict(linmod7,newData = xtest)
test_mse_7 <- mean((y_pred_test_7-ytest)^2)
test_mse_7 #9.845995

#8 variable
linmod8 <- lm(ytrain~V2+V4+V5+V9+V11+V13+V14+V20,data = trainData)
y_pred_train_8 <- predict(linmod8,newData = xtrain)
train_mse_8 <- mean((y_pred_train_8-ytrain)^2)
train_mse_8 #0.8342664

y_pred_test_8 <- predict(linmod8,newData = xtest)
test_mse_8 <- mean((y_pred_test_8-ytest)^2)
test_mse_8 #10.3667

#9 variable
linmod9 <- lm(ytrain~V2+V4+V5+V7+V9+V11+V13+V14+V20,data = trainData)
y_pred_train_9 <- predict(linmod9,newData = xtrain)
train_mse_9 <- mean((y_pred_train_9-ytrain)^2)
train_mse_9 #0.6669305

y_pred_test_9 <- predict(linmod9,newData = xtest)
test_mse_9 <- mean((y_pred_test_9-ytest)^2)
test_mse_9 #10.68864


#10 variable
linmod10 <- lm(ytrain~V2+V3+V4+V5+V7+V9+V11+V13+V14+V20,data = trainData)
y_pred_train_10 <- predict(linmod10,newData = xtrain)
train_mse_10 <- mean((y_pred_train_10-ytrain)^2)
train_mse_10 #0.5028939

y_pred_test_10 <- predict(linmod10,newData = xtest)
test_mse_10 <- mean((y_pred_test_10-ytest)^2)
test_mse_10 #10.93955

#11 variable
linmod11 <- lm(ytrain~V2+V3+V4+V5+V7+V9+V10+V11+V13+V14+V20,data = trainData)
y_pred_train_11 <- predict(linmod11,newData = xtrain)
train_mse_11 <- mean((y_pred_train_11-ytrain)^2)
train_mse_11 #0.3377517

y_pred_test_11 <- predict(linmod11,newData = xtest)
test_mse_11 <- mean((y_pred_test_11-ytest)^2)
test_mse_11 # 10.95312

#12 variable
linmod12 <- lm(ytrain~V2+V3+V4+V5+V7+V9+V10+V11+V12+V13+V14+V20,data = trainData)
y_pred_train_12 <- predict(linmod12,newData = xtrain)
train_mse_12 <- mean((y_pred_train_12-ytrain)^2)
train_mse_12 #0.219437

y_pred_test_12 <- predict(linmod12,newData = xtest)
test_mse_12 <- mean((y_pred_test_12-ytest)^2)
test_mse_12 #11.1391


#13 variable
linmod13 <- lm(ytrain~.-V6-V8-V15-V16-V17-V18-V19,data = trainData)
y_pred_train_13 <- predict(linmod13,newData = xtrain)
train_mse_13 <- mean((y_pred_train_13-ytrain)^2)
train_mse_13 #0.1277248

y_pred_test_13 <- predict(linmod13,newData = xtest)
test_mse_13 <- mean((y_pred_test_13-ytest)^2)
test_mse_13 #11.22897

#14 variable
linmod14 <- lm(ytrain~.-V6-V8-V15-V16-V17-V18,data = trainData)
y_pred_train_14 <- predict(linmod14,newData = xtrain)
train_mse_14 <- mean((y_pred_train_14-ytrain)^2)
train_mse_14 #5.307452

y_pred_test_14 <- predict(linmod14,newData = xtest)
test_mse_14 <- mean((y_pred_test_14-ytest)^2)
test_mse_14 #6.43285


#15 variable
linmod15 <- lm(ytrain~.-V6-V8-V15-V16-V18,data = trainData)
y_pred_train_15 <- predict(linmod15,newData = xtrain)
train_mse_15 <- mean((y_pred_train_15-ytrain)^2)
train_mse_15 #0.06538578

y_pred_test_15 <- predict(linmod15,newData = xtest)
test_mse_15 <- mean((y_pred_test_15-ytest)^2)
test_mse_15 #11.29675

#16 variable
linmod16 <- lm(ytrain~.-V6-V8-V15-V16,data = trainData)
y_pred_train_16 <- predict(linmod16,newData = xtrain)
train_mse_16 <- mean((y_pred_train_16-ytrain)^2)
train_mse_16 #0.06390304

y_pred_test_16 <- predict(linmod16,newData = xtest)
test_mse_16 <- mean((y_pred_test_16-ytest)^2)
test_mse_16 #11.28697

#17 variable
linmod17 <- lm(ytrain~.-V6-V15-V16,data = trainData)
y_pred_train_17 <- predict(linmod17,newData = xtrain)
train_mse_17 <- mean((y_pred_train_17-ytrain)^2)
train_mse_17 #0.06264742

y_pred_test_17 <- predict(linmod17,newData = xtest)
test_mse_17 <- mean((y_pred_test_17-ytest)^2)
test_mse_17 #11.28889

#18 variable
linmod18 <- lm(ytrain~.-V15-V16,data = trainData)
y_pred_train_18 <- predict(linmod18,newData = xtrain)
train_mse_18 <- mean((y_pred_train_18-ytrain)^2)
train_mse_18 #0.06178818

y_pred_test_18 <- predict(linmod18,newData = xtest)
test_mse_18 <- mean((y_pred_test_18-ytest)^2)
test_mse_18 #11.28764

#19 variable
linmod19 <- lm(ytrain~.-V16,data = trainData)
y_pred_train_19 <- predict(linmod19,newData = xtrain)
train_mse_19 <- mean((y_pred_train_19-ytrain)^2)
train_mse_19 #0.06127865

y_pred_test_19 <- predict(linmod19,newData = xtest)
test_mse_19 <- mean((y_pred_test_19-ytest)^2)
test_mse_19 #11.28769

#20 variable
linmod20 <- lm(ytrain~.,data = trainData)
y_pred_train_20 <- predict(linmod20,newData = xtrain)
train_mse_20 <- mean((y_pred_train_20-ytrain)^2)
train_mse_20 #0.06087767

y_pred_test_20 <- predict(linmod20,newData = xtest)
test_mse_20 <- mean((y_pred_test_20-ytest)^2)
test_mse_20 #11.28468

t <- c(1:20)
trainErrorArray <- c(train_mse_1,train_mse_2,train_mse_3,train_mse_4,train_mse_5,train_mse_6,train_mse_7,train_mse_8,train_mse_9,train_mse_10,train_mse_11,train_mse_12,train_mse_13,train_mse_14,train_mse_15,train_mse_16,train_mse_17,train_mse_18,train_mse_19,train_mse_20)
trainErrorArray
testErrorArray <- c(test_mse_1,test_mse_2,test_mse_3,test_mse_4,test_mse_5,test_mse_6,test_mse_7,test_mse_8,test_mse_9,test_mse_10,test_mse_11,test_mse_12,test_mse_13,test_mse_14,test_mse_15,test_mse_16,test_mse_17,test_mse_18,test_mse_19,test_mse_20)

plot(t,trainErrorArray,xlab = "Model Size",ylab = "MSE", main = "Model size vs MSE for Training Data" , type = "b")

plot(t,testErrorArray,xlab = "Model Size",ylab = "MSE", main = "Model size vs MSE for Test Data" , type = "b")

summary(linmod14)
TrainErrorDF <- as.data.frame(trainErrorArray)
TestErrorDF <- as.data.frame(testErrorArray)
tDF <- as.data.frame(t)

epsilonMean <- mean(epsilon)
epsilonMean
BetaMean <- mean(beta)
BetaMean

