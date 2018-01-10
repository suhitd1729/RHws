#######################################################
# This code is used to analyse the Dataset given 
# as per HW 4 - Question 7

# Suhit Datta
#######################################################

rm (list=ls())

library(e1071)
library(ISLR)
data(OJ)

set.seed(123)
train<-sample(1:nrow(OJ),.70*nrow(OJ))
oj.train<-OJ[train,]
oj.test<-OJ[-train,]

# Part a

lintest.err<-c()
lintrain.err<-c()
for(i in c(0.01,0.05,0.1,0.5,1,5,7,10)){
  tune.model.linear<-tune(svm,Purchase~.,data=oj.train,kernel="linear",ranges=list(cost=i))
  y.hat.test.linear<-predict(tune.model.linear$best.model,newdata=oj.test)
  y.ture.test<-oj.test$Purchase
  
  test.error<-length(which(y.hat.test.linear!=y.ture.test))/length(y.ture.test)
  lintest.err<-c(lintest.err,test.error)
  
  y.hat.train.linear<-predict(tune.model.linear$best.model,newdata=oj.train)
  y.true.train<-oj.train$Purchase
  
  train.error<-length(which(y.hat.train.linear!=y.true.train))/length(y.true.train)
  lintrain.err<-c(lintrain.err,train.error)
}

lintest.err  
lintrain.err 

cost<-c(0.01,0.05,0.1,0.5,1,5,7,10)

#x11()
par(mfrow=c(1,2))
plot(cost,lintest.err,type="b",lty=2,col = "blue",xlab = "cost",main="linear test error ",ylab="test error")
plot(cost,lintrain.err,type="b",lty=1,col="red",xlab = "cost",main=" linear train error",ylab="train error")

linear.err.table<-cbind(lintest.err,lintrain.err,cost)
linear.err.table

# Part b

radtest.err.1<-c()
radtrain.err.1<-c()
for(i in c(0.01,0.05,0.1,0.5,1,5,7,10)){
  tune.model.rad.1<-tune(svm,Purchase~.,data=oj.train,kernel="radial",ranges=list(cost=i))
  
  y.hat.test.rad.1<-predict(tune.model.rad.1$best.model,newdata=oj.test)
  y.ture.test<-oj.test$Purchase
  
  test.error.1<-length(which(y.hat.test.rad.1!=y.ture.test))/length(y.ture.test)
  radtest.err.1<-c(radtest.err.1,test.error.1)
  
  y.hat.train.rad.1<-predict(tune.model.rad.1$best.model,newdata=oj.train)
  y.true.train<-oj.train$Purchase
  
  train.error.1<-length(which(y.hat.train.rad.1!=y.true.train))/length(y.true.train)
  radtrain.err.1<-c(radtrain.err.1,train.error.1)
}

radtest.err.1 
radtrain.err.1 

cost<-c(0.01,0.05,0.1,0.5,1,5,7,10)

#par(mfrow=c(1,2))
plot(cost,radtest.err.1,type="b",lty=2,col = "blue",xlab = "cost",main="radial test error",ylab="test error")
plot(cost,radtrain.err.1,type="b",lty=1,col="red",xlab = "cost",main="radial train error",ylab="train error")

rad.err.1.table<-cbind(radtest.err.1,radtrain.err.1,cost)
rad.err.1.table

polytest.err<-c()
polytrain.err<-c()
for(i in c(0.01,0.05,0.1,0.5,1,5,7,10)){
  tune.model.polynomial<-tune(svm,Purchase~.,data=oj.train,degree=2,kernel="polynomial",ranges=list(cost=i))
  
  y.hat.test.polynomial<-predict(tune.model.polynomial$best.model,newdata=oj.test)
  y.ture.test<-oj.test$Purchase
  
  test.error.2<-length(which(y.hat.test.polynomial!=y.ture.test))/length(y.ture.test)
  polytest.err<-c(polytest.err,test.error.2)
  
  y.hat.train.polynomial<-predict(tune.model.polynomial$best.model,newdata=oj.train)
  y.true.train<-oj.train$Purchase
  
  train.error.2<-length(which(y.hat.train.polynomial!=y.true.train))/length(y.true.train)
  polytrain.err<-c(polytrain.err,train.error.2)
}

polytest.err 
polytrain.err 

cost<-c(0.01,0.05,0.1,0.5,1,5,7,10)


#x11()
par(mfrow=c(1,2))
plot(cost,polytest.err,type="b",lty=2,col = "blue",xlab = "cost",main="polynomial test error",ylab="test error")
plot(cost,polytrain.err,type="b",lty=1,col="red",xlab = "cost",main="polynomial train error",ylab="train error")

polynomial.err.table<-cbind(polytest.err,polytrain.err,cost)
polynomial.err.table

