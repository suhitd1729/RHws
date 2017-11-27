#######################################################
# This code is used to analyse the Question given 
# as per HW 3 - Question 2 in STA 545

# Suhit Datta
#######################################################

rm(list = ls())
library(caret)
df <- read.table("https://astro.temple.edu/~alan/DiabetesAndrews36_1.txt",header = FALSE)


diabetesdf <- subset(df , select= -c(V1,V2,V3,V4))
  
#V4 fourth column is the observation number, 
#V5 glucose.area, 
#V6  insulin.area,
#V7 SSPG,
#V8 relative.weight,
#V9 fasting.plasma.glucose
#V10 class number  

xyplot(V5 ~ V6, group=V10, data=diabetesdf,auto.key=list(space="right"),ylab = 'glucose.area',xlab = 'insulin.area')
xyplot(V5 ~ V7, group=V10, data=diabetesdf,auto.key=list(space="right"),ylab = 'glucose.area',xlab = 'SSPG')
xyplot(V5 ~ V8, group=V10, data=diabetesdf,auto.key=list(space="right"),ylab = 'glucose.area',xlab = 'relative.weight')
xyplot(V5 ~ V9, group=V10, data=diabetesdf,auto.key=list(space="right"),ylab = 'glucose.area',xlab = 'fasting.plasma.glucose')
xyplot(V6 ~ V7, group=V10, data=diabetesdf,auto.key=list(space="right"),ylab = 'insulin.area',xlab = 'SSPG')
xyplot(V6 ~ V8, group=V10, data=diabetesdf,auto.key=list(space="right"),ylab = 'insulin.area',xlab = 'relative.weight')
xyplot(V6 ~ V9, group=V10, data=diabetesdf,auto.key=list(space="right"),ylab = 'insulin.area',xlab = 'fasting.plasma.glucose')
xyplot(V7 ~ V8, group=V10, data=diabetesdf,auto.key=list(space="right"),ylab = 'SSPG',xlab = 'relative.weight')
xyplot(V7 ~ V9, group=V10, data=diabetesdf,auto.key=list(space="right"),ylab = 'SSPG',xlab = 'fasting.plasma.glucose')
xyplot(V8 ~ V9, group=V10, data=diabetesdf,auto.key=list(space="right"),ylab = 'relative.weight',xlab = 'fasting.plasma.glucose')

pairs(diabetesdf)

#part b
set.seed(1)
train <- sample(1:nrow(diabetesdf), .75*nrow(diabetesdf))
d_train <- diabetesdf[train,]
d_test <- diabetesdf[-train,]
d_true_train <- d_train[,6]
d_true_test <- d_test[,6]

####################################
## Linear Discriminant Analysis (LDA)
####################################
lda.fit <- lda(V10~., data = d_train)
lda.pred.train <- predict(lda.fit, newdata = d_train[,-6])
lda.pred.train <- as.numeric(lda.pred.train$class)

lda.pred.test <- predict(lda.fit, newdata = d_test[,-6])
lda.pred.test <- as.numeric(lda.pred.test$class)

# Compute the mean squared error
lda_train_error <- sum((d_true_train - lda.pred.train)^2)/length(d_true_train) 
lda_test_error <- sum((d_true_test - lda.pred.test)^2)/length(d_true_test)
lda_train_error #0.1018519
lda_test_error #0.1621622

lda_conf <- confusionMatrix(lda.pred.test, d_true_test)
names(lda_conf)
lda_conf$table


####################################
#   Quadratic Discriminant Analysis
####################################
qda.fit <- qda(V10~., data = d_train)
qda.pred.train <- predict(qda.fit, newdata = d_train[,-6])
qda.pred.train <- as.numeric(qda.pred.train$class)

qda.pred.test <- predict(qda.fit, newdata = d_test[,-6])
qda.pred.test <- as.numeric(qda.pred.test$class)


# Compute the mean squared error
qda_train_error <- sum((d_true_train - qda.pred.train)^2)/length(d_true_train) 
qda_test_error <- sum((d_true_test - qda.pred.test)^2)/length(d_true_test)
qda_train_error #0.02777778
qda_test_error #0.08108108

qda_conf <- confusionMatrix(qda.pred.test, d_true_test)
names(qda_conf)
qda_conf$table

#qda error lower

#part c
#V4 : glucose area = 0.98,
#V5 : insulin area = 122, 
#V6 : SSPG = 544,
#V7 : Relative weight = 186, 
#V8 : fasting plasma glucose = 184

data <- as.data.frame(t(c(0.98,122,544,186,184)))
names(data) <- names(d_train[,-6])

lda.pred <- predict(lda.fit, newdata = data)
lda.pred <- as.numeric(lda.pred$class)
lda.pred #2

qda.pred <- predict(qda.fit, newdata = data)
qda.pred <- as.numeric(qda.pred$class)
qda.pred #2
