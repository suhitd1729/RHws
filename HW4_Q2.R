#######################################################
# This code is used to analyse the Dataset given 
# as per HW 4 - Question 2

# Suhit Datta
#######################################################
rm(list = ls())
setwd('C:/DragonBallZ/Fall2017/Prof.Rachel/R_directory')
library("rpart")
library("rpart.plot")

Wdata <- read.delim("wine_data.txt", sep = ",", header= FALSE)
Wdata$V1 = as.factor(Wdata$V1)

set.seed(123)
train <- sample(1:nrow(Wdata), .70*nrow(Wdata))
wine_train <- Wdata[train,]
wine_test <- Wdata[-train,]



#####################################
#TREES
#####################################
model.control <- rpart.control(minsplit = 5, xval = 10, cp = 0)
#train 
wTModel <- rpart(V1~., data = wine_train, method = "class", control = model.control)
wTModel
plot(wTModel, uniform = T, compress = T)
text(wTModel, cex = 0.6,pretty=T)
prp(wTModel,  fallen.leaves = FALSE, type=4, extra=1, varlen=0, faclen=0, yesno.yshift=-1)


#Count number of Test data in nodes
nodes_wine <- wTModel
nodes_wine$frame$yval = as.numeric(rownames(nodes_wine$frame))
test_nodes <- predict(nodes_wine, wine_test, type="vector")
testNodeDf = data.frame(rowNum = c(1:length(test_nodes)),test_nodes)
testNodeData = data.frame(aggregate(rowNum~test_nodes,data = testNodeDf,FUN = length))


#Count number of train data in nodes
nodes_wine <- wTModel
nodes_wine$frame$yval = as.numeric(rownames(nodes_wine$frame))
train_nodes <- predict(nodes_wine, wine_train, type="vector")
trainNodeDf = data.frame(rowNum = c(1:length(train_nodes)),train_nodes)
trainNodeData = data.frame(aggregate(rowNum~train_nodes,data = trainNodeDf,FUN = length))


wineTreePred_test <- predict(wTModel, newdata = wine_test,type = 'class')
wineTreePred_train <- predict(wTModel, newdata = wine_train, type='class')


Tree_test_err <- mean(wineTreePred_test != wine_test$V1)
Tree_train_err <- mean(wineTreePred_train != wine_train$V1)

Tree_test_err
Tree_train_err
testNodeData
trainNodeData
