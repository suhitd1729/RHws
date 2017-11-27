#######################################################
# This code is used to analyse the Question given 
# as per HW 3 - Question 4 in STA 545

# Suhit Datta
#######################################################

rm(list=ls())
library(caret)
library(boot)
set.seed(1)
x = rnorm(100)
y<- x-2*x^2+rnorm(100)

plot(x,y , main='Plot of X vs Y',xlab ='X coordinate values', ylab = 'Y coordinate values')

#creating a dataframe
xyValues <- data.frame(x, y)

#Y = linear model 
glmfit_1 = glm(y~x)
summary((glmfit_1))
cv.glm(xyValues,glmfit_1)$delta[1]
#7.288162
coeff1 <- glmfit_1$coefficients
coeff1

#Y = quadratic
glmfit_2 <- glm(y ~ poly(x, 2))
summary(glmfit_2)
cv.glm(xyValues,glmfit_2)$delta[1]
#0.9374236
coeff2 <- glmfit_2$coefficients
coeff2

#Y = cubic
glmfit_3 <- glm(y ~ poly(x, 3))
summary(glmfit_3)
cv.glm(xyValues,glmfit_3)$delta[1]
#0.9566218
coeff3 <- glmfit_3$coefficients
coeff3

#Y = 4th powered 
glmfit_4 <- glm(y ~ poly(x, 4))
summary(glmfit_4)
cv.glm(xyValues,glmfit_4)$delta[1]
#0.9539049
coeff4 <- glmfit_4$coefficients
coeff4
#least square error is obtained for 2
