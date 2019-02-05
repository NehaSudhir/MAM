#RIDGE REGRESSION

library(MASS)
library(mlbench)
library(caret)
library(psych)
library(glmnet)
library(packrat)
library(ISLR)
library(dplyr)
traindata <- read.csv(file.choose(), header = T)
str(traindata)

par(mar=c(0.05,0.05,0.05,0.05))
pairs.panels(traindata)

set.seed(1234)
ind = sample(2,nrow(traindata), replace = T, prob = c(0.7,0.3))

train1 = traindata[ind==1,] #saving data in index 1
train1
test = traindata[ind==2,]
test
# Ridge regression
lambdas = seq(.0001, 1, length = 5)
lambdas
SP = na.omit(SalesPrice)
ridge = train(SP~. , data =  traindata, method ='glmnet',tuneGrid =expand.grid(alpha = 0, lambda = lambdas))
summary(ridge)

# Plot Results
plot(ridge$finalModel, xvar = "lambda", label = T)
plot(ridge$finalModel, xvar = 'dev', label = T)
plot(varImp(ridge,scale = T))
plot(ridge)

#SIMPLE LINEAR REGRESSION
library(caret)
library(mlbench)
#strength file
d1=read.csv(file.choose())
head(d1)
str(d1)
mydata=d1[ ,2:81]
OverallQuality=d1$OverallQual
SalesPrice=d1$SalePrice
plot(SalesPrice,OverallQuality)
cor(SalesPrice,OverallQuality)


pairs.panels(SalesPrice)

as.numeric(d1$SaleType, d1$SaleCondition, d1$Functional)
cor(d1)

#simple linear regression
Reg=lm(SalesPrice~., data = d1)
Reg
summary(Reg)
mad=mean(abs(SalesPrice))
mad
mse=mean(SalesPrice^2)
mse
rmse=sqrt(mse)
rmse
