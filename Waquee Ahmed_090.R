install.packages("titanic")
install.packages("rpart.plot")
install.packages("randomForest")
install.packages("DAAG")
library(titanic)
library(rpart.plot)
library(gmodels)
library(Hmisc)
library(pROC)
library(ResourceSelection)
library(car)
library(caret)
library(dplyr)
library(InformationValue)
library(rpart)
library(randomForest)
library("DAAG")

getwd()
setwd("C:/Waquee/AMMA 2017/Assignment 2")

titanic_training_2<-read.csv('train.csv')
titanic_training<-titanic_training_2
titanic_training_3 <- read.csv('train.csv')

titanic_test_const <-read.csv('test-3.csv')

set.seed(1234)
titanic_training$rand <- runif(nrow(titanic_training))
titanic_training_start <- titanic_training[titanic_training$rand <= 0.7,]
titanic_test_start <- titanic_training[titanic_training$rand > 0.7,]

CrossTable(titanic_training$Survived)
titanic_training <- titanic_training[!apply(titanic_training[,c("Pclass", "Sex", "SibSp", "Parch", "Fare", "Age")], 1, anyNA),]
titanic_training_NA_allcols <- titanic_training_2[!apply(titanic_training_2[,c("Pclass", "Sex", "SibSp", "Parch", "Fare", "Age")], 1, anyNA),]
nrow(titanic_training_2)

mean_age = mean(titanic_training_2$Age)
titanic_training_average <- titanic_training_start
titanic_training_average2 <- titanic_training_start
titanic_training_average$Age[is.na(titanic_training_average$Age)] = mean(titanic_training_average$Age, na.rm = TRUE)
titanic_training_average2$Age[is.na(titanic_training_average2$Age)] = mean(titanic_training_average2$Age, na.rm = TRUE)

full.model.titanic.mean <- glm(formula = Survived ~ Pclass + Sex + SibSp + Parch + Fare + Age,
                               data=titanic_training_average, family = binomial) #family = binomial implies that the type of regression is logistic

fit.train.mean <- lm(formula = Survived ~ Pclass + Sex + SibSp + Parch + Fare + Age,
                     data=titanic_training_average2) #family = binomial implies that the type of regression is logistic
summary(fit.train.mean)

vif(fit.train.mean) 

titanic_training_average$Parch<-NULL
full.model.titanic.mean <- glm(formula = Survived ~ Pclass + Sex + SibSp + Fare + Age,
                               data=titanic_training_average, family = binomial) #family = binomial implies that the type of regression is logistic
summary(full.model.titanic.mean)

titanic_training_average$Fare<-NULL
full.model.titanic.mean <- glm(formula = Survived ~ Pclass + Sex + SibSp + Age,
                               data=titanic_training_average, family = binomial) #family = binomial implies that the type of regression is logistic
summary(full.model.titanic.mean)

titanic_training_average$prob = predict(full.model.titanic.mean, type=c("response"))
titanic_training_average$Survived.pred = ifelse(titanic_training_average$prob>=.5,'pred_yes','pred_no')
table(titanic_training_average$Survived.pred,titanic_training_average$Survived)

nrow(titanic_test)
nrow(titanic_test2_average)
titanic_test2_average <- titanic_test_start

titanic_test2_average$Age[is.na(titanic_test2_average$Age)] = mean(titanic_test2_average$Age, na.rm = TRUE)

titanic_test2_average$prob = predict(full.model.titanic.mean, newdata=titanic_test2_average, type=c("response"))
titanic_test2_average$Survived.pred = ifelse(titanic_test2_average$prob>=.5,'pred_yes','pred_no')
table(titanic_test2_average$Survived.pred,titanic_test2_average$Survived)

Kfold_func <- function(dataset,formula,family,k)
{
  object <- glm(formula=formula, data=dataset, family = family)
  CVbinary(object, nfolds= k, print.details=TRUE)
}

MeanSquaredError_func <- function(dataset,formula)
{
  LM_Object <- lm(formula=formula, data=dataset)
  LM_Object_sum <-summary(LM_Object)
  MSE <- mean(LM_Object_sum$residuals^2)
  print("Mean squared error")
  print(MSE)
}

Kfoldobj <- Kfold_func(titanic_training_average,Survived ~ Pclass + Sex + SibSp + Age,binomial,10)

MSE_Train <-MeanSquaredError_func(titanic_training_average,Survived ~ Pclass + Sex + SibSp + Age)

table(titanic_training_average$Survived,round(Kfoldobj$cvhat))
print("Estimate of Accuracy")
print(Kfoldobj$acc.cv)

Kfoldobj.test <- Kfold_func(titanic_test2_average,Survived ~ Pclass + Sex + SibSp + Age,binomial,10)

MSE_Test <-MeanSquaredError_func(titanic_test2_average,Survived ~ Pclass + Sex + SibSp + Age)

table(titanic_test2_average$Survived,round(Kfoldobj.test$cvhat))
print("Estimate of Accuracy")
print(Kfoldobj.test$acc.cv)