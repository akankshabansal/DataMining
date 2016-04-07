rm(list = ls(all.names = TRUE))
setwd("/Users/bansal/Google Drive/Spring2014/DataMining/HW4/")
library(glmnet)
library(ggplot2)
library(caret)
library(ipred)
library(rpart)
library(foreach)

load("/Users/bansal/Google Drive/Spring2014/DataMining/HW4/Data/spam.Rdata")

#Part A 
test <- subset(spam, test == "TRUE", -c(test))
train <- subset(spam, test == "FALSE",-c(test))

trainScaled <- train
testScaled <- test
for (j in 1:57 ) {
  testScaled[, j]= (test[, j] - mean(train[, j]))/sd(train[,j])
  trainScaled[, j] = (train[, j] - mean(train[, j]))/sd(train[,j])
}
rm(train, test)
train<-trainScaled
test<-testScaled
training<-train
predictions<-NULL

predictions.train<-NULL 
predictions.test<-NULL 

predictions <- foreach(m=1:9,.combine=cbind) %do% {
    training_positions <- sample(nrow(train), size=floor(nrow(train)),replace = TRUE)
    trainsample<-training[training_positions,]
    matrix<-data.matrix(trainsample[,1:57])
    ridge <- cv.glmnet( matrix, trainsample$spam, family="binomial", 
                        standardize=FALSE, alpha=0, nfolds = 10, type.measure="class")
    predictions.test <- cbind(predictions.test, predict(ridge, data.matrix(test[,-58]), type="class"))
    predictions.train<- cbind (predictions.train, predict(ridge, data.matrix(train[,-58]), type="response"))
}

predicted <- test$spam
error <- 0
for(i in 1:(dim(predictions.test)[1]))
{
  value = 0;
  for(j in 1:9) {
    if(predictions.test[i,j]==1)
      value = value + 1;
    }
  if(value >= 5){
    predicted[i]=1;
  }else{
    predicted[i]=0;
  }
  
  if(predicted[i]!=test$spam[i])
    error = error + 1
}

error 
merENSEMBLE9 = error/length(test$spam)
merENSEMBLE9

predicted <- train$spam
error <- 0
for(i in 1:(dim(predictions.train)[1]))
{
  value = 0;
  for(j in 1:9) {
    if(predictions.train[i,j]==1)
      value = value + 1;
  }
  if(value >= 5){
    predicted[i]=1;
  }else{
    predicted[i]=0;
  }
  
  if(predicted[i]!=train$spam[i])
    error = error + 1
}

error 
merENSEMBLE9TRAIN = error/length(train$spam)
merENSEMBLE9TRAIN

#Part B 

rm(list = ls(all.names = TRUE))
setwd("/Users/bansal/Google Drive/Spring2014/DataMining/HW4/")
load("/Users/bansal/Google Drive/Spring2014/DataMining/HW4/Data/spam.Rdata")

#Standarised 
test <- subset(spam, test == "TRUE")
train <- subset(spam, test == "FALSE")

testScaled<-test
trainScaled<-train
for (j in 1:((dim(train)[2])-2) ) {
  testScaled[, j]= (test[, j] - mean(train[, j]))/sd(train[,j])
  trainScaled[, j] = (train[, j] - mean(train[, j]))/sd(train[,j])
}

predictions <- NULL
predictions.train <- NULL
matrix<-data.matrix(trainScaled[,-58])
train.spam <- data.matrix(trainScaled[,58])

ridge <- cv.glmnet( matrix, train.spam, family="binomial", 
                    standardize=FALSE, alpha=0, nfolds = 10, type.measure="class")
predictions <- cbind (predictions, predict(ridge, data.matrix(testScaled[,-58]), type="response"))
predictions.train <- cbind (predictions.train, predict(ridge, data.matrix(trainScaled[,-58]), type="response"))

lasso <- cv.glmnet(matrix, train.spam, family="binomial", 
                    standardize=FALSE, alpha=1, nfolds = 10, type.measure="class")
predictions <- cbind (predictions, predict(lasso, data.matrix(testScaled[,-58]), type="response"))
predictions.train <- cbind (predictions.train, predict(lasso, data.matrix(trainScaled[,-58]), type="response"))

unpenalized <- glm(spam ~ ., family="binomial", trainScaled)
predictions <- cbind(predictions, predict(unpenalized, testScaled, type="response"))
predictions.train <- cbind(predictions.train, predict(unpenalized, trainScaled, type="response"))

#Log transformed 
spamTran<-spam
spamTran[,1:57]<-log(spam[,1:57] + 0.2)
testTran <- subset(spamTran, test == "TRUE",select = -c(test) )
trainTran <- subset(spamTran, test == "FALSE",select = -c(test) )

matrix<-data.matrix(trainTran[,-58])
train.spam <- data.matrix(trainTran[,58])

ridge <- cv.glmnet( matrix, train.spam, family="binomial", 
                    standardize=FALSE, alpha=0, nfolds = 10, type.measure="class")
predictions <- cbind (predictions, predict(ridge, data.matrix(testTran[,-58]), type="response"))
predictions.train <- cbind (predictions.train, predict(ridge, data.matrix(trainTran[,-58]), type="response"))

lasso <- cv.glmnet(matrix, train.spam, family="binomial", 
                   standardize=FALSE, alpha=1, nfolds = 10, type.measure="class")
predictions <- cbind (predictions, predict(lasso, data.matrix(testTran[,-58]), type="response"))
predictions.train <- cbind (predictions.train, predict(lasso, data.matrix(trainTran[,-58]), type="response"))

unpenalized <- glm(spam ~ ., family="binomial", trainTran)
predictions <- cbind(predictions, predict(unpenalized, testTran, type="response"))
predictions.train <- cbind(predictions.train, predict(unpenalized, trainTran, type="response"))

#Binary 
testBin<-test
trainBin<-train

trainBin[,1:57] <- ifelse(train[,1:57] > 0,1,0)
testBin[,1:57] <- ifelse(test[,1:57] > 0,1,0)

matrix<-data.matrix(trainBin[,-58])
train.spam <- data.matrix(trainBin[,58])

ridge <- cv.glmnet( matrix, train.spam, family="binomial", 
                    standardize=FALSE, alpha=0, nfolds = 10, type.measure="class")
predictions <- cbind (predictions, predict(ridge, data.matrix(testBin[,-58]), type="response"))
predictions.train <- cbind (predictions.train, predict(ridge, data.matrix(trainBin[,-58]), type="response"))

lasso <- cv.glmnet(matrix, train.spam, family="binomial", 
                   standardize=FALSE, alpha=1, nfolds = 10, type.measure="class")
predictions <- cbind (predictions, predict(lasso, data.matrix(testBin[,-58]), type="response"))
predictions.train <- cbind (predictions.train, predict(lasso, data.matrix(trainBin[,-58]), type="response"))

unpenalized <- glm(spam ~ ., family="binomial", trainBin)
predictions <- cbind(predictions, predict(unpenalized, testBin, type="response"))
predictions.train <- cbind(predictions.train, predict(unpenalized, trainBin, type="response"))


threshold <- dim(subset(train, train.spam == "1"))[1]/dim(train)[1]

predicted <- test$spam
error <- 0
for(i in 1:(dim(predictions)[1]))
{
  mean = 0;
  for(j in 1:9) {
    mean = mean + predictions[i,j];
  }
  mean = mean / 9;
  if(mean > threshold){
    predicted[i]=1;
  }else{
    predicted[i]=0;
  }
  
  if(predicted[i]!=test$spam[i])
    error = error + 1
}

error 
merENSEMBLEMIX = error/length(test$spam)
merENSEMBLEMIX


predicted <- train$spam
error <- 0
for(i in 1:(dim(predictions.train)[1]))
{
  mean = 0;
  for(j in 1:9) {
    mean = mean + predictions.train[i,j];
  }
  mean = mean / 9;
  if(mean > threshold){
    predicted[i]=1;
  }else{
    predicted[i]=0;
  }
  
  if(predicted[i]!=train$spam[i])
    error = error + 1
}

error 
merENSEMBLEMIXTRAIN = error/length(train$spam)
merENSEMBLEMIXTRAIN

