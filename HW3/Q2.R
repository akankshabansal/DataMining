rm(list = ls())
library(MASS)
library(hydroGOF)
setwd("/Users/bansal/Google Drive/Spring2014/DataMining/HW3/")
train = read.csv("~/Google Drive/Spring2014/DataMining/HW3/HW3Data/Machine1.csv")

#regression 
model<- lm(PRP ~ MYCT + MMIN + CACH + CHMIN + CHMAX, data=train)
summary(model)
mse(model$fitted.values,train$PRP)
sqrt(mse(model$fitted.values,train$PRP))

testModel<-predict.lm(model, newdata = test)
summary(testModel)
mse(testModel,test$area)


dev.copy(png,'Q1plot2.png')
dev.off()


