#Using any kind of weigths makes the MER value high.. 
rm(list = ls(all.names = TRUE))
setwd("/Users/bansal/Google Drive/Spring2014/DataMining/HW4/")
library(ggplot2)
library(kernlab)
library(DMwR)
library(ROCR)
load("/Users/bansal/Google Drive/Spring2014/DataMining/HW4/Data/spam.Rdata")
test <- subset(spam, test == "TRUE",select = -c(test) )
train <- subset(spam, test == "FALSE",select = -c(test) )

trainScaled <- train
testScaled <- test
for (j in 1:((dim(train)[2])-1) ) {
  testScaled[, j]= (test[, j] - mean(train[, j]))/sd(train[,j])
  trainScaled[, j] = (train[, j] - mean(train[, j]))/sd(train[,j])
}
train<-trainScaled
test<-testScaled

svm <- ksvm(data.matrix(train[,-58]),train$spam, scaled = FALSE, type = "C-svc", 
            kernel="vanilladot", class.weights = NULL, C=400)
summary(svm)
predValues<-predict(svm, data.matrix(test[,-58]))
tab = table(predValues,test$spam)
merScaled = 1 - sum(diag(tab))/sum(tab)

#Gaussian Radial Kernel Function 
svm_radial <- ksvm(data.matrix(train[,-58]),train$spam, type="C-svc", kernel="rbfdot", scaled = FALSE, 
                   class.weights = NULL, C = 400)
svm_radial
predValues<-predict(svm_radial, data.matrix(test[,-58]))
tab = table(predValues,test$spam)
merScaledRadial = 1 - sum(diag(tab))/sum(tab)

spamTran<-spam
spamTran[,1:57]<-log(spam[,1:57]+0.2)

test <- subset(spamTran, test == "TRUE",select = -c(test) )
train <- subset(spamTran, test == "FALSE",select = -c(test) )
svm <- ksvm(data.matrix(train[,-58]),train$spam, scaled = FALSE, type = "C-svc", 
            kernel="vanilladot", class.weights = NULL,C=300)
summary(svm)
predValues<-predict(svm, data.matrix(test[,-58]))
tab = table(predValues,test$spam)
merTran = 1 - sum(diag(tab))/sum(tab)

#Gaussian Radial Kernel Function 
svm_radial <- ksvm(data.matrix(train[,-58]),train$spam, type="C-svc", kernel="rbfdot", scaled = FALSE, 
                   class.weights = NULL, C=300)
svm_radial
predValues<-predict(svm_radial, data.matrix(test[,-58]))
tab = table(predValues,test$spam)
merTranRadial = 1 - sum(diag(tab))/sum(tab)

merScaled
merScaledRadial
merTran
merTranRadial

#No Cross no weigths 
#[1] 0.07096354
#[1] 0.07617188
#[1] 0.07096354
#[1] 0.07617188

#With cross with weigths 
#[1] 0.07096354
#[1] 0.07552083
#[1] 0.07096354
#[1] 0.07617188

#C=100
#[1] 0.06835938
#[1] 0.07421875
#[1] 0.06835938
#[1] 0.07486979

#C = 1000
#[1] 0.1145833
#[1] 0.08723958
#[1] 0.1145833
#[1] 0.08723958

#C = .001 
#[1] 0.1191406
#[1] 0.3873698
#[1] 0.1191406
#[1] 0.3873698

#not providing sigma (Model tunes it automatically)
#0.06705729
#0.08072917
#0.05729167
#0.05403646