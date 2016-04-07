#install.packages("ridge")
rm(list = ls(all.names = TRUE))
setwd("/Users/bansal/Google Drive/Spring2014/DataMining/HW4/")
library(glmnet)
library(ggplot2)
load("/Users/bansal/Google Drive/Spring2014/DataMining/HW4/Data/spam.Rdata")

#Part A 
test <- subset(spam, test == "TRUE")
train <- subset(spam, test == "FALSE")

scaledTrain <- train[,1:58]
scaledTest <- test[,1:58]

for (j in 1:57)
{
  scaledTrain[,j] = (train[,j] - mean(train[,j]))/sd(train[,j])
  scaledTest[,j] = (test[,j] - mean(train[,j]))/sd(train[,j])
}
rm(test,train)
test<-scaledTest
train<-scaledTrain
x <- data.matrix(train[,-58])
y <- train[,58]
matrix_test <- data.matrix(test[,-58])
y.test<-test[,58]

ridge <- cv.glmnet( x, y ,family="binomial", standardize=FALSE, alpha=0, nfolds = 10,type.measure="class")

predValues<-predict(ridge, x, type="response")
correct <- 0
for(i in 1:length(predValues))
{
  if((predValues[i]>0.5&&y[i]==1)||(predValues[i]<0.5&&y[i]==0))
    correct = correct + 1
}
merScalTrain <- 1 - (correct/length(predValues))

predValues<-predict(ridge, matrix_test, type="response")

correct <- 0
for(i in 1:length(predValues))
{
  if((predValues[i]>0.5&&y.test[i]==1)||(predValues[i]<0.5&&y.test[i]==0))
    correct = correct + 1
}
merScalTest <- 1 - (correct/length(predValues))

plot.roc <- NULL;
plot.lift <- NULL;

pred <- prediction(predValues, test[,58])
roc.perf <- performance(pred,"tpr","fpr")
tpr.points <- attr(roc.perf, "y.values")[[1]]
fpr.points <- attr(roc.perf, "x.values")[[1]]
aucStan <- attr(performance(pred, "auc"), "y.values")[[1]]
plot(roc.perf, col=rainbow(10))
plot.roc <- rbind(plot.roc, data.frame(fpr=fpr.points, tpr=tpr.points, model="Scaled"));
#plot.roc = rbind(plot.roc,data.frame(Model="standardized", Target=tpr.points, Pred=fpr.points))

lift.perf <- performance(pred, "lift", "rpp")
x.points <- attr(lift.perf, "x.values")[[1]]
y.points <- attr(lift.perf, "y.values")[[1]]
plot(lift.perf, col=rainbow(10))
plot.lift = rbind(plot.lift,data.frame(Model="standardized", Target=x.points, Pred=y.points))



#Part B 
rm(test,train)
spamTran<-spam
spamTran[,1:57]<-log(spam[,1:57]+0.2)

test <- subset(spamTran, test == "TRUE",select = -c(test) )
train <- subset(spamTran, test == "FALSE",select = -c(test) )
x <- data.matrix(train[,-58])
y <- train[,58]
matrix_test <- data.matrix(test[,-58])
y.test<-test[,58]

ridge <- cv.glmnet( x,y ,family="binomial", standardize=FALSE, alpha=0, nfolds = 10,type.measure="class")
predValues<-predict(ridge, x, type="response")
correct <- 0
for(i in 1:length(predValues))
{
  if((predValues[i]>0.5&&y[i]==1)||(predValues[i]<0.5&&y[i]==0))
    correct = correct + 1
}
merTranTrain <- 1 - (correct/length(predValues))

predValues<-predict(ridge, matrix_test, type="response")

correct <- 0
for(i in 1:length(predValues))
{
  if((predValues[i]>0.5&&y.test[i]==1)||(predValues[i]<0.5&&y.test[i]==0))
    correct = correct + 1
}
merTranTest <- 1 - (correct/length(predValues))


pred <- prediction(predValues, test[,58])
roc.perf <- performance(pred,"tpr","fpr")
tpr.points <- attr(roc.perf, "y.values")[[1]]
fpr.points <- attr(roc.perf, "x.values")[[1]]
aucTran <- attr(performance(pred, "auc"), "y.values")[[1]]
plot(roc.perf, col=rainbow(10))
plot.roc <- rbind(plot.roc, data.frame(fpr=fpr.points, tpr=tpr.points, model="Transformed"));
#plot.roc = rbind(plot.roc,data.frame(Model="standardized", Target=tpr.points, Pred=fpr.points))

lift.perf <- performance(pred, "lift", "rpp")
x.points <- attr(lift.perf, "x.values")[[1]]
y.points <- attr(lift.perf, "y.values")[[1]]
plot(lift.perf, col=rainbow(10))
plot.lift = rbind(plot.lift,data.frame(Model="Transformed", Target=x.points, Pred=y.points))


#Part C
rm(test,train)
test <- subset(spam, test == "TRUE")
train <- subset(spam, test == "FALSE")

train[,1:57] <- ifelse(train[,1:57] > 0,1,0)
test[,1:57] <- ifelse(test[,1:57] > 0,1,0)

x <- data.matrix(train[,-58])
y <- train[,58]
matrix_test <- data.matrix(test[,-58])
y.test<-test[,58]
ridge <- cv.glmnet( x,y ,family="binomial", standardize=FALSE, alpha=0, nfolds = 10, type.measure="class")

predValues<-predict(ridge, x, type="response")
correct <- 0
for(i in 1:length(predValues))
{
  if((predValues[i]>0.5&&y[i]==1)||(predValues[i]<0.5&&y[i]==0))
    correct = correct + 1
}
merBinTrain <- 1 - (correct/length(predValues))

predValues<-predict(ridge, matrix_test, type="response")
correct <- 0
for(i in 1:length(predValues))
{
  if((predValues[i]>0.5&&y.test[i]==1)||(predValues[i]<0.5&&y.test[i]==0))
    correct = correct + 1
}
merBinTest <- 1 - (correct/length(predValues))


#tab = table(predValues,test[,58])
#merBin = 1 - sum(diag(tab))/sum(tab)

pred <- prediction(predValues, test[,58])
roc.perf <- performance(pred,"tpr","fpr")
tpr.points <- attr(roc.perf, "y.values")[[1]]
fpr.points <- attr(roc.perf, "x.values")[[1]]
aucBin <- attr(performance(pred, "auc"), "y.values")[[1]]
plot(roc.perf, col=rainbow(10))
plot.roc <- rbind(plot.roc, data.frame(fpr=fpr.points, tpr=tpr.points, model="bin"));

#plot.roc = rbind(plot.roc,data.frame(Model="standardized", Target=tpr.points, Pred=fpr.points))

lift.perf <- performance(pred, "lift", "rpp")
x.points <- attr(lift.perf, "x.values")[[1]]
y.points <- attr(lift.perf, "y.values")[[1]]
plot(lift.perf, col=rainbow(10))
plot.lift = rbind(plot.lift,data.frame(Model="binary", Target=x.points, Pred=y.points))



#ggplot(subset(plot.roc, Data="Train"), aes(x = label, y = pred, color=model, shape=model)) + 
#  theme_bw() + geom_point() + xlab("FPR") + ylab("TPR") + geom_abline(intercept=0, slope=1, color="grey") + scale_colour_brewer(palette="Set1")
#ggsave("Q2ROC.png",width=10,height=3);

ggplot(plot.roc, aes(x=fpr, y=tpr, colour=model)) + 
  geom_line() + 
  theme_bw();
ggsave("Q2ROC.png",width=10,height=3);

#ggplot(subset(plot.lift, Data="Train"), aes(x = Target, y = Pred, color=Model, shape=Model)) + theme_bw() + geom_point() + 
#  xlab("Rate of positive Predictions") + ylab("Lift Value") + geom_abline(intercept=0, slope=1, color="grey") + scale_colour_brewer(palette="Set1")
ggplot(plot.lift, aes(x = Target, y = Pred, color=Model)) + 
  geom_line() + 
  theme_bw();

ggsave("Q2Lift.png",width=10,height=3);

aucStan
aucTran
aucBin

merScalTrain
merScalTest
merTranTrain
merTranTest
merBinTrain
merBinTest