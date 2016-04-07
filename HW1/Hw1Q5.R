#NOT READING DATA FROM 
library(MASS)
rm(list=ls())
boxplot(as.data.frame(Boston[,c('lstat','medv')]), main = "boxplot(LSTAT, MEDV)")
boxdata <- with(Boston,boxplot(as.data.frame(Boston[,c('lstat','medv')]), main = 
                                 "boxplot(LSTAT, MEDV)"))
identify(rep(1, length(Boston)), Boston, labels = seq_along(Boston))
#plot(boxdata)
#inserting the Labels of the cutoff
text(boxdata$stats[1][1],label=boxdata$stats[1][1])
text(boxdata$stats[5][1],label=boxdata$stats[5][1])
text(boxdata$stats[6],label=boxdata$stats[6])
text(boxdata$stats[10],boxdata$group[10],label=boxdata$stats[10])

#Scatter plot LSAT on Y and MDEV in Y 
plot(Boston$medv, Boston$lstat, main="LSTAT vs MEDV",xlab="MEDV",ylab="LSTAT")

#Showing the outliers in a different color 
outlier.colors <- (Boston$medv %in% boxdata$out)*1 + (Boston$lstat %in% boxdata$out)*2
outlier.colors <- outlier.colors + 1
plot(Boston$medv, Boston$lstat, col=outlier.colors)

#PROCESSED: Removing the outliers from the dataset to see the behaviour of the model
medv<-Boston$medv[!Boston$medv %in% boxdata$out]
lstat<-Boston$lstat[!Boston$lstat %in% boxdata$out]
#box plot after removal of outliers 
boxplot(as.data.frame(lstat), main = "tuned LSTAT",data=lstat)
boxplot(as.data.frame(medv), main = "tuned MEDV",data=medv)

Bostrain <- Boston[(1:300),]
Bostest <- Boston[(301:506),]
Bostrain$lmedv <- log(Bostrain$medv)
Bostest$lmedv <- log(Bostest$medv)

model <- lm(lmedv ~ lstat + rm + crim + zn + chas, data = Bostrain)
summary(model)
mse(model$fitted.values,Bostrain$lmedv)
#Ans Based on the t values removing lstat, rm can be tried

#Testing the models behavior when crim is dropped from the model equation
model2 <- lm(lmedv ~ lstat + rm + zn + chas, data = Bostrain)
summary(model2)
mse(model2$fitted.values,Bostrain$lmedv)
#This increased the t-value of every other component, in addition to causing huge in-crease in MSE

#Testing the models behavior when zn is dropped from the model equation
model2 <- lm(lmedv ~ lstat + rm + chas, data = Bostrain)
summary(model2)
mse(model2$fitted.values,Bostrain$lmedv)
#not much of change in MSE.

#Based on the t-values it doesn't make sense to remove chase from the model but considering that it is a dummy value (as read from the variables details ) removing it from the model can be tried.
model2 <- lm(lmedv ~ lstat + rm, data = Bostrain)
summary(model2)
mse(model2$fit,Bostrain$lmedv)
#There is a slight increase in the MSE value. 


testModel<-predict.lm(model, newdata = Bostest)
summary(testModel)
mse(testModel,Bostest$lmedv)


#part c
library(car)
library(hydroGOF)
#Linear Regression sample 
#fit<-lm(lstat ~ test$MEDV, data=test[1:300,])
#Provides with the summary 
Boston$lmedv <- log(Boston$medv)
fit <- lm(lmedv ~ lstat + rm + crim + zn + chas, data = Boston)
summary(fit, data = Boston)
plot(fit)
outlierTest(fit)
qqPlot(fit, main="QQ Plot")
leveragePlots(fit)

