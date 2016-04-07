rm(list = ls())

setwd("/Users/bansal/Google Drive/Spring2014/DataMining/HW2/")

load("~/Google Drive/Spring2014/DataMining/HW2/auto.Rdata")
library(AppliedPredictiveModeling)
library(caret)
library(glmnet)
library(hydroGOF)
autoTrain <- subset(auto.data, train == "TRUE")
autoTest <- subset(auto.data, train == "FALSE")

autoTrainX <- autoTrain[, -(0:1)]
autoTestX <- autoTrain[, -(0:1)]
autoTrainY <- autoTrain$mpg
autoTestY <- autoTest$mpg

dummyCodexCylinder <- model.matrix( ~ cylinders - 1 + displacement + horsepower + weight + acceleration , data = autoTrain)
dummyCodexOrigin <- model.matrix(~ origin - 1 , data = autoTrain)

dummyCodex <- cbind(dummyCodexOrigin, dummyCodexCylinder)

testXCylinder <- model.matrix( ~ cylinders - 1 + displacement + horsepower + weight + acceleration , data = autoTest)
testXOrigin <- model.matrix(~ origin - 1 , data = autoTest)

testX <- cbind(testXOrigin, testXCylinder)

#MLR 
#dummyCodemlrCylinder <- model.matrix(~ cylinders - 1 + displacement + horsepower + weight + acceleration, data = autoTrain)
#dummyCodemlrOrigin <- model.matrix(~ origin - 1 , data = autoTrain)
#dummyCodemlr <-cbind(testXOrigin, testXCylinder)

train.data<-cbind(mpg=autoTrain$mpg,dummyCodex )

mlr <- lm(formula = mpg ~ displacement + horsepower + weight + acceleration + 
            cylinders3 + cylinders4 + cylinders5 + cylinders6 + cylinders8 + 
            origin1 + origin2 + origin3, data = as.data.frame(train.data))

summary(mlr)
testModel<-predict.lm(mlr, newdata = as.data.frame(testX))
summary(testModel)
mlrMSE<-mse(testModel,autoTestY)


head(dummyCodex)
lev <- hat(dummyCodex)
#plot(lev,ylab="Leverages",main="Index plot of Leverages")
#dev.copy(png,'Q7Leverages.png')
#dev.off()

#Ans b Plot Ridge and Lasso MSE for various values of lambda 
#LASSO
lasso <- cv.glmnet(dummyCodex, autoTrainY , alpha=1, nfolds = 5,type.measure="mse")
#Ans d Optimal lambda for glmnet for Lasso 
lasso$lambda.min
#[1] 0.3628411

#RIDGE
ridge <- cv.glmnet(dummyCodex, autoTrainY , alpha=0, nfolds = 5,type.measure="mse")
#Ans d Optimal lambda for glmnet for ridge 
ridge$lambda.min
#[1] 0.6489956

plot(ridge, xvar = c("norm", "lambda", "dev"))
dev.copy(png,'Q7_Ridge.png')
dev.off()
plot(lasso, xvar=c("norm", "lambda", "dev"))
dev.copy(png,'Q7_Lasso.png')
dev.off()

#Ans c Plot the Coefficient paths 
ridgeFit <- glmnet(dummyCodex, autoTrainY,  alpha = 0, nlambda = 5)
plot(ridgeFit)
dev.copy(png,'Q7_RidgeCoefFit.png')
dev.off()


lassoFit <- glmnet(dummyCodex, autoTrainY, alpha = 1, nlambda = 5)
plot(lassoFit)
dev.copy(png,'Q7_LassoCoefFit.png')
dev.off()

plot(log(lasso$lambda), lasso$cvm, pch = 19, col = "red", xlab = "log(Lambda)", ylab = lasso$name)
points(log(ridge$lambda), ridge$cvm, pch = 19, col = "blue" )
legend("topleft", legend = c("alpha= 1(Lasso)", "alpha 0(Ridge)"), pch = 19, 
       col = c("red", "grey", "blue"))
dev.copy(png,'Q7_MSEComp.png')
dev.off()


#Ans Variation in the predicted vs the observed values for Ridge and for Lasso 
pred <- predict(ridge, testX,s = "lambda.min")
plot(pred,autoTestY,xlab="Predicted Ridge",ylab ="Observed")
dev.copy(png,'Q7_RidgePredicted.png')
dev.off()

ridgeMSE <-mse(as.matrix(pred),as.matrix(autoTestY))

pred <- predict(lasso, testX,s = "lambda.min")
plot(pred,autoTestY,xlab="Predicted Lasso",ylab ="Observed")
dev.copy(png,'Q7_LassoPredicted.png')
dev.off()

lassoMSE <- mse(as.matrix(pred),as.matrix(autoTestY))

lassoCoef<-coef(lasso, s = "lambda.min")
lassoCoef
lassoCoef<-coef(lasso, s = "lambda.min",exact= TRUE)
lassoCoef

#MLR 
mlr2 <- lm(formula = mpg ~ horsepower + weight + cylinders3 + cylinders4 + 
             origin3, data = as.data.frame(train.data))

summary(mlr2)
testModel2<-predict.lm(mlr2, newdata = as.data.frame(testX))
summary(testModel2)
mlrMSE2<-mse(testModel2,autoTestY)
mlrMSE2


