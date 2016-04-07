#install.packages("randomForest")
rm(list = ls(all.names = TRUE))
setwd("/Users/bansal/Google Drive/Spring2014/DataMining/HW3/")
library(MASS)
library(hydroGOF)
library(ggplot2)
library(rpart)
library(caTools)
library(grid)
library(lattice)
library(DMwR)
library(DAAG)
library(caret)
library(randomForest)

train_data = read.csv("~/Google Drive/Spring2014/DataMining/HW3/HW3Data/Machine1.csv", header = TRUE)
test_data = read.csv("~/Google Drive/Spring2014/DataMining/HW3/HW3Data/Machinetest.csv", header = TRUE)

#regression 
lm_model<- lm(PRP ~ MYCT + MMIN + MMAX + CACH + CHMIN + CHMAX , data=train_data)
summary(lm_model)

png("Q3plot_lm_all.png")
layout(matrix(1:4, ncol = 2))
plot(lm_model)
layout(1)
dev.off()

#Some additional values which can com en handy 
#lm.rmse<- sqrt(mse(lm.model$fitted.values,train_data$PRP))
#lm.coef<-coefficients(lm.model) # model coefficients
#lm.ci<-confint(lm.model, level=0.95) # CIs for model parameters 
#lm.resd<-residuals(lm.model) # residuals
#lm.anova<-anova(lm.model) # anova table 
#lm.cov<-vcov(lm.model) # covariance matrix for model parameters 
#lm.influ<-influence(lm.model) # regression diagnostics

pred_lm<-fitted(lm_model) # predicted values
pred_lm_test<-predict(lm_model, newdata = test_data )
regr.eval(train_data$PRP, pred_lm,
          stats = c("mae", "mse", "rmse"),
          train.y = NULL)
cor(pred_lm, train_data$PRP)**2

cv.lm(df=train_data, lm_model, m=5)
dev.copy(png,'Q3plot_lm_cross.png')
dev.off()

#Regression Tree RT
rt_model <- rpart(PRP ~ MYCT + MMIN + MMAX + CACH + CHMIN + CHMAX , data = train_data, 
                  control = rpart.control(xval = 5, minbucket = 2, cp = 0))
#use of cp and max depth to control over and under fitting of the model 
plot(rt_model)
text(rt_model,use.n=TRUE, all=TRUE, cex=.5)
dev.copy(png,'Q3plot_rt.png')
dev.off()

#Train the model using the Cross validation (upto 5)
indx <- createFolds(train_data$PRP, returnTrain = TRUE)
ctrl <- trainControl(method = "cv", index = indx, number = 5)
set.seed(187)

cartTune <- train(x = train_data, y = train_data$PRP,
                  method = "rpart",
                  tuneLength = 25,
                  preProc = c("center", "scale"),
                  trControl = ctrl)
cartTune
### Plot the tuning results
plot(cartTune, scales = list(x = list(log = 10)))
dev.copy(png,'Q3plot_rt_crossValidation.png')
dev.off()

#visualise the Corss validation of the model 
plotcp(rt_model)
dev.copy(png,'Q3plot_rt_complexity.png')
dev.off()

#prune Chose the tree with ideal value of CP based on the Error values collected. 
cptable <- as.data.frame(rt_model$cptable)
alpha <- cptable$CP[which.min(cptable$xerror)]
rt_pruned <- prune(rt_model,cp=alpha)
plot(rt_pruned, branch = 0.5 )
text(rt_pruned,use.n=TRUE, all=TRUE, cex=.5)
dev.copy(png,'Q3plot_rt_prunedTree.png')
dev.off()

#Capture the Error Values 
#Rsquare 
rsq.rpart(rt_pruned)
pred_rt <- predict(rt_pruned)
cor(pred_rt, train_data$PRP)**2
pred_rt_test<-predict(rt_pruned, newdata = test_data )
cor(pred_rt_test, test_data$PRP)**2
#sqrt(mse(pred, train_data$PRP))
regr.eval(train_data$PRP, pred_rt,
          stats = c("mae", "mse", "rmse"),
          train.y = NULL)


#part c 
library(kernlab)

svr <- ksvm(PRP ~ MYCT + MMIN + MMAX + CACH + CHMIN + CHMAX , data=train_data, 
            type="eps-svr", kernel="vanilladot", cross=5)
summary(svr)

pred_svr <- predict(svr, train_data )
pred_svr_test<-predict(svr, newdata = test_data )

regr.eval(train_data$PRP, pred_svr,
          stats = c("mae", "mse", "rmse"),
          train.y = NULL)
cor(pred_svr, train_data$PRP)**2

#Part d 
pred <- pred_lm
pred <- append(pred,pred_rt)
pred <- append(pred,pred_svr)
target <- train_data$PRP
target <- append(target,train_data$PRP)
target <- append(target,train_data$PRP)
model <- rep("MLR",dim(train_data)[1])
model <- append(model,rep("RT",dim(train_data)[1]))
model <- append(model,rep("SVR",dim(train_data)[1]))

dataframe <- data.frame(Pred = pred, Target = target, Model = model)
ggplot(data.frame(dataframe), aes(x = dataframe$Pred, y = dataframe$Target, color=dataframe$Model, shape=dataframe$Model)) + theme_bw() +
  geom_point() + geom_abline(intercept=0, slope=1, color="grey") +
  scale_colour_brewer(palette="Set1") + xlab("Predicted PRP") +
  ylab("Target PRP") + theme(legend.position = "top")
dev.copy(png,'Q3plot_part_d.png')
dev.off()


#part e
regr.eval(test_data$PRP, pred_lm_test,
          stats = c("mae", "mse", "rmse"),
          train.y = NULL)
cor(pred_lm_test, test_data$PRP)**2
regr.eval(test_data$PRP, pred_rt_test,
          stats = c("mae", "mse", "rmse"),
          train.y = NULL)
cor(pred_rt_test, test_data$PRP)**2
regr.eval(test_data$PRP, pred_svr_test,
          stats = c("mae", "mse", "rmse"),
          train.y = NULL)
cor(pred_svr_test, test_data$PRP)**2

train_data2 = read.csv("~/Google Drive/Spring2014/DataMining/HW3/HW3Data/Machine2.csv", header = TRUE)

lm_model<- lm(PRP ~ MYCT + MMIN + MMAX + CACH + CHMIN + CHMAX , data=train_data2)
cv.lm(df=train_data, lm_model, m=5)
pred_lm_test<-predict(lm_model, newdata = test_data )
regr.eval(test_data$PRP, pred_lm,
          stats = c("mae", "mse", "rmse"),
          train.y = NULL)
cor(pred_lm_test, test_data$PRP)**2

#Regression Tree RT
rt_model <- rpart(PRP ~ MYCT + MMIN + MMAX + CACH + CHMIN + CHMAX , data = train_data2, 
                  control = rpart.control(xval = 5, minbucket = 2, cp = 0))
#prune Chose the tree with ideal value of CP based on the Error values collected. 
cptable <- as.data.frame(rt_model$cptable)
alpha <- cptable$CP[which.min(cptable$xerror)]
rt_pruned <- prune(rt_model,cp=alpha)

#Rsquare 
rsq.rpart(rt_pruned)
pred_rt_test<-predict(rt_pruned, newdata = test_data )
regr.eval(test_data$PRP, pred_rt,
          stats = c("mae", "mse", "rmse"),
          train.y = NULL)
cor(pred_rt_test, test_data$PRP)**2

#SVR
svr <- ksvm(PRP ~ MYCT + MMIN + MMAX + CACH + CHMIN + CHMAX , data=train_data2, 
            type="eps-svr", kernel="vanilladot", cross=5)
pred_svr_test<-predict(svr, newdata = test_data )
regr.eval(test_data$PRP, pred_svr,
          stats = c("mae", "mse", "rmse"),
          train.y = NULL)
cor(pred_svr_test, test_data$PRP)**2

#part f 
randf <- randomForest(PRP ~ MYCT + MMIN + MMAX + CACH + CHMIN + CHMAX, data=train_data, ntree=10 )
pred_randf <- predict(randf, newdata = train_data)
cor(pred_randf, train_data$PRP)**2

regr.eval(train_data$PRP, pred_randf,
          stats = c("mae", "mse", "rmse"),
          train.y = NULL)
