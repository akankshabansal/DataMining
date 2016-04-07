#install.packages("nlme")
rm(list = ls())
setwd("/Users/bansal/Google Drive/Spring2014/DataMining/HW3/")
library(MASS)
library(hydroGOF)
library(ggplot2)
library(nlme)
oxboys = read.csv("~/Google Drive/Spring2014/DataMining/HW3/HW3Data/oxboys.csv")

global <- lm(formula = height ~ year , data = oxboys)
summary(global)
png("Q6_Part_a.png")
layout(matrix(1:4, ncol = 2))
plot(global)
layout(1)
dev.off()

ggplot = ggplot(oxboys, aes(x=year, y=height, group=id)) + stat_smooth(method="lm")
ggsave('Q6_Part_b.png')

oxboys.train <- subset(oxboys, year < 3)
oxboys.test <- subset(oxboys, year >= 3)

global <- lm(formula = height ~ year , data = oxboys.train)
testGolbal <- predict.lm(global , newdata = oxboys.test)
mse(testGolbal,oxboys.test$height )
summary(global)
png("Q6_Part_c_a.png")
layout(matrix(1:4, ncol = 2))
plot(global)
layout(1)
dev.off()
list.mse  <- NULL 

for( key in unique(oxboys.train$id)){
  oxboys.train.ind <- subset(oxboys.train, id==key)
  oxboys.test.ind <- subset(oxboys.test, id==key)
  # build a linear model with oxboys.train.ind
  mlr <- lm(height ~ year , data = oxboys.train.ind)
  summary(mlr)
  #plot(mlr)
  
  # predict on oxboys.test.ind, and measure MSE
  testModel<-predict.lm(mlr, newdata = oxboys.test.ind)
  mlrMSE<-mse(testModel,oxboys.test.ind$height)
  list.mse <- c(list.mse,mlrMSE) 
}
sum(list.mse)

#lme(height ~ year + id , data = oxboys , random = ~id/year , method = "ML", na.action = na.omit)
mlm.obj <- lme(height~year, data=oxboys.train, random=list(id=pdDiag(~year)))
testmlm <- predict(mlm.obj, newdata=oxboys.test, level=1)
# calculate MSE
mse(testmlm, oxboys.test$height)


#PART D 
oxboys.train <- subset(oxboys, year < 7)
oxboys.test <- subset(oxboys, year >= 7)

global <- lm(formula = height ~ year , data = oxboys.train)
testGolbal <- predict.lm(global , newdata = oxboys.test)
mse(testGolbal,oxboys.test$height )
summary(global)
png("Q6_Part_d_a.png")
layout(matrix(1:4, ncol = 2))
plot(global)
layout(1)
dev.off()
list.mse  <- NULL 
for( key in unique(oxboys.train$id)){
  oxboys.train.ind <- subset(oxboys.train, id==key)
  oxboys.test.ind <- subset(oxboys.test, id==key)
  # build a linear model with oxboys.train.ind
  mlr <- lm(height ~ year , data = oxboys.train.ind)
  #summary(mlr)
  # predict on oxboys.test.ind, and measure MSE
  testModel<-predict.lm(mlr, newdata = oxboys.test.ind)
  mlrMSE<-mse(testModel,oxboys.test.ind$height)
  list.mse <- c(list.mse,mlrMSE) 
}
sum(list.mse)

mlm2.obj <- lme(height~year, data=oxboys.train, random=list(id=pdDiag(~year)))
test2mlm <- predict(mlm2.obj, newdata=oxboys.test, level=1)
# calculate MSE
mse(test2mlm, oxboys.test$height)
#p1 <- ggplot(oxboys.test,aes(x=year,y=height, group = id)) + geom_line() 
