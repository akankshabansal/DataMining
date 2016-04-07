rm(list = ls())
setwd("/Users/bansal/Google Drive/Spring2014/DataMining/HW3/")
library(MASS)
library(hydroGOF)
library(ggplot2)
test = read.csv("~/Google Drive/Spring2014/DataMining/HW3/HW3Data/forestfire-test.csv",header=TRUE)
train = read.csv("~/Google Drive/Spring2014/DataMining/HW3/HW3Data/forestfire-train.csv",header=TRUE)

num.iterations <- 1000

# Download South African heart disease data
x <- train[,c("FFMC", "DMC","DC","ISI","temp","RH","wind","rain")]
y <- train$area
plot(x, pch=21, bg=c("red","green")[factor(y)])
dev.copy(png,'Q1plot1.png')
dev.off()

for (j in 1:dim(train)[2] ) {
  train[, j] = (train[, j] - mean(train[, j])/sd(train[,j]))
  test[, j]= (test[, j] - mean(train[, j])/sd(train[,j]))
}


# Standardize the features
x.scaled <- train[,c("FFMC", "DMC","DC","ISI","temp","RH","wind","rain")]
y.scaled <- train$area

# Gradient descent function
grad <- function(x, y, theta) {
  gradient <- (1 / nrow(y)) * (t(x) %*% (1/(1 + exp(-x %*% t(theta))) - y))
  return(t(gradient))
}

gradient.descent <- function(x, y, alpha=0.1, num.iterations=1000, threshold=0.1, output.path=FALSE) {
  
  # Add x_0 = 1 as the first column
  m <- if(is.vector(x)) length(x) else nrow(x)
  if(is.vector(x) || (!all(x[,1] == 1))) x <- cbind(rep(1, m), x)
  if(is.vector(y)) y <- matrix(y)
  x <- apply(x, 2, as.numeric)
  
  num.features <- ncol(x)
  
  # Initialize the parameters
  theta <- matrix(rep(0.5, num.features), nrow=1)
  
  # Look at the values over each iteration
  #change this function to check for RMSE and not the abs difference 
  theta.path <- theta
  for (i in 1:num.iterations) {
    theta <- theta - alpha * grad(x, y, theta)
    if(all(is.na(theta))) 
      break
    theta.path <- rbind(theta.path, theta)
    if(i > 2) 
      if(all(abs(theta - theta.path[i-1,]) < threshold)) 
      {
        #theta.path[]
        print(sqrt(mse(model$fitted.values,train$area)))
        break 
      }
  }
  
  if(output.path) 
    return(theta.path) 
  else 
    return(theta.path[nrow(theta.path),])
}

unscaled.theta <- gradient.descent(x=x, y=y, num.iterations=num.iterations, output.path=TRUE)
scaled.theta <- gradient.descent(x=x.scaled, y=y.scaled, num.iterations=num.iterations, output.path=TRUE)

#linear regression 
model<- lm(area ~ FFMC + DMC + DC + ISI + temp + RH + wind + rain , family = binomial, data=train)
summary(model)
mse(model$fitted.values,train$area)
sqrt(mse(model$fitted.values,train$area))

testModel<-predict.lm(model, newdata = test)
summary(testModel)
mse(testModel,test$area)
sqrt(mse(testModel,test$area))

qplot(1:(nrow(scaled.theta)), scaled.theta[,1], geom=c("line"), xlab="iteration", ylab="theta_1")
dev.copy(png,'Q1plot_scaled_theata1.png')
dev.off()
qplot(1:(nrow(scaled.theta)), scaled.theta[,2], geom=c("line"), xlab="iteration", ylab="theta_2")
dev.copy(png,'Q1plot_scaled_theata2.png')
dev.off()

# Look at output for various different alpha values
alphas <- c(0.00025, 0.00575, 0.0065)

vary.alpha <- lapply(alphas, 
                     function(alpha) gradient.descent(x=x.scaled, y=y.scaled, 
                     alpha=alpha, num.iterations=num.iterations, output.path=TRUE))

par(mfrow = c(2, 3))
for (j in 1:3) {
  plot(vary.alpha[[j]][,2], ylab=paste("alpha",alphas[j],sep="-"), xlab="iteration", type="l")
}
#vary.alpha[[1]][,9]
#print no of iterations 
dev.copy(png,'Q1plot2.png')
dev.off()


