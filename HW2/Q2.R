rm(list = ls())
setwd("/Users/bansal/Google Drive/Spring2014/DataMining/HW2/")

library(AppliedPredictiveModeling)
library(e1071)
library(caret)
data(segmentationOriginal)

## Retain the original training set
segTrain <- subset(segmentationOriginal, Case == "Train")

## Remove the first three columns (identifier columns)
segTrainX <- segTrain[, -(1:3)]
segTrainClass <- segTrain$Class

transparentTheme(pchSize = .7, trans = .3)

## Apply PCA to the entire set of predictors.
## There are a few predictors with only a single value, so we remove these first
## (since PCA uses variances, which would be zero)

isZV <- apply(segTrainX, 2, function(x) length(unique(x)) == 1)
segTrainX <- segTrainX[, !isZV]

segPP <- preProcess(segTrainX, c("BoxCox", "center", "scale"))
segTrainTrans <- predict(segPP, segTrainX)

## To filter on correlations, we first get the correlation matrix for the 
## predictor set
#original Cor image 
segCorr <- cor(segTrainX)

library(corrplot)
corrplot(segCorr, order = "hclust", tl.cex = .35)
dev.copy(png,'Q2Originalcor.png')
dev.off()

## caret's findCorrelation function is used to identify columns to remove.
highCorr <- findCorrelation(segCorr, cutoff=0)
highCorr
length(highCorr)
head(highCorr)

highCorr <- findCorrelation(segCorr, cutoff=.75)
highCorr
length(highCorr)
head(highCorr)

highCorr <- findCorrelation(segCorr, cutoff=.5)
highCorr
length(highCorr)
#Ans 2.a 
#[1] 69
head(highCorr)

filSeg <- segTrainTrans[, -highCorr]
filSegCorr <- cor(filSeg)

#ans 2.b 
corrplot(filSegCorr, order = "hclust", tl.cex = .35)
dev.copy(png,'Q2cor.png')
dev.off()

segPCA <- prcomp(filSeg, center = TRUE, scale. = TRUE)

## Plot a scatterplot matrix of the first three components
transparentTheme(pchSize = .8, trans = .3)

#Ans 2.c 
panelRange <- extendrange(segPCA$x[, 1:3])
splom(as.data.frame(segPCA$x[, 1:3]),
      groups = segTrainClass,
      type = c("p", "g"),
      as.table = TRUE,
      auto.key = list(columns = 2),
      prepanel.limits = function(x) panelRange)
dev.copy(png,'Q2PCAplot.png')
dev.off()
