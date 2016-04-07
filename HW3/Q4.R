rm(list = ls())

setwd("/Users/bansal/Google Drive/Spring2014/DataMining/HW2/")

library(pixmap)
library(plyr)
#Ans 4 Create Mydata 
path <- "/Users/bansal/Google Drive/Spring2014/DataMining/HW2/images/"
filenames <- paste0(path, list.files(path, pattern = "pgm"))
vec <- vector()
mydata = ldply(filenames, function(filename) {
  ppm =  read.pnm(filename)
  mat = ppm@grey
  vec = rbind(vec, as.vector(mat))
})

#Ans 4.a Canter the data 
#takes mean of the values 
mean <- colMeans(mydata)
#Centers the data from the mean however doesnt scale the data 
centeredData <- scale(mydata, mean, scale = FALSE)
centeredData1 <- scale(mydata, center = TRUE, scale = FALSE)
#Checks that the centered data calculated from the Mean is equivalent to the one create by using the directly 
identical(centeredData, centeredData1)

#Ans 4.a Center the data and scale it 
centeredDataScale <- scale(mydata, mean, scale = TRUE)
#Centers the data from the mean and scales it 
centeredDataScale1 <- scale(mydata, center = TRUE, scale = TRUE)
identical(centeredDataScale, centeredDataScale1)

#Ans 4.b SVD of the data 
mydata.svd <- svd(centeredData)

d <- diag(mydata.svd$d)
dim(d)
u <- mydata.svd$u
v <- mydata.svd$v
#Plots the amount of variance being explained by the Features 
plot(1:length(mydata.svd$d), mydata.svd$d)
dev.copy(png,'Q4svd.png')
dev.off()

#Ans 4.c and 4.d 
plot_image <- function(vec, nd, msg)
{
  xpgm = pixmapGrey(data=matrix(vec, nrow=nd[1], ncol=nd[2]))
  plot(xpgm, main=msg)
  dev.copy(png,paste(msg,'png',sep="."))
  dev.off()
}

nimage <- 40*10304
dim <- c(1,10,50,100,150,200,250,400);

for (i in 1:8 ) {
  reduced_data = matrix(rep(0,nimage), nrow = 400, ncol=400)
  for (j in 1:dim[i]){
    reduced_data[j,j]= mydata.svd$d[j]
  }
  constructed.data <- mydata.svd$u %*% reduced_data %*% t(mydata.svd$v)
  #Pick the 10th image from the constructed data set
  plot_image(constructed.data[10,], c(112,92), paste('image',as.character(i),sep="_"))
}
