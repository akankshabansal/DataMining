library(MASS)
#Change the wording 
#Case 1 
#bivn <- mvrnorm(1000, mu = c(0, 0), Sigma = matrix(c(1, 0, 0, 1), 2))
#Case 2 
bivn <- mvrnorm(1000, mu = c(0, 0), Sigma = matrix(c(1, 0, 0, 4), 2))
#Case 3 
#bivn <- mvrnorm(1000, mu = c(0, 0), Sigma = matrix(c(1, 1, 1, 4), 2))
# now we do a kernel density estimate
bivn.kde <- kde2d(bivn[,1], bivn[,2], n = 50)
# now plot your results
contour(bivn.kde)
image(bivn.kde)
persp(bivn.kde, phi = 45, theta = 30)
# fancy contour with image
image(bivn.kde); contour(bivn.kde, add = T)
# fancy perspective
#png("~/Google\ Drive/Spring2014/DataMining/HW1/temp.png")
persp(bivn.kde, phi = 60, theta = 45, shade = .1, border = NA)
#dev.off()
