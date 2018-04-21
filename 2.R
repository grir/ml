# Rcpp::sourceCpp('generators.cpp')
# means = matrix(c(0,0,2,2),nrow=2, byrow = TRUE)
# sds = matrix(c(1,1,1,1),nrow=2, byrow = TRUE)
# mats = generateStdNormalMatrix(means, sds, seed = 123, numRows = 10)

library(MASS)


mvStdNorms <- function(means=c(0,0), sds=c(1,1), clsId=1){
  sigma <- diag(sds)  
  result <- mvrnorm(n=1, means, sigma)
  result <- c(result,clsId) 
  # print(result)
  return(result)
}

observationsStdNormsTwoClasses <- function(numObs=10, 
                                           meanss=matrix(c(0,0,3,3),nrow=2,byrow=TRUE), 
                                           sdss=matrix(c(1,1,1,1),nrow=2,byrow=TRUE),
                                           dimension=2)
  {
  result = matrix(nrow = numObs, ncol = dimension+1)
  clsIds = sample.int(dimension, size=numObs, replace = TRUE)
  for(rw in 1:numObs){
    clsId = clsIds[rw]
   # print(sdss[clsId,])
    result[rw,] = mvStdNorms(means=meanss[clsId,],sds=sdss[clsId,],clsId = clsId)
  }
  return(result)
}


plotTwoClasses <- function(observationsMatrix){
  minX = min(observationsMatrix[,1])-2
  maxX = max(observationsMatrix[,1])+2
  minY = min(observationsMatrix[,2])-2
  maxY = max(observationsMatrix[,2])+2
  x1 = observationsMatrix[observationsMatrix[,3]==1,1]
  y1 = observationsMatrix[observationsMatrix[,3]==1,2]
  x2 = observationsMatrix[observationsMatrix[,3]==2,1]
  y2 = observationsMatrix[observationsMatrix[,3]==2,2]
  plot(x1, y1, col="red", xlim=c(minX,maxX), ylim=c(minY,maxY))
  points(x2,y2, col="blue", xlim=c(minX,maxX),ylim=c(minY,maxY))
  
}


