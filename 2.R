# Rcpp::sourceCpp('generators.cpp')
# means = matrix(c(0,0,2,2),nrow=2, byrow = TRUE)
# sds = matrix(c(1,1,1,1),nrow=2, byrow = TRUE)
# mats = generateStdNormalMatrix(means, sds, seed = 123, numRows = 10)

library(MASS)


mvStdNorms <- function(nrow=1,means=c(0,0), sds=c(1,1), clsId=1){
  sigma <- diag(sds)  
  result <- mvrnorm(nrow, means, sigma)
  result <- cbind(result,rep(clsId,nrow)) 
  return(result)
}

observationsStdNormsTwoClasses <- function(numObs=10, dimension=2){
  
  
}
