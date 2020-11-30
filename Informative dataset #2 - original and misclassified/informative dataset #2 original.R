#QBA of random forests
#Informative dataset #2
#Original without misclassification

library(randomForest)


simulated.data2.func <- function(i){
  n=10000
  
  y=rbinom(n, 1, 0.2)
  x1=rbinom(n, 1, 0.1+0.1*y)
  x2=rbinom(n, 1, 0.1)
  x3=rbinom(n, 1, 0.1)
  x4=rbinom(n, 1, 0.1)
  x5=rbinom(n, 1, 0.1)
  
  dat2 <- as.data.frame(cbind(x1, x2, x3, x4, x5, y))
  
  dat2 <- data.frame(lapply(dat2, factor))
  
  rf.dat2 <- randomForest(y ~., data=dat2, importance=T, sampsize=c(length(which(dat2$y==1)), length(which(dat2$y==1))))
  importance(rf.dat2, type=1)
}

#Repeat the whole process
simulated.data2.func.result <- t(sapply(1:10000, simulated.data2.func))




