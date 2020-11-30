#QBA of random forests
#Uninformative dataset
#Original without miclassification

library(randomForest)

#Run random forest for data set #1
simulated.data1.func <- function(i){
  n=10000
  dat1 = data.frame(x1=rbinom(n, 1, 0.1),
                   x2=rbinom(n, 1, 0.1),
                   x3=rbinom(n, 1, 0.1),
                   x4=rbinom(n, 1, 0.1),
                   x5=rbinom(n, 1, 0.1),
                   y=rbinom(n, 1, 0.2))
  
  dat1 <- data.frame(lapply(dat1, factor))
  
  rf.dat1 <- randomForest(y ~., data=dat1, importance=T, sampsize=c(length(which(dat1$y==1)), length(which(dat1$y==1))))
  
  importance(rf.dat1, type=1)
}

#Repeat the whole process
simulated.data1.func.result <- t(sapply(1:10000, simulated.data1.func))
