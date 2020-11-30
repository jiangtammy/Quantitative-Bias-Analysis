#QBA of random forests in the simulated data without any misclassification 
#Informative dataset #1
#No misclassification (original simulated data) 

library(randomForest)
library(pROC)

#Run random forest for data set #1
simulated.data.func <- function(i){
  n=10000
  x1=rbinom(n, 1, 0.10)
  x2=rbinom(n, 1, 0.35+0.1*x1)
  x3=rbinom(n, 1, 0.20+0.15*x2)
  x4=rbinom(n, 1, 0.25+0.05*x3)
  y=rbinom(n, 1, 0.05+0.05*x1+0.38*x2+0.35*x3+0.17*x4)
  
  dat <- as.data.frame(cbind(x1, x2, x3, x4, y))
  
  dat <- data.frame(lapply(dat, factor))
  
  rf.dat <- randomForest(y ~., data=dat, importance=T, sampsize=c(length(which(dat$y==1)),  length(which(dat$y==1))))
  
  varimp <- importance(rf.dat, type=1)
  
  #AUC
  rf.dat.roc <- roc(dat$y, rf.dat$votes[,2])
  auc <- auc(rf.dat.roc) 
  
  #Other Performance metrics
  confusion.tab <- rf.dat$confusion
  accuracy <- (confusion.tab[1,1] + confusion.tab[2,2]) / (confusion.tab[1,1] + confusion.tab[1,2] + confusion.tab[2,1] + confusion.tab[2,2])
  sensitivity <- confusion.tab[2,2] / (confusion.tab[2,1] + confusion.tab[2,2])
  specificity <- confusion.tab[1,1] / (confusion.tab[1,1] + confusion.tab[1,2])
  ppv <- confusion.tab[2,2] / (confusion.tab[2,2] + confusion.tab[1,2])
  npv <- confusion.tab[1,1] / (confusion.tab[1,1] + confusion.tab[2,1])
  
  #Combine
  results <- rbind(varimp, auc, accuracy, sensitivity, specificity, ppv, npv)
  t(results)
}

#Repeat the whole process
simulated.data.func.result <- t(sapply(1:10000, simulated.data.func)) 

