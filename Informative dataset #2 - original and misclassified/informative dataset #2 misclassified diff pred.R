#QBA random forests
#Informative dataset #2
#Differential misclassification of predictor for simulated data set #2

library(foreign)
library(randomForest)
library(pROC)

#Get arguments from the command line
argv <- commandArgs(TRUE)

# Check if the command line is not empty and convert values to numerical values
if (length(argv) > 0){
  myse1 <- as.numeric( argv[1] )
  mysp1 <- as.numeric( argv[2] ) 
  myse0 <- as.numeric( argv[3] )
  mysp0 <- as.numeric( argv[4] ) 
} else {
  stop("Please input parameters")
  #myse <- 0.95
  #mysp <- 0.95
}


################################# Differential misclassification of predictor x1 #################################
dat2.diff.misc.pred.func <- function(i, se1, sp1, se0, sp0){
  n=10000
  y=rbinom(n, 1, 0.2)
  x1=rbinom(n, 1, 0.1+0.1*y)
  x2=rbinom(n, 1, 0.1)
  x3=rbinom(n, 1, 0.1)
  x4=rbinom(n, 1, 0.1)
  x5=rbinom(n, 1, 0.1)
  
  dat2 <- as.data.frame(cbind(x1, x2, x3, x4, x5, y))
  
  dat2 <- data.frame(lapply(dat2, factor))
  
  dat2.diff.pred <- dat2
  
  dat2.diff.pred$x1.new <- NA
  dat2.diff.pred$x1.new[dat2.diff.pred$x1==1 & dat2.diff.pred$y==1] <- rbinom(length(which(dat2.diff.pred$x1==1 & dat2.diff.pred$y==1)), 1, se1)
  dat2.diff.pred$x1.new[dat2.diff.pred$x1==0 & dat2.diff.pred$y==1] <- rbinom(length(which(dat2.diff.pred$x1==0 & dat2.diff.pred$y==1)), 1, 1-sp1)
  dat2.diff.pred$x1.new[dat2.diff.pred$x1==1 & dat2.diff.pred$y==0] <- rbinom(length(which(dat2.diff.pred$x1==1 & dat2.diff.pred$y==0)), 1, se0)
  dat2.diff.pred$x1.new[dat2.diff.pred$x1==0 & dat2.diff.pred$y==0] <- rbinom(length(which(dat2.diff.pred$x1==0 & dat2.diff.pred$y==0)), 1, 1-sp0)
  
  #Subset data for misclassified variables and the other variables of interest
  vars <- c("x1.new", "x2", "x3", "x4", "x5", "y")
  dat2.diff.pred2 <- dat2.diff.pred[vars] 
  
  dat2.diff.pred2$x1.new <- as.factor(dat2.diff.pred2$x1.new)
  
  #Run random forest for misclassified data set #2 with diff misclass. of predictor
  rf.dat2.diff.pred <- randomForest(y ~., data=dat2.diff.pred2, importance=T, sampsize=c(length(which(dat2.diff.pred2$y==1)), length(which(dat2.diff.pred2$y==1))))
  
  #Variable importance
  importance(rf.dat2.diff.pred, type=1)
}


#Repeat the whole process
dat2.diff.misc.pred.func.result <- t(sapply(1:10000, dat2.diff.misc.pred.func, se1=myse1, sp1=mysp1, se0=myse0, sp0=mysp0)) 