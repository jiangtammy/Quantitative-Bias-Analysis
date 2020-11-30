#QBA random forests
#Uninformative dataset
#Differential misclassification of predictor

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



################################# Non-differential misclassification of predictor x5 #################################
dat1.diff.misc.pred.func <- function(i, se1, sp1, se0, sp0){
  
  n=10000
  dat1 = data.frame(x1=rbinom(n, 1, 0.1),
                    x2=rbinom(n, 1, 0.1),
                    x3=rbinom(n, 1, 0.1),
                    x4=rbinom(n, 1, 0.1),
                    x5=rbinom(n, 1, 0.1),
                    y=rbinom(n, 1, 0.2))
  
  dat1 <- data.frame(lapply(dat1, factor))
  
  dat1.diff.pred <- dat1
  
  dat1.diff.pred$x5.new <- NA
  dat1.diff.pred$x5.new[dat1.diff.pred$x5==1 & dat1.diff.pred$y==1] <- rbinom(length(which(dat1.diff.pred$x5==1 & dat1.diff.pred$y==1)), 1, se1)
  dat1.diff.pred$x5.new[dat1.diff.pred$x5==0 & dat1.diff.pred$y==1] <- rbinom(length(which(dat1.diff.pred$x5==0 & dat1.diff.pred$y==1)), 1, 1-sp1)
  dat1.diff.pred$x5.new[dat1.diff.pred$x5==1 & dat1.diff.pred$y==0] <- rbinom(length(which(dat1.diff.pred$x5==1 & dat1.diff.pred$y==0)), 1, se0)
  dat1.diff.pred$x5.new[dat1.diff.pred$x5==0 & dat1.diff.pred$y==0] <- rbinom(length(which(dat1.diff.pred$x5==0 & dat1.diff.pred$y==0)), 1, 1-sp0)
  
  #Subset data for misclassified variables and the other variables of interest
  vars <- c("x1", "x2", "x3", "x4", "x5.new", "y")
  dat1.diff.pred2 <- dat1.diff.pred[vars]
  
  dat1.diff.pred2$x5.new <- as.factor(dat1.diff.pred2$x5.new)
  
  #Run random forest for misclassified data set #1 with diff misclass. of predictor
  rf.dat1.diff.pred <- randomForest(y ~., data=dat1.diff.pred2, importance=T, sampsize=c(length(which(dat1.diff.pred2$y==1)), length(which(dat1.diff.pred2$y==1))))
  
  #Variable importance
  importance(rf.dat1.diff.pred, type=1)

}


#Repeat the whole process
dat1.diff.misc.pred.func.result <- t(sapply(1:10000, dat1.diff.misc.pred.func, se1=myse1, sp1=mysp1, se0=myse0, sp0=mysp0)) 