#QBA random forests
#Uninformative dataset
#Non-differential misclassification of predictor 

library(foreign)
library(randomForest)
library(pROC)

#Get arguments from the command line
argv <- commandArgs(TRUE)

# Check if the command line is not empty and convert values to numerical values
if (length(argv) > 0){
  myse <- as.numeric( argv[1] )
  mysp <- as.numeric( argv[2] ) 
} else {
  stop("Please input parameters")
  #myse <- 0.95
  #mysp <- 0.95
} 

################################# Non-differential misclassification of predictor x5 #################################
dat1.nondiff.misc.pred.func <- function(i, se, sp){
  
  n=10000
  dat1 = data.frame(x1=rbinom(n, 1, 0.1),
                    x2=rbinom(n, 1, 0.1),
                    x3=rbinom(n, 1, 0.1),
                    x4=rbinom(n, 1, 0.1),
                    x5=rbinom(n, 1, 0.1),
                    y=rbinom(n, 1, 0.2))
  
  dat1 <- data.frame(lapply(dat1, factor))
  
dat1.nondiff.pred <- dat1

dat1.nondiff.pred$x5.new <- NA
dat1.nondiff.pred$x5.new[dat1.nondiff.pred$x5==1] <- rbinom(length(which(dat1.nondiff.pred$x5==1)), 1, se)
dat1.nondiff.pred$x5.new[dat1.nondiff.pred$x5==0] <- rbinom(length(which(dat1.nondiff.pred$x5==0)), 1, 1-sp)

#Subset data for misclassified variables and the other variables of interest
vars <- c("x1", "x2", "x3", "x4", "x5.new", "y")
dat1.nondiff.pred2 <- dat1.nondiff.pred[vars] 

dat1.nondiff.pred2$x5.new <- as.factor(dat1.nondiff.pred2$x5.new)

#Run random forest for misclassified data set #1 with nondiff misclass. of predictor
rf.dat1.nondiff.pred <- randomForest(y ~., data=dat1.nondiff.pred2, importance=T, sampsize=c(length(which(dat1.nondiff.pred2$y==1)), length(which(dat1.nondiff.pred2$y==1))))

#Variable importance
importance(rf.dat1.nondiff.pred, type=1) 
}


#Repeat the whole process
dat1.nondiff.misc.pred.func.result <- t(sapply(1:10000, dat1.nondiff.misc.pred.func, se=myse,sp=mysp))
