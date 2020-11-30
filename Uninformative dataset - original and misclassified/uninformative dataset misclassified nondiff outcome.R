#QBA random forests
#Uninformative dataset
#Non-differential misclassification of outcome

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


################################# Non-differential misclassification of outcome #################################
dat1.nondiff.misc.outcome.func <- function(i, se, sp){
  
  n=10000
  dat1 = data.frame(x1=rbinom(n, 1, 0.1),
                    x2=rbinom(n, 1, 0.1),
                    x3=rbinom(n, 1, 0.1),
                    x4=rbinom(n, 1, 0.1),
                    x5=rbinom(n, 1, 0.1),
                    y=rbinom(n, 1, 0.2))
  
  dat1 <- data.frame(lapply(dat1, factor))
  
  dat1.nondiff.outcome <- dat1
  
  dat1.nondiff.outcome$y.new <- NA
  dat1.nondiff.outcome$y.new[dat1.nondiff.outcome$y==1] <- rbinom(length(which(dat1.nondiff.outcome$y==1)), 1, se)
  dat1.nondiff.outcome$y.new[dat1.nondiff.outcome$y==0] <- rbinom(length(which(dat1.nondiff.outcome$y==0)), 1, 1-sp)
  
  #Subset data for misclassified variables and the other variables of interest
  vars <- c("x1", "x2", "x3", "x4", "x5", "y.new")
  dat1.nondiff.outcome2 <- dat1.nondiff.outcome[vars] 
  
  dat1.nondiff.outcome2$y.new <- as.factor(dat1.nondiff.outcome2$y.new)
  
  #Run random forest for misclassified data set #1 with nondiff misclass. of outcome
  rf.dat1.nondiff.outcome <- randomForest(y.new ~., data=dat1.nondiff.outcome2, importance=T, sampsize=c(length(which(dat1.nondiff.outcome2$y.new==1)), length(which(dat1.nondiff.outcome2$y.new==1))))
  
  #Variable importance
  importance(rf.dat1.nondiff.outcome, type=1)
}

#Repeat the whole process
dat1.nondiff.misc.outcome.func.result <- t(sapply(1:10000, dat1.nondiff.misc.outcome.func, se=myse,sp=mysp)) 
