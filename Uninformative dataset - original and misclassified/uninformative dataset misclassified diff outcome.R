#QBA random forests
#Uninformative dataset
#Differential misclassification of outcome 

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



################################# Differential misclassification of outcome #################################
dat1.diff.misc.outcome.func <- function(i, se1, sp1, se0, sp0){
  
  n=10000
  dat1 = data.frame(x1=rbinom(n, 1, 0.1),
                    x2=rbinom(n, 1, 0.1),
                    x3=rbinom(n, 1, 0.1),
                    x4=rbinom(n, 1, 0.1),
                    x5=rbinom(n, 1, 0.1),
                    y=rbinom(n, 1, 0.2))
  
  dat1 <- data.frame(lapply(dat1, factor))
  
  
  dat1.diff.outcome <- dat1
  
  dat1.diff.outcome$y.new <- NA
  dat1.diff.outcome$y.new[dat1.diff.outcome$y==1 & dat1.diff.outcome$x5==1] <- rbinom(length(which(dat1.diff.outcome$y==1 & dat1.diff.outcome$x5==1)), 1, se1)
  dat1.diff.outcome$y.new[dat1.diff.outcome$y==0 & dat1.diff.outcome$x5==1] <- rbinom(length(which(dat1.diff.outcome$y==0 & dat1.diff.outcome$x5==1)), 1, 1-sp1)
  dat1.diff.outcome$y.new[dat1.diff.outcome$y==1 & dat1.diff.outcome$x5==0] <- rbinom(length(which(dat1.diff.outcome$y==1 & dat1.diff.outcome$x5==0)), 1, se0)
  dat1.diff.outcome$y.new[dat1.diff.outcome$y==0 & dat1.diff.outcome$x5==0] <- rbinom(length(which(dat1.diff.outcome$y==0 & dat1.diff.outcome$x5==0)), 1, 1-sp0)
  
  #Subset data for misclassified variables and the other variables of interest
  vars <- c("x1", "x2", "x3", "x4", "x5", "y.new")
  dat1.diff.outcome2 <- dat1.diff.outcome[vars] 
  
  dat1.diff.outcome2$y.new <- as.factor(dat1.diff.outcome2$y.new)
  
  #Run random forest for misclassified data set #1 with diff misclass. of outcome
  rf.dat1.diff.outcome <- randomForest(y.new ~., data=dat1.diff.outcome2, importance=T, sampsize=c(length(which(dat1.diff.outcome2$y.new==1)), length(which(dat1.diff.outcome2$y.new==1))))
  
  #Variable importance
  importance(rf.dat1.diff.outcome, type=1)
}

#Repeat the whole process
dat1.diff.misc.outcome.func.result <- t(sapply(1:10000, dat1.diff.misc.outcome.func, se1=myse1, sp1=mysp1, se0=myse0, sp0=mysp0))