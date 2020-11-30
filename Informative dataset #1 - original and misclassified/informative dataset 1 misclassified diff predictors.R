#QBA random forests
#Informative dataset #1 
#Differential misclassification of predictors for simulated data to assess prediction accuracy

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


################################# Differential misclassification of all predictors #################################
dat.diff.misc.pred.func <- function(i, se1, sp1, se0, sp0){
  
  n=10000
  x1=rbinom(n, 1, 0.10)
  x2=rbinom(n, 1, 0.35+0.1*x1)
  x3=rbinom(n, 1, 0.20+0.15*x2)
  x4=rbinom(n, 1, 0.25+0.05*x3)
  y=rbinom(n, 1, 0.05+0.05*x1+0.38*x2+0.35*x3+0.17*x4)
  
  dat <- as.data.frame(cbind(x1, x2, x3, x4, y))
  
  dat <- data.frame(lapply(dat, factor))
  
  dat.diff.pred <- dat
  
  dat.diff.pred$x1.new <- NA
  dat.diff.pred$x1.new[dat.diff.pred$x1==1 & dat.diff.pred$y==1] <- rbinom(length(which(dat.diff.pred$x1==1 & dat.diff.pred$y==1)), 1, se1)
  dat.diff.pred$x1.new[dat.diff.pred$x1==1 & dat.diff.pred$y==0] <- rbinom(length(which(dat.diff.pred$x1==1 & dat.diff.pred$y==0)), 1, se0)
  dat.diff.pred$x1.new[dat.diff.pred$x1==0 & dat.diff.pred$y==1] <- rbinom(length(which(dat.diff.pred$x1==0 & dat.diff.pred$y==1)), 1, 1-sp1)
  dat.diff.pred$x1.new[dat.diff.pred$x1==0 & dat.diff.pred$y==0] <- rbinom(length(which(dat.diff.pred$x1==0 & dat.diff.pred$y==0)), 1, 1-sp0)
  
  dat.diff.pred$x2.new <- NA
  dat.diff.pred$x2.new[dat.diff.pred$x2==1 & dat.diff.pred$y==1] <- rbinom(length(which(dat.diff.pred$x2==1 & dat.diff.pred$y==1)), 1, se1)
  dat.diff.pred$x2.new[dat.diff.pred$x2==1 & dat.diff.pred$y==0] <- rbinom(length(which(dat.diff.pred$x2==1 & dat.diff.pred$y==0)), 1, se0)
  dat.diff.pred$x2.new[dat.diff.pred$x2==0 & dat.diff.pred$y==1] <- rbinom(length(which(dat.diff.pred$x2==0 & dat.diff.pred$y==1)), 1, 1-sp1)
  dat.diff.pred$x2.new[dat.diff.pred$x2==0 & dat.diff.pred$y==0] <- rbinom(length(which(dat.diff.pred$x2==0 & dat.diff.pred$y==0)), 1, 1-sp0)
  
  dat.diff.pred$x3.new <- NA
  dat.diff.pred$x3.new[dat.diff.pred$x3==1 & dat.diff.pred$y==1] <- rbinom(length(which(dat.diff.pred$x3==1 & dat.diff.pred$y==1)), 1, se1)
  dat.diff.pred$x3.new[dat.diff.pred$x3==1 & dat.diff.pred$y==0] <- rbinom(length(which(dat.diff.pred$x3==1 & dat.diff.pred$y==0)), 1, se0)
  dat.diff.pred$x3.new[dat.diff.pred$x3==0 & dat.diff.pred$y==1] <- rbinom(length(which(dat.diff.pred$x3==0 & dat.diff.pred$y==1)), 1, 1-sp1)
  dat.diff.pred$x3.new[dat.diff.pred$x3==0 & dat.diff.pred$y==0] <- rbinom(length(which(dat.diff.pred$x3==0 & dat.diff.pred$y==0)), 1, 1-sp0)
  
  dat.diff.pred$x4.new <- NA
  dat.diff.pred$x4.new[dat.diff.pred$x4==1 & dat.diff.pred$y==1] <- rbinom(length(which(dat.diff.pred$x4==1 & dat.diff.pred$y==1)), 1, se1)
  dat.diff.pred$x4.new[dat.diff.pred$x4==1 & dat.diff.pred$y==0] <- rbinom(length(which(dat.diff.pred$x4==1 & dat.diff.pred$y==0)), 1, se0)
  dat.diff.pred$x4.new[dat.diff.pred$x4==0 & dat.diff.pred$y==1] <- rbinom(length(which(dat.diff.pred$x4==0 & dat.diff.pred$y==1)), 1, 1-sp1)
  dat.diff.pred$x4.new[dat.diff.pred$x4==0 & dat.diff.pred$y==0] <- rbinom(length(which(dat.diff.pred$x4==0 & dat.diff.pred$y==0)), 1, 1-sp0)

  #Subset data for misclassified variables and the other variables of interest
  vars <- c("x1.new", "x2.new", "x3.new", "x4.new", "y")
  dat.diff.pred2 <- dat.diff.pred[vars] 
  
  dat.diff.pred2 <- data.frame(lapply(dat.diff.pred2, factor))
  
  #Run random forest for misclassified data set #1 with diff misclass. of predictor
  rf.dat.diff.pred <- randomForest(y ~., data=dat.diff.pred2, importance=T, sampsize=c(length(which(dat.diff.pred2$y==1)), length(which(dat.diff.pred2$y==1))))
  
  #Varimp
  varimp <- importance(rf.dat.diff.pred, type=1)
  
  #AUC
  rf.dat.diff.pred.roc <- roc(dat$y, rf.dat.diff.pred$votes[,2])
  auc <- auc(rf.dat.diff.pred.roc) 
  
  #Other Performance metrics
  confusion.tab <- rf.dat.diff.pred$confusion
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
dat.diff.misc.pred.func.result <- t(sapply(1:10000, dat.diff.misc.pred.func, se1=myse1, sp1=mysp1, se0=myse0, sp0=mysp0))

