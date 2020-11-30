#QBA random forests
#Informative dataset #1
#Non-differential misclassification of predictor for simulated data to assess prediction accuracy

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


################################# Non-differential misclassification of all predictors #################################
dat.nondiff.misc.pred.func <- function(i, se, sp){
  
  n=10000
  x1=rbinom(n, 1, 0.10)
  x2=rbinom(n, 1, 0.35+0.1*x1)
  x3=rbinom(n, 1, 0.20+0.15*x2)
  x4=rbinom(n, 1, 0.25+0.05*x3)
  y=rbinom(n, 1, 0.05+0.05*x1+0.38*x2+0.35*x3+0.17*x4)
  
  dat <- as.data.frame(cbind(x1, x2, x3, x4, y))
  
  dat <- data.frame(lapply(dat, factor))
  
  dat.nondiff.pred <- dat
  
  dat.nondiff.pred$x1.new <- NA
  dat.nondiff.pred$x1.new[dat.nondiff.pred$x1==1] <- rbinom(length(which(dat.nondiff.pred$x1==1)), 1, se)
  dat.nondiff.pred$x1.new[dat.nondiff.pred$x1==0] <- rbinom(length(which(dat.nondiff.pred$x1==0)), 1, 1-sp)
  
  dat.nondiff.pred$x2.new <- NA
  dat.nondiff.pred$x2.new[dat.nondiff.pred$x2==1] <- rbinom(length(which(dat.nondiff.pred$x2==1)), 1, se)
  dat.nondiff.pred$x2.new[dat.nondiff.pred$x2==0] <- rbinom(length(which(dat.nondiff.pred$x2==0)), 1, 1-sp)
  
  dat.nondiff.pred$x3.new <- NA
  dat.nondiff.pred$x3.new[dat.nondiff.pred$x3==1] <- rbinom(length(which(dat.nondiff.pred$x3==1)), 1, se)
  dat.nondiff.pred$x3.new[dat.nondiff.pred$x3==0] <- rbinom(length(which(dat.nondiff.pred$x3==0)), 1, 1-sp)
  
  dat.nondiff.pred$x4.new <- NA
  dat.nondiff.pred$x4.new[dat.nondiff.pred$x4==1] <- rbinom(length(which(dat.nondiff.pred$x4==1)), 1, se)
  dat.nondiff.pred$x4.new[dat.nondiff.pred$x4==0] <- rbinom(length(which(dat.nondiff.pred$x4==0)), 1, 1-sp)
  
  #Subset data for misclassified variables and the other variables of interest
  vars <- c("x1.new", "x2.new", "x3.new", "x4.new", "y")
  dat.nondiff.pred2 <- dat.nondiff.pred[vars] 
  
  dat.nondiff.pred2 <- data.frame(lapply(dat.nondiff.pred2, factor))
  
  #Run random forest for misclassified data set #1 with nondiff misclass. of predictor
  rf.dat.nondiff.pred <- randomForest(y ~., data=dat.nondiff.pred2, importance=T, sampsize=c(length(which(dat.nondiff.pred2$y==1)), length(which(dat.nondiff.pred2$y==1))))
  
  #Varimp
  varimp <- importance(rf.dat.nondiff.pred, type=1)
  
  #AUC
  rf.dat.nondiff.pred.roc <- roc(dat$y, rf.dat.nondiff.pred$votes[,2])
  auc <- auc(rf.dat.nondiff.pred.roc) 
  
  #Other Performance metrics
  confusion.tab <- rf.dat.nondiff.pred$confusion
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
dat.nondiff.misc.pred.func.result <- t(sapply(1:10000, dat.nondiff.misc.pred.func, se=myse,sp=mysp)) 