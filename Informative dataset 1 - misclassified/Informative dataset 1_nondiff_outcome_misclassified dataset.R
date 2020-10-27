#QBA random forests - non-differential outcome misclassification for simulated data to assess prediction accuracy

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

################################# True data set #1 #################################
################################# Non-differential outcome misclassification #################################
dat.nondiff.misc.outcome.func <- function(i, se, sp){
  
  n=10000
  x1=rbinom(n, 1, 0.10)
  x2=rbinom(n, 1, 0.35+0.1*x1)
  x3=rbinom(n, 1, 0.20+0.15*x2)
  x4=rbinom(n, 1, 0.25+0.05*x3)
  y=rbinom(n, 1, 0.05+0.05*x1+0.38*x2+0.35*x3+0.17*x4)
  
  dat <- as.data.frame(cbind(x1, x2, x3, x4, y))
  
  dat <- data.frame(lapply(dat, factor))
  
  dat.nondiff.outcome <- dat
  
  dat.nondiff.outcome$y.new <- NA
  dat.nondiff.outcome$y.new[dat.nondiff.outcome$y==1] <- rbinom(length(which(dat.nondiff.outcome$y==1)), 1, se)
  dat.nondiff.outcome$y.new[dat.nondiff.outcome$y==0] <- rbinom(length(which(dat.nondiff.outcome$y==0)), 1, 1-sp)
  
  
  #Subset data for misclassified variables and the other variables of interest
  vars <- c("x1", "x2", "x3", "x4", "y.new")
  dat.nondiff.outcome2 <- dat.nondiff.outcome[vars] 
  
  dat.nondiff.outcome2 <- data.frame(lapply(dat.nondiff.outcome2, factor))
  
  #Run random forest for misclassified data set #1 with diff misclass. of predictor
  rf.dat.nondiff.outcome <- randomForest(y.new ~., data=dat.nondiff.outcome2, importance=T, sampsize=c(length(which(dat.nondiff.outcome2$y.new==1)), length(which(dat.nondiff.outcome2$y.new==1))))
  
  #Varimp
  varimp <- importance(rf.dat.nondiff.outcome, type=1)
  
  #AUC
  rf.dat.nondiff.outcome.roc <- roc(dat$y, rf.dat.nondiff.outcome$votes[,2])
  auc <- auc(rf.dat.nondiff.outcome.roc) 
  
  #Other Performance metrics
  confusion.tab <- rf.dat.nondiff.outcome$confusion
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
dat.nondiff.misc.outcome.func.result <- t(sapply(1:1000, dat.nondiff.misc.outcome.func, se=myse, sp=mysp)) #Ran 10x for 10000 simulations total

write.csv(dat.nondiff.misc.outcome.func.result, col.names=F,
          paste0("results/dat_nondiff_misc_outcome_myse_", myse, "_mysp_", mysp, 
                 format(Sys.time(), "%m%d%Y_%s"), ".csv"),
          quote=F, row.names=F)

