#QBA random forests
#Uninformative dataset
#Bias-adjusted for non-differential misclassification of predictor 

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
dat1.nondiff.cor.pred.func <- function(i, se, sp){
  
  repeat {
    
    n=10000
    dat1 = data.frame(x1=rbinom(n, 1, 0.1),
                      x2=rbinom(n, 1, 0.1),
                      x3=rbinom(n, 1, 0.1),
                      x4=rbinom(n, 1, 0.1),
                      x5=rbinom(n, 1, 0.1),
                      y=rbinom(n, 1, 0.2))
  
  dat1.nondiff.pred <- dat1
  
  #Create misclassified X5 variable
  dat1.nondiff.pred$x5.new <- NA
  dat1.nondiff.pred$x5.new[dat1.nondiff.pred$x5==1] <- rbinom(length(which(dat1.nondiff.pred$x5==1)), 1, se)
  dat1.nondiff.pred$x5.new[dat1.nondiff.pred$x5==0] <- rbinom(length(which(dat1.nondiff.pred$x5==0)), 1, 1-sp)
  
  #Subset data for misclassified variables and the other variables of interest
  vars <- c("x1", "x2", "x3", "x4", "x5.new", "y")
  dat1.nondiff.pred2 <- dat1.nondiff.pred[vars] 
  
  dat1.nondiff.pred2 <- data.frame(lapply(dat1.nondiff.pred2, factor))
  
  #Get the counts for each of the cells in the 2x2 table for the misclassified data set for predictor X5
  a.x5 <- length(which(dat1.nondiff.pred2$y==1 & dat1.nondiff.pred2$x5.new==1))
  b.x5 <- length(which(dat1.nondiff.pred2$y==1 & dat1.nondiff.pred2$x5.new==0))
  c.x5 <- length(which(dat1.nondiff.pred2$y==0 & dat1.nondiff.pred2$x5.new==1))
  d.x5 <- length(which(dat1.nondiff.pred2$y==0 & dat1.nondiff.pred2$x5.new==0))
  x5.dis <- sum(a.x5, b.x5)
  x5.undis <- sum(c.x5, d.x5)
  
  #Calculate expected truth based on sensitivity and specificity
  a.x5.expected <- (a.x5-x5.dis*(1-sp)) / (se-(1-sp))
  b.x5.expected <- x5.dis-a.x5.expected
  c.x5.expected <- (c.x5-x5.undis*(1-sp)) / (se-(1-sp))
  d.x5.expected <- x5.undis-c.x5.expected
  
  #Count up number of negative values
  negative_a.x5.expected <- ifelse(a.x5.expected<0, 1, 0)
  negative_b.x5.expected <- ifelse(b.x5.expected<0, 1, 0)
  negative_c.x5.expected <- ifelse(c.x5.expected<0, 1, 0)
  negative_d.x5.expected <- ifelse(d.x5.expected<0, 1, 0)
  
  negative_total <- sum(negative_a.x5.expected, negative_b.x5.expected, negative_c.x5.expected, negative_d.x5.expected) 
  if (negative_total==0) break
  }
  
  #From the expected truth, calculate false positives, false negatives, true positives, and true negatives for those with the outcome
  t.pos.d.x5 <- se*a.x5.expected
  t.neg.d.x5 <- sp*b.x5.expected
  f.neg.d.x5 <- (1-se)*a.x5.expected
  f.pos.d.x5 <- (1-sp)*b.x5.expected
  
  #From the expected truth, calculate false positives, false negatives, true positives, and true negatives for those withOUT the outcome
  t.pos.und.x5 <- se*c.x5.expected
  t.neg.und.x5 <- sp*d.x5.expected
  f.neg.und.x5 <- (1-se)*c.x5.expected
  f.pos.und.x5 <- (1-sp)*d.x5.expected
  
  #From expected truth, calculate PPV and NPV for D+ and D-
  ppv.d.x5 <- t.pos.d.x5 / sum(t.pos.d.x5, f.pos.d.x5)
  npv.d.x5 <- t.neg.d.x5 / sum(t.neg.d.x5, f.neg.d.x5)
  ppv.und.x5 <- t.pos.und.x5 / sum(t.pos.und.x5, f.pos.und.x5)
  npv.und.x5 <- t.neg.und.x5 / sum(t.neg.und.x5, f.neg.und.x5) 
  
  #Record level correction of data using PPV and NPV
  dat.nondiff.predictors.cor <- dat1.nondiff.pred2
  
  dat.nondiff.predictors.cor$x5.expected <- NA
  dat.nondiff.predictors.cor$x5.expected[dat.nondiff.predictors.cor$x5.new==1 & dat.nondiff.predictors.cor$y==1] <- rbinom(length(which(dat.nondiff.predictors.cor$x5.new==1 & dat.nondiff.predictors.cor$y==1)), 1, ppv.d.x5)
  dat.nondiff.predictors.cor$x5.expected[dat.nondiff.predictors.cor$x5.new==0 & dat.nondiff.predictors.cor$y==1] <- rbinom(length(which(dat.nondiff.predictors.cor$x5.new==0 & dat.nondiff.predictors.cor$y==1)), 1, 1-npv.d.x5)
  dat.nondiff.predictors.cor$x5.expected[dat.nondiff.predictors.cor$x5.new==1 & dat.nondiff.predictors.cor$y==0] <- rbinom(length(which(dat.nondiff.predictors.cor$x5.new==1 & dat.nondiff.predictors.cor$y==0)), 1, ppv.und.x5)
  dat.nondiff.predictors.cor$x5.expected[dat.nondiff.predictors.cor$x5.new==0 & dat.nondiff.predictors.cor$y==0] <- rbinom(length(which(dat.nondiff.predictors.cor$x5.new==0 & dat.nondiff.predictors.cor$y==0)), 1, 1-npv.und.x5)
  
  #Subset data for only variables of interest
  vars <- c("x1", "x2", "x3", "x4", "x5.expected", "y")
  dat.nondiff.predictors.cor <- dat.nondiff.predictors.cor[vars]
  dat.nondiff.predictors.cor <- data.frame(lapply(dat.nondiff.predictors.cor, as.factor))

  
  #Run random forest for corrected data set #1 with nondiff misclass. of predictor
  rf.dat.nondiff.predictors.cor <- randomForest(y ~., data=dat.nondiff.predictors.cor, importance=T, sampsize=c(length(which(dat.nondiff.predictors.cor$y==1)), length(which(dat.nondiff.predictors.cor$y==1))))
  
  #Variable importance
  importance(rf.dat.nondiff.predictors.cor, type=1)
}


#Repeat the whole process
dat1.nondiff.cor.pred.func.result <- t(sapply(1:10000, dat1.nondiff.cor.pred.func, se=myse,sp=mysp)) 