#QBA random forests
#Informative dataset #2
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


################################# Differential misclassification of predictor x1 #################################
dat2.diff.cor.pred.func <- function(i, se1, sp1, se0, sp0){
  
  repeat {
    
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
  
  #Create misclassified x1 variable
  dat2.diff.pred$x1.new <- NA
  dat2.diff.pred$x1.new[dat2.diff.pred$x1==1 & dat2.diff.pred$y==1] <- rbinom(length(which(dat2.diff.pred$x1==1 & dat2.diff.pred$y==1)), 1, se1)
  dat2.diff.pred$x1.new[dat2.diff.pred$x1==0 & dat2.diff.pred$y==1] <- rbinom(length(which(dat2.diff.pred$x1==0 & dat2.diff.pred$y==1)), 1, 1-sp1)
  dat2.diff.pred$x1.new[dat2.diff.pred$x1==1 & dat2.diff.pred$y==0] <- rbinom(length(which(dat2.diff.pred$x1==1 & dat2.diff.pred$y==0)), 1, se0)
  dat2.diff.pred$x1.new[dat2.diff.pred$x1==0 & dat2.diff.pred$y==0] <- rbinom(length(which(dat2.diff.pred$x1==0 & dat2.diff.pred$y==0)), 1, 1-sp0)
  
  #Subset data for misclassified variables and the other variables of interest
  vars <- c("x1.new", "x2", "x3", "x4", "x5", "y")
  dat2.diff.pred2 <- dat2.diff.pred[vars] 
  
  dat2.diff.pred2$x1.new <- as.factor(dat2.diff.pred2$x1.new)
  
  #Get the counts for each of the cells in the 2x2 table for the misclassified data set for predictor x1
  a.x1 <- length(which(dat2.diff.pred2$y==1 & dat2.diff.pred2$x1.new==1))
  b.x1 <- length(which(dat2.diff.pred2$y==1 & dat2.diff.pred2$x1.new==0))
  c.x1 <- length(which(dat2.diff.pred2$y==0 & dat2.diff.pred2$x1.new==1))
  d.x1 <- length(which(dat2.diff.pred2$y==0 & dat2.diff.pred2$x1.new==0))
  x1.dis <- sum(a.x1, b.x1)
  x1.undis <- sum(c.x1, d.x1)
  
  #Calculate expected truth based on sensitivity and specificity
  a.x1.expected <- (a.x1-x1.dis*(1-sp1)) / (se1-(1-sp1))
  b.x1.expected <- x1.dis-a.x1.expected
  c.x1.expected <- (c.x1-x1.undis*(1-sp0)) / (se0-(1-sp0))
  d.x1.expected <- x1.undis-c.x1.expected
  
  #Count up number of negative values
  negative_a.x1.expected <- ifelse(a.x1.expected<0, 1, 0)
  negative_b.x1.expected <- ifelse(b.x1.expected<0, 1, 0)
  negative_c.x1.expected <- ifelse(c.x1.expected<0, 1, 0)
  negative_d.x1.expected <- ifelse(d.x1.expected<0, 1, 0)
  
  negative_total <- sum(negative_a.x1.expected, negative_b.x1.expected, negative_c.x1.expected, negative_d.x1.expected) #merp
  if (negative_total==0) break
  }
  
  #From the expected truth, calculate false positives, false negatives, true positives, and true negatives for those with the outcome
  t.pos.d.x1 <- se1*a.x1.expected
  t.neg.d.x1 <- sp1*b.x1.expected
  f.neg.d.x1 <- (1-se1)*a.x1.expected
  f.pos.d.x1 <- (1-sp1)*b.x1.expected
  
  #From the expected truth, calculate false positives, false negatives, true positives, and true negatives for those withOUT the outcome
  t.pos.und.x1 <- se0*c.x1.expected
  t.neg.und.x1 <- sp0*d.x1.expected
  f.neg.und.x1 <- (1-se0)*c.x1.expected
  f.pos.und.x1 <- (1-sp0)*d.x1.expected
  
  #From expected truth, calculate PPV and NPV for D+ and D-
  ppv.d.x1 <- t.pos.d.x1 / sum(t.pos.d.x1, f.pos.d.x1)
  npv.d.x1 <- t.neg.d.x1 / sum(t.neg.d.x1, f.neg.d.x1)
  ppv.und.x1 <- t.pos.und.x1 / sum(t.pos.und.x1, f.pos.und.x1)
  npv.und.x1 <- t.neg.und.x1 / sum(t.neg.und.x1, f.neg.und.x1) 
  
  #Record level correction of data using PPV and NPV
  dat.diff.predictors.cor <- dat2.diff.pred2
  
  dat.diff.predictors.cor$x1.expected <- NA
  dat.diff.predictors.cor$x1.expected[dat.diff.predictors.cor$x1.new==1 & dat.diff.predictors.cor$y==1] <- rbinom(length(which(dat.diff.predictors.cor$x1.new==1 & dat.diff.predictors.cor$y==1)), 1, ppv.d.x1)
  dat.diff.predictors.cor$x1.expected[dat.diff.predictors.cor$x1.new==0 & dat.diff.predictors.cor$y==1] <- rbinom(length(which(dat.diff.predictors.cor$x1.new==0 & dat.diff.predictors.cor$y==1)), 1, 1-npv.d.x1)
  dat.diff.predictors.cor$x1.expected[dat.diff.predictors.cor$x1.new==1 & dat.diff.predictors.cor$y==0] <- rbinom(length(which(dat.diff.predictors.cor$x1.new==1 & dat.diff.predictors.cor$y==0)), 1, ppv.und.x1)
  dat.diff.predictors.cor$x1.expected[dat.diff.predictors.cor$x1.new==0 & dat.diff.predictors.cor$y==0] <- rbinom(length(which(dat.diff.predictors.cor$x1.new==0 & dat.diff.predictors.cor$y==0)), 1, 1-npv.und.x1)
  
  #Subset data for only variables of interest
  vars <- c("x1.expected", "x2", "x3", "x4", "x5", "y")
  dat.diff.predictors.cor <- dat.diff.predictors.cor[vars]
  dat.diff.predictors.cor <- data.frame(lapply(dat.diff.predictors.cor, as.factor))

  
  #Run random forest for corrected data set #1 with diff misclass. of predictor
  rf.dat.diff.predictors.cor <- randomForest(y ~., data=dat.diff.predictors.cor, importance=T, sampsize=c(length(which(dat.diff.predictors.cor$y==1)), length(which(dat.diff.predictors.cor$y==1))))
  
  #Variable importance
  importance(rf.dat.diff.predictors.cor, type=1)
}


#Repeat the whole process
dat2.diff.cor.pred.func.result <- t(sapply(1:10000, dat2.diff.cor.pred.func, se1=myse1, sp1=mysp1, se0=myse0, sp0=mysp0)) 