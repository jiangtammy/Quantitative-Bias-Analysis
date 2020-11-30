#QBA random forests
#Informative dataset #1
#Bias adjustment for differential misclassification of predictors for dataset to assess prediction accuracy

library(foreign)
library(randomForest)
library(pROC)
library(dplyr)

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
dat.diff.cor.pred.func <- function(i, se1, sp1, se0, sp0){
  
  repeat {

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
    
    colnames(dat.diff.pred2) <- c("x1", "x2", "x3", "x4", "y")
  
    #Get the counts for each of the cells in the 2x2 table for the misclassified data set for each predictor
    a.x1 <- length(which(dat.diff.pred2$y==1 & dat.diff.pred2$x1==1))
    b.x1 <- length(which(dat.diff.pred2$y==1 & dat.diff.pred2$x1==0))
    c.x1 <- length(which(dat.diff.pred2$y==0 & dat.diff.pred2$x1==1))
    d.x1 <- length(which(dat.diff.pred2$y==0 & dat.diff.pred2$x1==0))
    x1.dis <- sum(a.x1, b.x1)
    x1.undis <- sum(c.x1, d.x1)
    
    a.x2 <- length(which(dat.diff.pred2$y==1 & dat.diff.pred2$x2==1))
    b.x2 <- length(which(dat.diff.pred2$y==1 & dat.diff.pred2$x2==0))
    c.x2 <- length(which(dat.diff.pred2$y==0 & dat.diff.pred2$x2==1))
    d.x2 <- length(which(dat.diff.pred2$y==0 & dat.diff.pred2$x2==0))
    x2.dis <- sum(a.x2, b.x2)
    x2.undis <- sum(c.x2, d.x2)
    
    a.x3 <- length(which(dat.diff.pred2$y==1 & dat.diff.pred2$x3==1))
    b.x3 <- length(which(dat.diff.pred2$y==1 & dat.diff.pred2$x3==0))
    c.x3 <- length(which(dat.diff.pred2$y==0 & dat.diff.pred2$x3==1))
    d.x3 <- length(which(dat.diff.pred2$y==0 & dat.diff.pred2$x3==0))
    x3.dis <- sum(a.x3, b.x3)
    x3.undis <- sum(c.x3, d.x3)
    
    a.x4 <- length(which(dat.diff.pred2$y==1 & dat.diff.pred2$x4==1))
    b.x4 <- length(which(dat.diff.pred2$y==1 & dat.diff.pred2$x4==0))
    c.x4 <- length(which(dat.diff.pred2$y==0 & dat.diff.pred2$x4==1))
    d.x4 <- length(which(dat.diff.pred2$y==0 & dat.diff.pred2$x4==0))
    x4.dis <- sum(a.x4, b.x4)
    x4.undis <- sum(c.x4, d.x4)
    
    #Calculate expected truth based on sensitivity and specificity
    a.x1.expected <- (a.x1-x1.dis*(1-sp1)) / (se1-(1-sp1))
    b.x1.expected <- x1.dis-a.x1.expected
    c.x1.expected <- (c.x1-x1.undis*(1-sp0)) / (se0-(1-sp0))
    d.x1.expected <- x1.undis-c.x1.expected
    
    a.x2.expected <- (a.x2-x2.dis*(1-sp1)) / (se1-(1-sp1))
    b.x2.expected <- x2.dis-a.x2.expected
    c.x2.expected <- (c.x2-x2.undis*(1-sp0)) / (se0-(1-sp0))
    d.x2.expected <- x2.undis-c.x2.expected
    
    a.x3.expected <- (a.x3-x3.dis*(1-sp1)) / (se1-(1-sp1))
    b.x3.expected <- x3.dis-a.x3.expected
    c.x3.expected <- (c.x3-x3.undis*(1-sp0)) / (se0-(1-sp0))
    d.x3.expected <- x3.undis-c.x3.expected
    
    a.x4.expected <- (a.x4-x4.dis*(1-sp1)) / (se1-(1-sp1))
    b.x4.expected <- x4.dis-a.x4.expected
    c.x4.expected <- (c.x4-x4.undis*(1-sp0)) / (se0-(1-sp0))
    d.x4.expected <- x4.undis-c.x4.expected
    
    #Count up number of negative values
    negative_a.x1.expected <- ifelse(a.x1.expected<0, 1, 0)
    negative_b.x1.expected <- ifelse(b.x1.expected<0, 1, 0)
    negative_c.x1.expected <- ifelse(c.x1.expected<0, 1, 0)
    negative_d.x1.expected <- ifelse(d.x1.expected<0, 1, 0)
    
    negative_a.x2.expected <- ifelse(a.x2.expected<0, 1, 0)
    negative_b.x2.expected <- ifelse(b.x2.expected<0, 1, 0)
    negative_c.x2.expected <- ifelse(c.x2.expected<0, 1, 0)
    negative_d.x2.expected <- ifelse(d.x2.expected<0, 1, 0)
    
    negative_a.x3.expected <- ifelse(a.x3.expected<0, 1, 0)
    negative_b.x3.expected <- ifelse(b.x3.expected<0, 1, 0)
    negative_c.x3.expected <- ifelse(c.x3.expected<0, 1, 0)
    negative_d.x3.expected <- ifelse(d.x3.expected<0, 1, 0)
    
    negative_a.x4.expected <- ifelse(a.x4.expected<0, 1, 0)
    negative_b.x4.expected <- ifelse(b.x4.expected<0, 1, 0)
    negative_c.x4.expected <- ifelse(c.x4.expected<0, 1, 0)
    negative_d.x4.expected <- ifelse(d.x4.expected<0, 1, 0)
    negative_total <- sum(negative_a.x1.expected, negative_b.x1.expected, negative_c.x1.expected, negative_d.x1.expected, negative_a.x2.expected, negative_b.x2.expected, negative_c.x2.expected, negative_d.x2.expected, negative_a.x3.expected, negative_b.x3.expected, negative_c.x3.expected, negative_d.x3.expected, negative_a.x4.expected, negative_b.x4.expected, negative_c.x4.expected, negative_d.x4.expected) #merp
    if (negative_total==0) break
  }
  
  #From the expected truth, calculate false positives, false negatives, true positives, and true negatives for those with the outcome
  t.pos.d.x1 <- se1*a.x1.expected
  t.neg.d.x1 <- sp1*b.x1.expected
  f.neg.d.x1 <- (1-se1)*a.x1.expected
  f.pos.d.x1 <- (1-sp1)*b.x1.expected
  
  t.pos.d.x2 <- se1*a.x2.expected
  t.neg.d.x2 <- sp1*b.x2.expected
  f.neg.d.x2 <- (1-se1)*a.x2.expected
  f.pos.d.x2 <- (1-sp1)*b.x2.expected
  
  t.pos.d.x3 <- se1*a.x3.expected
  t.neg.d.x3 <- sp1*b.x3.expected
  f.neg.d.x3 <- (1-se1)*a.x3.expected
  f.pos.d.x3 <- (1-sp1)*b.x3.expected
  
  t.pos.d.x4 <- se1*a.x4.expected
  t.neg.d.x4 <- sp1*b.x4.expected
  f.neg.d.x4 <- (1-se1)*a.x4.expected
  f.pos.d.x4 <- (1-sp1)*b.x4.expected
  
  #From the expected truth, calculate false positives, false negatives, true positives, and true negatives for those withOUT the outcome
  t.pos.und.x1 <- se0*c.x1.expected
  t.neg.und.x1 <- sp0*d.x1.expected
  f.neg.und.x1 <- (1-se0)*c.x1.expected
  f.pos.und.x1 <- (1-sp0)*d.x1.expected
  
  t.pos.und.x2 <- se0*c.x2.expected
  t.neg.und.x2 <- sp0*d.x2.expected
  f.neg.und.x2 <- (1-se0)*c.x2.expected
  f.pos.und.x2 <- (1-sp0)*d.x2.expected
  
  t.pos.und.x3 <- se0*c.x3.expected
  t.neg.und.x3 <- sp0*d.x3.expected
  f.neg.und.x3 <- (1-se0)*c.x3.expected
  f.pos.und.x3 <- (1-sp0)*d.x3.expected
  
  t.pos.und.x4 <- se0*c.x4.expected
  t.neg.und.x4 <- sp0*d.x4.expected
  f.neg.und.x4 <- (1-se0)*c.x4.expected
  f.pos.und.x4 <- (1-sp0)*d.x4.expected
  
  #From expected truth, calculate PPV and NPV for D+ and D-
  ppv.d.x1 <- t.pos.d.x1 / sum(t.pos.d.x1, f.pos.d.x1)
  npv.d.x1 <- t.neg.d.x1 / sum(t.neg.d.x1, f.neg.d.x1)
  ppv.und.x1 <- t.pos.und.x1 / sum(t.pos.und.x1, f.pos.und.x1)
  npv.und.x1 <- t.neg.und.x1 / sum(t.neg.und.x1, f.neg.und.x1) 
  
  ppv.d.x2 <- t.pos.d.x2 / sum(t.pos.d.x2, f.pos.d.x2)
  npv.d.x2 <- t.neg.d.x2 / sum(t.neg.d.x2, f.neg.d.x2)
  ppv.und.x2 <- t.pos.und.x2 / sum(t.pos.und.x2, f.pos.und.x2)
  npv.und.x2 <- t.neg.und.x2 / sum(t.neg.und.x2, f.neg.und.x2) 
  
  ppv.d.x3 <- t.pos.d.x3 / sum(t.pos.d.x3, f.pos.d.x3)
  npv.d.x3 <- t.neg.d.x3 / sum(t.neg.d.x3, f.neg.d.x3)
  ppv.und.x3 <- t.pos.und.x3 / sum(t.pos.und.x3, f.pos.und.x3)
  npv.und.x3 <- t.neg.und.x3 / sum(t.neg.und.x3, f.neg.und.x3) 
  
  ppv.d.x4 <- t.pos.d.x4 / sum(t.pos.d.x4, f.pos.d.x4)
  npv.d.x4 <- t.neg.d.x4 / sum(t.neg.d.x4, f.neg.d.x4)
  ppv.und.x4 <- t.pos.und.x4 / sum(t.pos.und.x4, f.pos.und.x4)
  npv.und.x4 <- t.neg.und.x4 / sum(t.neg.und.x4, f.neg.und.x4) 
  
  #Record level correction of data using PPV and NPV
  dat.diff.pred2.cor <- dat.diff.pred2
  
  dat.diff.pred2.cor$x1.expected <- NA
  dat.diff.pred2.cor$x1.expected[dat.diff.pred2.cor$x1==1 & dat.diff.pred2.cor$y==1] <- rbinom(length(which(dat.diff.pred2.cor$x1==1 & dat.diff.pred2.cor$y==1)), 1, ppv.d.x1)
  dat.diff.pred2.cor$x1.expected[dat.diff.pred2.cor$x1==0 & dat.diff.pred2.cor$y==1] <- rbinom(length(which(dat.diff.pred2.cor$x1==0 & dat.diff.pred2.cor$y==1)), 1, 1-npv.d.x1)
  dat.diff.pred2.cor$x1.expected[dat.diff.pred2.cor$x1==1 & dat.diff.pred2.cor$y==0] <- rbinom(length(which(dat.diff.pred2.cor$x1==1 & dat.diff.pred2.cor$y==0)), 1, ppv.und.x1)
  dat.diff.pred2.cor$x1.expected[dat.diff.pred2.cor$x1==0 & dat.diff.pred2.cor$y==0] <- rbinom(length(which(dat.diff.pred2.cor$x1==0 & dat.diff.pred2.cor$y==0)), 1, 1-npv.und.x1)
  
  dat.diff.pred2.cor$x2.expected <- NA
  dat.diff.pred2.cor$x2.expected[dat.diff.pred2.cor$x2==1 & dat.diff.pred2.cor$y==1] <- rbinom(length(which(dat.diff.pred2.cor$x2==1 & dat.diff.pred2.cor$y==1)), 1, ppv.d.x2)
  dat.diff.pred2.cor$x2.expected[dat.diff.pred2.cor$x2==0 & dat.diff.pred2.cor$y==1] <- rbinom(length(which(dat.diff.pred2.cor$x2==0 & dat.diff.pred2.cor$y==1)), 1, 1-npv.d.x2)
  dat.diff.pred2.cor$x2.expected[dat.diff.pred2.cor$x2==1 & dat.diff.pred2.cor$y==0] <- rbinom(length(which(dat.diff.pred2.cor$x2==1 & dat.diff.pred2.cor$y==0)), 1, ppv.und.x2)
  dat.diff.pred2.cor$x2.expected[dat.diff.pred2.cor$x2==0 & dat.diff.pred2.cor$y==0] <- rbinom(length(which(dat.diff.pred2.cor$x2==0 & dat.diff.pred2.cor$y==0)), 1, 1-npv.und.x2)
  
  dat.diff.pred2.cor$x3.expected <- NA
  dat.diff.pred2.cor$x3.expected[dat.diff.pred2.cor$x3==1 & dat.diff.pred2.cor$y==1] <- rbinom(length(which(dat.diff.pred2.cor$x3==1 & dat.diff.pred2.cor$y==1)), 1, ppv.d.x3)
  dat.diff.pred2.cor$x3.expected[dat.diff.pred2.cor$x3==0 & dat.diff.pred2.cor$y==1] <- rbinom(length(which(dat.diff.pred2.cor$x3==0 & dat.diff.pred2.cor$y==1)), 1, 1-npv.d.x3)
  dat.diff.pred2.cor$x3.expected[dat.diff.pred2.cor$x3==1 & dat.diff.pred2.cor$y==0] <- rbinom(length(which(dat.diff.pred2.cor$x3==1 & dat.diff.pred2.cor$y==0)), 1, ppv.und.x3)
  dat.diff.pred2.cor$x3.expected[dat.diff.pred2.cor$x3==0 & dat.diff.pred2.cor$y==0] <- rbinom(length(which(dat.diff.pred2.cor$x3==0 & dat.diff.pred2.cor$y==0)), 1, 1-npv.und.x3)
  
  dat.diff.pred2.cor$x4.expected <- NA
  dat.diff.pred2.cor$x4.expected[dat.diff.pred2.cor$x4==1 & dat.diff.pred2.cor$y==1] <- rbinom(length(which(dat.diff.pred2.cor$x4==1 & dat.diff.pred2.cor$y==1)), 1, ppv.d.x4)
  dat.diff.pred2.cor$x4.expected[dat.diff.pred2.cor$x4==0 & dat.diff.pred2.cor$y==1] <- rbinom(length(which(dat.diff.pred2.cor$x4==0 & dat.diff.pred2.cor$y==1)), 1, 1-npv.d.x4)
  dat.diff.pred2.cor$x4.expected[dat.diff.pred2.cor$x4==1 & dat.diff.pred2.cor$y==0] <- rbinom(length(which(dat.diff.pred2.cor$x4==1 & dat.diff.pred2.cor$y==0)), 1, ppv.und.x4)
  dat.diff.pred2.cor$x4.expected[dat.diff.pred2.cor$x4==0 & dat.diff.pred2.cor$y==0] <- rbinom(length(which(dat.diff.pred2.cor$x4==0 & dat.diff.pred2.cor$y==0)), 1, 1-npv.und.x4)
  
  vars <- c("x1.expected", "x2.expected", "x3.expected", "x4.expected", "y")
  dat.diff.pred2.cor <- dat.diff.pred2.cor[vars]
  dat.diff.pred2.cor <- data.frame(lapply(dat.diff.pred2.cor, as.factor))
  
  #Run random forest for misclassified data set #1 with diff misclass. of predictor
  rf.dat.diff.pred <- randomForest(y ~., data=dat.diff.pred2.cor, importance=T, sampsize=c(length(which(dat.diff.pred2.cor$y==1)), length(which(dat.diff.pred2.cor$y==1))))
  
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
dat.diff.cor.pred.func.result <- t(sapply(1:10000, dat.diff.cor.pred.func, se1=myse1, sp1=mysp1, se0=myse0, sp0=mysp0)) 