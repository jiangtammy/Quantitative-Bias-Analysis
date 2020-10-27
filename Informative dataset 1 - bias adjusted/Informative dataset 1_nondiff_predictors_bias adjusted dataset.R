#QBA random forests - non-differential misclassification of predictors for dataset to assess prediction accuracy

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


################################# Data set #1: Non-differential misclassification of all predictors #################################
dat.nondiff.cor.pred.func <- function(i, se, sp){
  
  repeat {
    
    n=10000
    x1=rbinom(n, 1, 0.10)
    x2=rbinom(n, 1, 0.35+0.1*x1)
    x3=rbinom(n, 1, 0.20+0.15*x2)
    x4=rbinom(n, 1, 0.25+0.05*x3)
    y=rbinom(n, 1, 0.05+0.05*x1+0.38*x2+0.35*x3+0.17*x4)
  
  dat <- as.data.frame(cbind(x1, x2, x3, x4, y))
  
  dat <- data.frame(lapply(dat, factor))
  
  dat.nondiff.pred <- dat
  
  #Misclassify predictors
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
  
  colnames(dat.nondiff.pred2) <- c("x1", "x2", "x3", "x4", "y")
  
  #Get the counts for each of the cells in the 2x2 table for the misclassified data set for each predictor
  a.x1 <- length(which(dat.nondiff.pred2$y==1 & dat.nondiff.pred2$x1==1))
  b.x1 <- length(which(dat.nondiff.pred2$y==1 & dat.nondiff.pred2$x1==0))
  c.x1 <- length(which(dat.nondiff.pred2$y==0 & dat.nondiff.pred2$x1==1))
  d.x1 <- length(which(dat.nondiff.pred2$y==0 & dat.nondiff.pred2$x1==0))
  x1.dis <- sum(a.x1, b.x1)
  x1.undis <- sum(c.x1, d.x1)
  
  a.x2 <- length(which(dat.nondiff.pred2$y==1 & dat.nondiff.pred2$x2==1))
  b.x2 <- length(which(dat.nondiff.pred2$y==1 & dat.nondiff.pred2$x2==0))
  c.x2 <- length(which(dat.nondiff.pred2$y==0 & dat.nondiff.pred2$x2==1))
  d.x2 <- length(which(dat.nondiff.pred2$y==0 & dat.nondiff.pred2$x2==0))
  x2.dis <- sum(a.x2, b.x2)
  x2.undis <- sum(c.x2, d.x2)
  
  a.x3 <- length(which(dat.nondiff.pred2$y==1 & dat.nondiff.pred2$x3==1))
  b.x3 <- length(which(dat.nondiff.pred2$y==1 & dat.nondiff.pred2$x3==0))
  c.x3 <- length(which(dat.nondiff.pred2$y==0 & dat.nondiff.pred2$x3==1))
  d.x3 <- length(which(dat.nondiff.pred2$y==0 & dat.nondiff.pred2$x3==0))
  x3.dis <- sum(a.x3, b.x3)
  x3.undis <- sum(c.x3, d.x3)
  
  a.x4 <- length(which(dat.nondiff.pred2$y==1 & dat.nondiff.pred2$x4==1))
  b.x4 <- length(which(dat.nondiff.pred2$y==1 & dat.nondiff.pred2$x4==0))
  c.x4 <- length(which(dat.nondiff.pred2$y==0 & dat.nondiff.pred2$x4==1))
  d.x4 <- length(which(dat.nondiff.pred2$y==0 & dat.nondiff.pred2$x4==0))
  x4.dis <- sum(a.x4, b.x4)
  x4.undis <- sum(c.x4, d.x4)
  
  #Calculate expected truth based on sensitivity and specificity
  a.x1.expected <- (a.x1-x1.dis*(1-sp)) / (se-(1-sp))
  b.x1.expected <- x1.dis-a.x1.expected
  c.x1.expected <- (c.x1-x1.undis*(1-sp)) / (se-(1-sp))
  d.x1.expected <- x1.undis-c.x1.expected
  
  a.x2.expected <- (a.x2-x2.dis*(1-sp)) / (se-(1-sp))
  b.x2.expected <- x2.dis-a.x2.expected
  c.x2.expected <- (c.x2-x2.undis*(1-sp)) / (se-(1-sp))
  d.x2.expected <- x2.undis-c.x2.expected
  
  a.x3.expected <- (a.x3-x3.dis*(1-sp)) / (se-(1-sp))
  b.x3.expected <- x3.dis-a.x3.expected
  c.x3.expected <- (c.x3-x3.undis*(1-sp)) / (se-(1-sp))
  d.x3.expected <- x3.undis-c.x3.expected
  
  a.x4.expected <- (a.x4-x4.dis*(1-sp)) / (se-(1-sp))
  b.x4.expected <- x4.dis-a.x4.expected
  c.x4.expected <- (c.x4-x4.undis*(1-sp)) / (se-(1-sp))
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
  negative_total <- sum(negative_a.x1.expected, negative_b.x1.expected, negative_c.x1.expected, negative_d.x1.expected, negative_a.x2.expected, negative_b.x2.expected, negative_c.x2.expected, negative_d.x2.expected, negative_a.x3.expected, negative_b.x3.expected, negative_c.x3.expected, negative_d.x3.expected, negative_a.x3.expected, negative_b.x3.expected, negative_c.x3.expected, negative_d.x3.expected) #merp
  if (negative_total==0) break
  }
  
  #From the expected truth, calculate false positives, false negatives, true positives, and true negatives for those with the outcome
  t.pos.d.x1 <- se*a.x1.expected
  t.neg.d.x1 <- sp*b.x1.expected
  f.neg.d.x1 <- (1-se)*a.x1.expected
  f.pos.d.x1 <- (1-sp)*b.x1.expected
  
  t.pos.d.x2 <- se*a.x2.expected
  t.neg.d.x2 <- sp*b.x2.expected
  f.neg.d.x2 <- (1-se)*a.x2.expected
  f.pos.d.x2 <- (1-sp)*b.x2.expected
  
  t.pos.d.x3 <- se*a.x3.expected
  t.neg.d.x3 <- sp*b.x3.expected
  f.neg.d.x3 <- (1-se)*a.x3.expected
  f.pos.d.x3 <- (1-sp)*b.x3.expected
  
  t.pos.d.x4 <- se*a.x4.expected
  t.neg.d.x4 <- sp*b.x4.expected
  f.neg.d.x4 <- (1-se)*a.x4.expected
  f.pos.d.x4 <- (1-sp)*b.x4.expected
  
  #From the expected truth, calculate false positives, false negatives, true positives, and true negatives for those withOUT the outcome
  t.pos.und.x1 <- se*c.x1.expected
  t.neg.und.x1 <- sp*d.x1.expected
  f.neg.und.x1 <- (1-se)*c.x1.expected
  f.pos.und.x1 <- (1-sp)*d.x1.expected
  
  t.pos.und.x2 <- se*c.x2.expected
  t.neg.und.x2 <- sp*d.x2.expected
  f.neg.und.x2 <- (1-se)*c.x2.expected
  f.pos.und.x2 <- (1-sp)*d.x2.expected
  
  t.pos.und.x3 <- se*c.x3.expected
  t.neg.und.x3 <- sp*d.x3.expected
  f.neg.und.x3 <- (1-se)*c.x3.expected
  f.pos.und.x3 <- (1-sp)*d.x3.expected
  
  t.pos.und.x4 <- se*c.x4.expected
  t.neg.und.x4 <- sp*d.x4.expected
  f.neg.und.x4 <- (1-se)*c.x4.expected
  f.pos.und.x4 <- (1-sp)*d.x4.expected
  
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
  dat.nondiff.pred2.cor <- dat.nondiff.pred2
  
  dat.nondiff.pred2.cor$x1.expected <- NA
  dat.nondiff.pred2.cor$x1.expected[dat.nondiff.pred2.cor$x1==1 & dat.nondiff.pred2.cor$y==1] <- rbinom(length(which(dat.nondiff.pred2.cor$x1==1 & dat.nondiff.pred2.cor$y==1)), 1, ppv.d.x1)
  dat.nondiff.pred2.cor$x1.expected[dat.nondiff.pred2.cor$x1==0 & dat.nondiff.pred2.cor$y==1] <- rbinom(length(which(dat.nondiff.pred2.cor$x1==0 & dat.nondiff.pred2.cor$y==1)), 1, 1-npv.d.x1)
  dat.nondiff.pred2.cor$x1.expected[dat.nondiff.pred2.cor$x1==1 & dat.nondiff.pred2.cor$y==0] <- rbinom(length(which(dat.nondiff.pred2.cor$x1==1 & dat.nondiff.pred2.cor$y==0)), 1, ppv.und.x1)
  dat.nondiff.pred2.cor$x1.expected[dat.nondiff.pred2.cor$x1==0 & dat.nondiff.pred2.cor$y==0] <- rbinom(length(which(dat.nondiff.pred2.cor$x1==0 & dat.nondiff.pred2.cor$y==0)), 1, 1-npv.und.x1)
  
  dat.nondiff.pred2.cor$x2.expected <- NA
  dat.nondiff.pred2.cor$x2.expected[dat.nondiff.pred2.cor$x2==1 & dat.nondiff.pred2.cor$y==1] <- rbinom(length(which(dat.nondiff.pred2.cor$x2==1 & dat.nondiff.pred2.cor$y==1)), 1, ppv.d.x2)
  dat.nondiff.pred2.cor$x2.expected[dat.nondiff.pred2.cor$x2==0 & dat.nondiff.pred2.cor$y==1] <- rbinom(length(which(dat.nondiff.pred2.cor$x2==0 & dat.nondiff.pred2.cor$y==1)), 1, 1-npv.d.x2)
  dat.nondiff.pred2.cor$x2.expected[dat.nondiff.pred2.cor$x2==1 & dat.nondiff.pred2.cor$y==0] <- rbinom(length(which(dat.nondiff.pred2.cor$x2==1 & dat.nondiff.pred2.cor$y==0)), 1, ppv.und.x2)
  dat.nondiff.pred2.cor$x2.expected[dat.nondiff.pred2.cor$x2==0 & dat.nondiff.pred2.cor$y==0] <- rbinom(length(which(dat.nondiff.pred2.cor$x2==0 & dat.nondiff.pred2.cor$y==0)), 1, 1-npv.und.x2)
  
  dat.nondiff.pred2.cor$x3.expected <- NA
  dat.nondiff.pred2.cor$x3.expected[dat.nondiff.pred2.cor$x3==1 & dat.nondiff.pred2.cor$y==1] <- rbinom(length(which(dat.nondiff.pred2.cor$x3==1 & dat.nondiff.pred2.cor$y==1)), 1, ppv.d.x3)
  dat.nondiff.pred2.cor$x3.expected[dat.nondiff.pred2.cor$x3==0 & dat.nondiff.pred2.cor$y==1] <- rbinom(length(which(dat.nondiff.pred2.cor$x3==0 & dat.nondiff.pred2.cor$y==1)), 1, 1-npv.d.x3)
  dat.nondiff.pred2.cor$x3.expected[dat.nondiff.pred2.cor$x3==1 & dat.nondiff.pred2.cor$y==0] <- rbinom(length(which(dat.nondiff.pred2.cor$x3==1 & dat.nondiff.pred2.cor$y==0)), 1, ppv.und.x3)
  dat.nondiff.pred2.cor$x3.expected[dat.nondiff.pred2.cor$x3==0 & dat.nondiff.pred2.cor$y==0] <- rbinom(length(which(dat.nondiff.pred2.cor$x3==0 & dat.nondiff.pred2.cor$y==0)), 1, 1-npv.und.x3)
  
  dat.nondiff.pred2.cor$x4.expected <- NA
  dat.nondiff.pred2.cor$x4.expected[dat.nondiff.pred2.cor$x4==1 & dat.nondiff.pred2.cor$y==1] <- rbinom(length(which(dat.nondiff.pred2.cor$x4==1 & dat.nondiff.pred2.cor$y==1)), 1, ppv.d.x4)
  dat.nondiff.pred2.cor$x4.expected[dat.nondiff.pred2.cor$x4==0 & dat.nondiff.pred2.cor$y==1] <- rbinom(length(which(dat.nondiff.pred2.cor$x4==0 & dat.nondiff.pred2.cor$y==1)), 1, 1-npv.d.x4)
  dat.nondiff.pred2.cor$x4.expected[dat.nondiff.pred2.cor$x4==1 & dat.nondiff.pred2.cor$y==0] <- rbinom(length(which(dat.nondiff.pred2.cor$x4==1 & dat.nondiff.pred2.cor$y==0)), 1, ppv.und.x4)
  dat.nondiff.pred2.cor$x4.expected[dat.nondiff.pred2.cor$x4==0 & dat.nondiff.pred2.cor$y==0] <- rbinom(length(which(dat.nondiff.pred2.cor$x4==0 & dat.nondiff.pred2.cor$y==0)), 1, 1-npv.und.x4)
  
  vars <- c("x1.expected", "x2.expected", "x3.expected", "x4.expected", "y")
  dat.nondiff.pred2.cor <- dat.nondiff.pred2.cor[vars]
  dat.nondiff.pred2.cor <- data.frame(lapply(dat.nondiff.pred2.cor, as.factor))

  
  #Run random forest for corrected data set #1 with nondiff misclass. of predictor
  rf.dat.nondiff.pred2.cor <- randomForest(y ~., data=dat.nondiff.pred2.cor, importance=T, sampsize=c(length(which(dat.nondiff.pred2.cor$y==1)), length(which(dat.nondiff.pred2.cor$y==1))))
  
  #Varimp
  varimp <- importance(rf.dat.nondiff.pred2.cor, type=1)
  
  #AUC
  rf.data.nondiff.pred2.roc <- roc(dat$y, rf.dat.nondiff.pred2.cor$votes[,2])
  auc <- auc(rf.data.nondiff.pred2.roc) 
  
  #Other Performance metrics
  confusion.tab <- rf.dat.nondiff.pred2.cor$confusion
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
dat.nondiff.cor.pred.func.result <- t(sapply(1:1000, dat.nondiff.cor.pred.func, se=myse, sp=mysp))  #Ran this 10x (10000 simulations total)

write.csv(dat.nondiff.cor.pred.func.result, col.names=F,
          paste0("results/dat_nondiff_cor_predictor_myse_", myse, "_mysp_", mysp, "_", 
                 format(Sys.time(), "%m%d%Y_%s"), ".csv"),
          quote=F, row.names=F)
