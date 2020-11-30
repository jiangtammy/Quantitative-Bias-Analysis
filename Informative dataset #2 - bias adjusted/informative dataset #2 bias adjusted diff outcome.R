#QBA random forests
#Informative dataset #2
#Differential misclassification of outcome

library(foreign)
library(randomForest)
library(pROC)
library(dplyr)
library(data.table)

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

################################# Non-differential misclassification of outcome ################################# 
dat2.diff.cor.outcome.func <- function(i, se1, sp1, se0, sp0){
  
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
    
    
    #Misclassification of the outcome
    dat2.diff.outcome1 <- dat2 
    
    dat2.diff.outcome1$y.new <- NA
    dat2.diff.outcome1$y.new[dat2.diff.outcome1$y==1 & dat2.diff.outcome1$x1==1] <- rbinom(length(which(dat2.diff.outcome1$y==1 & dat2.diff.outcome1$x1==1)), 1, se1)
    dat2.diff.outcome1$y.new[dat2.diff.outcome1$y==0 & dat2.diff.outcome1$x1==1] <- rbinom(length(which(dat2.diff.outcome1$y==0 & dat2.diff.outcome1$x1==1)), 1, 1-sp1)
    dat2.diff.outcome1$y.new[dat2.diff.outcome1$y==1 & dat2.diff.outcome1$x1==0] <- rbinom(length(which(dat2.diff.outcome1$y==1 & dat2.diff.outcome1$x1==0)), 1, se0)
    dat2.diff.outcome1$y.new[dat2.diff.outcome1$y==0 & dat2.diff.outcome1$x1==0] <- rbinom(length(which(dat2.diff.outcome1$y==0 & dat2.diff.outcome1$x1==0)), 1, 1-sp0)
    
    
    #Subset data for misclassified variables and the other variables of interest
    vars <- c("x1", "x2", "x3", "x4", "x5", "y.new")
    dat.diff.outcome <- dat2.diff.outcome1[vars] 
    
    dat.diff.outcome$y.new <- as.factor(dat.diff.outcome$y.new)
    
    colnames(dat.diff.outcome) <- c("x1", "x2", "x3", "x4", "x5", "y")
    
    ####################################################################################################################################
    ### Step 1: Get the counts for each of the cells in the very large stratified 2x2 table for the misclassified data set for each predictor
    ####################################################################################################################################
    dat1.new <- dat.diff.outcome %>% 
      mutate(id = group_indices_(dat.diff.outcome, .dots=c("x1", "x2", "x3", "x4", "x5")))  #Give rows that match on values of X1:X5 the same ID. Will be useful later in merging with the dataset with the PPVs and NPVs
    #Replace this with the names of the predictors in the real dataset
    dat2 <- setDT(dat.diff.outcome)[,list(count=.N), names(dat.diff.outcome)]
    
    
    
    
    
    ####################################################################################################################################
    ### Step 2: Calculate the expected true cell values based on sensitivity and specificity
    ####################################################################################################################################
    dat3 <- dat2 %>% 
      mutate(id = group_indices_(dat2, .dots=c("x1", "x2", "x3", "x4", "x5"))) #Replace this with the names of the predictors in the real dataset
    
    ############For persons with the predictor (X1=1)############
    dat.se1.sp1 <- dat3[which(dat3$x1==1),] #Replace x1 with name of predictor
    
    #Obtain the total number of people in each grouping of cells (total number of people with the same id number)
    setDT(dat.se1.sp1)[ , total := sum(count), by = id]
    dat.se1.sp1 <- as.data.frame(dat.se1.sp1)
    
    #Create data rows for cell values of 0
    ids <- dat.se1.sp1[c('id')]
    dat.se1.sp1$freq <- ifelse(duplicated(ids) | duplicated(ids, fromLast = TRUE), 1, 2) #Identify the rows that need to be duplicated
    
    dat.se1.sp1.new <- dat.se1.sp1[rep(seq(nrow(dat.se1.sp1)), dat.se1.sp1$freq), 1:10]  #Duplicate the rows that need to be
    
    dat.se1.sp1.new$test <- ave(
      dat.se1.sp1.new$y, 
      dat.se1.sp1.new$id, 
      FUN = function(x) replace(x, duplicated(x), NA_integer_) #Create marker for which row is a duplicate
    )
    
    dat.se1.sp1.new$y <- as.numeric(as.character(dat.se1.sp1.new$y)) 
    
    dat.se1.sp1.new$y <- ifelse(is.na(dat.se1.sp1.new$test), 1-dat.se1.sp1.new$y, dat.se1.sp1.new$y) #Change the Y value to be the opposite of what it is if it's a duplicate row
    
    dat.se1.sp1.new$count <- ifelse(is.na(dat.se1.sp1.new$test), 0, dat.se1.sp1.new$count) #Change the count to zero for the duplicate rows 
    
    dat.se1.sp1.new$expected <- ifelse(dat.se1.sp1.new$y==1, (dat.se1.sp1.new$count - dat.se1.sp1.new$total*(1-sp1)) / (se1-(1-sp1)), NA) #Calculate expected cell values for those with the outcome
    dat.se1.sp1.new$expected <- ifelse(dat.se1.sp1.new$y==0, dat.se1.sp1.new$total - (((dat.se1.sp1.new$total-dat.se1.sp1.new$count) - dat.se1.sp1.new$total*(1-sp1)) / (se1-(1-sp1))), dat.se1.sp1.new$expected) #Calculate expected cell values for those without the outcome
    
    ############For persons with the predictor (X1=0)############
    dat.se0.sp0 <- dat3[which(dat3$x1==0),] #Change x1 to the predictor
    
    #Obtain the total number of people in each grouping of cells (total number of people with the same id number)
    setDT(dat.se0.sp0)[ , total := sum(count), by = id]
    dat.se0.sp0 <- as.data.frame(dat.se0.sp0)
    
    #Create data rows for cell values of 0
    ids <- dat.se0.sp0[c('id')]
    dat.se0.sp0$freq <- ifelse(duplicated(ids) | duplicated(ids, fromLast = TRUE), 1, 2) #Identify the rows that need to be duplicated
    
    dat.se0.sp0.new <- dat.se0.sp0[rep(seq(nrow(dat.se0.sp0)), dat.se0.sp0$freq), 1:10]  #Duplicate the rows that need to be
    
    dat.se0.sp0.new$test <- ave(
      dat.se0.sp0.new$y, 
      dat.se0.sp0.new$id, 
      FUN = function(x) replace(x, duplicated(x), NA_integer_) #Create marker for which row is a duplicate
    )
    
    dat.se0.sp0.new$y <- as.numeric(as.character(dat.se0.sp0.new$y)) 
    
    dat.se0.sp0.new$y <- ifelse(is.na(dat.se0.sp0.new$test), 1-dat.se0.sp0.new$y, dat.se0.sp0.new$y) #Change the Y value to be the opposite of what it is if it's a duplicate row
    
    dat.se0.sp0.new$count <- ifelse(is.na(dat.se0.sp0.new$test), 0, dat.se0.sp0.new$count) #Change the count to zero for the duplicate rows 
    
    dat.se0.sp0.new$expected <- ifelse(dat.se0.sp0.new$y==1, (dat.se0.sp0.new$count - dat.se0.sp0.new$total*(1-sp0)) / (se0-(1-sp0)), NA) #Calculate expected cell values for those with the outcome
    dat.se0.sp0.new$expected <- ifelse(dat.se0.sp0.new$y==0, dat.se0.sp0.new$total - (((dat.se0.sp0.new$total-dat.se0.sp0.new$count) - dat.se0.sp0.new$total*(1-sp0)) / (se0-(1-sp0))), dat.se0.sp0.new$expected) #Calculate expected cell values for those without the outcome
    
    
    
    
    
    ####################################################################################################################################
    ### Step 3: Count up the number of negative expected cell values 
    ####################################################################################################################################
    if ( length(which(dat.se1.sp1.new$expected<0)) + length(which(dat.se0.sp0.new$expected<0)) == 0 ) break
  }
  
  
  
  
  ####################################################################################################################################
  ### Step 4: From the expected truth, calculate the number of false positives, false negatives, true positives, and true negatives
  ####################################################################################################################################
  ############For persons with the predictor (X1=1)############
  dat.se1.sp1.new$t.pos <- ifelse(dat.se1.sp1.new$y==1, se1*dat.se1.sp1.new$expected, NA)
  dat.se1.sp1.new$t.neg <- ifelse(dat.se1.sp1.new$y==0, sp1*dat.se1.sp1.new$expected, NA)
  dat.se1.sp1.new$f.pos <- ifelse(dat.se1.sp1.new$y==0, (1-sp1)*dat.se1.sp1.new$expected, NA)
  dat.se1.sp1.new$f.neg <- ifelse(dat.se1.sp1.new$y==1, (1-se1)*dat.se1.sp1.new$expected, NA)
  
  setDT(dat.se1.sp1.new)[, t.pos:= t.pos[!is.na(t.pos)] , by = id]
  setDT(dat.se1.sp1.new)[, t.neg:= t.neg[!is.na(t.neg)] , by = id]
  setDT(dat.se1.sp1.new)[, f.pos:= f.pos[!is.na(f.pos)] , by = id]
  setDT(dat.se1.sp1.new)[, f.neg:= f.neg[!is.na(f.neg)] , by = id]
  
  ############For persons with the predictor (X1=0)############
  dat.se0.sp0.new$t.pos <- ifelse(dat.se0.sp0.new$y==1, se0*dat.se0.sp0.new$expected, NA)
  dat.se0.sp0.new$t.neg <- ifelse(dat.se0.sp0.new$y==0, sp0*dat.se0.sp0.new$expected, NA)
  dat.se0.sp0.new$f.pos <- ifelse(dat.se0.sp0.new$y==0, (1-sp0)*dat.se0.sp0.new$expected, NA)
  dat.se0.sp0.new$f.neg <- ifelse(dat.se0.sp0.new$y==1, (1-se0)*dat.se0.sp0.new$expected, NA)
  
  setDT(dat.se0.sp0.new)[, t.pos:= t.pos[!is.na(t.pos)] , by = id]
  setDT(dat.se0.sp0.new)[, t.neg:= t.neg[!is.na(t.neg)] , by = id]
  setDT(dat.se0.sp0.new)[, f.pos:= f.pos[!is.na(f.pos)] , by = id]
  setDT(dat.se0.sp0.new)[, f.neg:= f.neg[!is.na(f.neg)] , by = id]
  
  
  
  
  ####################################################################################################################################
  ### Step 5: From the expected truth, calculate PPV and NPV 
  ####################################################################################################################################
  ############For persons with the predictor (X1=1)############
  dat.se1.sp1.new$ppv <- dat.se1.sp1.new$t.pos / (dat.se1.sp1.new$t.pos + dat.se1.sp1.new$f.pos)
  dat.se1.sp1.new$npv <- dat.se1.sp1.new$t.neg / (dat.se1.sp1.new$t.neg + dat.se1.sp1.new$f.neg)
  
  ############For persons with the predictor (X1=0)############
  dat.se0.sp0.new$ppv <- dat.se0.sp0.new$t.pos / (dat.se0.sp0.new$t.pos + dat.se0.sp0.new$f.pos)
  dat.se0.sp0.new$npv <- dat.se0.sp0.new$t.neg / (dat.se0.sp0.new$t.neg + dat.se0.sp0.new$f.neg)
  
  
  
  
  ####################################################################################################################################
  ### Step 6: Record level correction using PPV and NPV
  ####################################################################################################################################
  #Combine the dat.se1.sp1.new and dat.se0.sp0.new datasets
  dat.se.sp <- rbind(dat.se1.sp1.new, dat.se0.sp0.new)
  
  #Merge the original dataset with the dat.se.sp dataset
  dat.se.sp$y <- as.factor(as.character(dat.se.sp$y))
  dat.pre.final <- merge(dat1.new, dat.se.sp, by=c("id", "y"))
  
  #Record level correction using  PPV and NPV
  dat.pre.final$y.expected <- NA
  dat.pre.final$y.expected[dat.pre.final$y==1] <- rbinom(1, 1, dat.pre.final$ppv)
  dat.pre.final$y.expected[dat.pre.final$y==0] <- rbinom(1, 1, 1-dat.pre.final$npv)

  
  ####################################################################################################################################
  ### Step 7: Random forests using the bias-adjusted dataset 
  ####################################################################################################################################
  dat.final <- dat.pre.final[c(3:7, 24)]
  dat.final$y.expected <- as.factor(dat.final$y.expected)
  
  #Run random forest for misclassified data set #1 with diff misclass. of outcome
  rf.dat2.diff.outcome <- randomForest(y.expected ~., data=dat.final, importance=T, sampsize=c(length(which(dat.final$y.expected==1)), length(which(dat.final$y.expected==1))))
  
  #Variable importance
  importance(rf.dat2.diff.outcome, type=1)
}

#Repeat the whole process
dat2.diff.cor.outcome.func.result <- t(sapply(1:10000, dat2.diff.cor.outcome.func, se1=myse1, sp1=mysp1, se0=myse0, sp0=mysp0)) 

