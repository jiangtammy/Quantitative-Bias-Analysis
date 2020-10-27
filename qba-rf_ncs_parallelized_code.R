#QBA of misclassification in random forests
#Correcting for exposure misclassification in the National Comorbidity Survey - Replication Study

library(Hmisc)
library(tidyr)
library(randomForest)
library(pROC)
library(dplyr)

#Get arguments from the command line
argv <- commandArgs(TRUE)

# Check if the command line is not empty and convert values to numerical values
if (length(argv) > 0){
  trial <- as.numeric( argv[1] )
} else {
  stop("Please input parameters")
}

#Load data set
ncs <- spss.get("/projectnb/qba-rf/simulated_data_and_CPES_combined/ncsr/20240-0002-Data.sav", use.value.labels=TRUE)

####################################################################################################################################
### Subsetting data and recoding variables  
####################################################################################################################################
#Subset data for only variables of interest
vars <- c("SD2", "SD15", "SD6", "SD10A", "SD19", "SD23A", "DSM.PDS", "PD.OND", "DSM.AGO", "AGO.OND", "DSM.SP", "SP.OND", "DSM.SO", "SO.OND", "DSM.PTS", "PTS.OND", "DSM.GAD", "GAD.OND", "DSM.MDDH", "MDDH.OND", "DSM.ALA", "ALA.OND", "DSM.ALD", "ALD.OND", "DSM.DRA", "DRA.OND", "DSM.DRD", "DRD.OND")
ncs.sub <- ncs[vars]

#Create new suicide attempt variable
ncs.sub$attempt <- NA
ncs.sub$attempt[ncs.sub$SD2=="NO" | ncs.sub$SD15=="NO" | ncs.sub$SD6=="NO" | ncs.sub$SD19=="NO"] <-  0 
ncs.sub$attempt[ncs.sub$SD6=="YES" | ncs.sub$SD19=="YES"] <- 1


#Create a new date of last suicide attempt variable 
ncs.sub$last.attempt <- NA
ncs.sub$last.attempt <- ifelse(!is.na(ncs.sub$SD10A), ncs.sub$SD10A, ncs.sub$SD23A)


#Subset data and rename variables
vars <- c("attempt", "last.attempt", "DSM.PDS", "PD.OND", "DSM.AGO", "AGO.OND", "DSM.SP", "SP.OND", "DSM.SO", "SO.OND", "DSM.PTS", "PTS.OND", "DSM.GAD", "GAD.OND", "DSM.MDDH", "MDDH.OND", "DSM.ALA", "ALA.OND", "DSM.ALD", "ALD.OND", "DSM.DRA", "DRA.OND", "DSM.DRD", "DRD.OND")
ncs.sub <- ncs.sub[vars]
colnames(ncs.sub) <- c("attempt", "last.attempt", "panic", "panic.onset", "agoraphobia", "agoraphobia.onset", "specific.phobia", "specific.phobia.onset", "social.phobia", "social.phobia.onset", "ptsd", "ptsd.onset", "gad", "gad.onset", "mdd", "mdd.onset", "alcohol.abuse", "alcohol.abuse.onset", "alcohol.dep", "alcohol.dep.onset", "drug.abuse", "drug.abuse.onset", "drug.dep", "drug.dep.onset") 


#Recode variables
ncs.sub1 <- ncs.sub %>% 
  mutate_at(c("panic", "agoraphobia", "specific.phobia", "social.phobia", "ptsd", "gad", "mdd", "alcohol.abuse", "alcohol.dep", "drug.abuse", "drug.dep"), funs(recode(., `ENDORSED`=1, `NOT ENDORSED`=0, .default = NaN)))

ncs.sub1$attempt <- factor(ncs.sub1$attempt)


#Factor variables
cols <- c("attempt", "panic", "agoraphobia", "specific.phobia", "social.phobia", "ptsd", "gad", "mdd", "alcohol.abuse", "alcohol.dep", "drug.abuse", "drug.dep")
ncs.sub1[cols] <- lapply(ncs.sub1[cols], factor)


#Numeric variables
ncs.sub1$panic.onset <- as.character(ncs.sub1$panic.onset)
ncs.sub1$panic.onset[ncs.sub1$panic.onset=="4 OR LESS"] <- "4"

ncs.sub1$agoraphobia.onset <- as.character(ncs.sub1$agoraphobia.onset)
ncs.sub1$agoraphobia.onset[ncs.sub1$agoraphobia.onset=="4 OR LESS"] <- "4"

ncs.sub1$specific.phobia.onset <- as.character(ncs.sub1$specific.phobia.onset)
ncs.sub1$specific.phobia.onset[ncs.sub1$specific.phobia.onset=="4 OR LESS"] <- "4"

ncs.sub1$social.phobia.onset <- as.character(ncs.sub1$social.phobia.onset)
ncs.sub1$social.phobia.onset[ncs.sub1$social.phobia.onset=="4 OR LESS"] <- "4"

ncs.sub1$ptsd.onset <- as.character(ncs.sub1$ptsd.onset)
ncs.sub1$ptsd.onset[ncs.sub1$ptsd.onset=="4 OR LESS"] <- "4"

ncs.sub1$gad.onset <- as.character(ncs.sub1$gad.onset)
ncs.sub1$gad.onset[ncs.sub1$gad.onset=="4 OR LESS"] <- "4"

ncs.sub1$mdd.onset <- as.character(ncs.sub1$mdd.onset)
ncs.sub1$mdd.onset[ncs.sub1$mdd.onset=="4 OR LESS"] <- "4"

ncs.sub1$alcohol.abuse.onset <- as.character(ncs.sub1$alcohol.abuse.onset)
ncs.sub1$alcohol.abuse.onset[ncs.sub1$alcohol.abuse.onset=="4 OR LESS"] <- "4"

ncs.sub1$alcohol.dep.onset <- as.character(ncs.sub1$alcohol.dep.onset)
ncs.sub1$alcohol.dep.onset[ncs.sub1$alcohol.dep.onset=="4 OR LESS"] <- "4"

ncs.sub1$drug.abuse.onset <- as.character(ncs.sub1$drug.abuse.onset)
ncs.sub1$drug.abuse.onset[ncs.sub1$drug.abuse.onset=="4 OR LESS"] <- "4"

ncs.sub1$drug.dep.onset <- as.character(ncs.sub1$drug.dep.onset)
ncs.sub1$drug.dep.onset[ncs.sub1$drug.dep.onset=="4 OR LESS"] <- "4"

cols2 <- c("last.attempt", "panic.onset", "agoraphobia.onset", "specific.phobia.onset", "social.phobia.onset", "ptsd.onset", "gad.onset", "mdd.onset", "alcohol.abuse.onset", "alcohol.dep.onset", "drug.abuse.onset", "drug.dep.onset")
ncs.sub1[cols2] <- lapply(ncs.sub1[cols2], as.numeric)


#Remove persons missing data on suicide attempts
#length(which(ncs.sub1$attempt==0 & ncs.sub1$last.attempt>0)) #Check to see if there's anyone who did not endorse a suicide attempt but had an age of last attempt. There were none.

ncs.sub2 <- ncs.sub1 %>% filter(is.na(attempt)==F) #people retained after removing persons missing data on suicide attempts


#Remove persons who attempted suicide but were missing data on age at last suicide attempt
#length(which(ncs.sub2$attempt==1 & is.na(ncs.sub2$last.attempt)==T))

ncs.sub3 <- ncs.sub2[!(ncs.sub2$attempt==1 & is.na(ncs.sub2$last.attempt)==T),]


#Remove persons missing data on any of the mental disorders
ncs.sub4 <- ncs.sub3 %>% drop_na(panic, agoraphobia, specific.phobia, social.phobia, ptsd, gad, mdd, alcohol.abuse, alcohol.dep, drug.abuse, drug.dep) #none were missing


#Remove persons with mental disorder diagnoses who were missing data on age of onset. There weren't any people with a mental disorder who were missing data on age of onset.
#length(which(ncs.sub4$panic==1 & is.na(ncs.sub4$panic.onset)==T))
ncs.sub5 <- ncs.sub4[!(ncs.sub4$panic==1 & is.na(ncs.sub4$panic.onset)==T),]
ncs.sub5 <- ncs.sub5[!(ncs.sub5$agoraphobia==1 & is.na(ncs.sub5$agoraphobia.onset)==T),]
ncs.sub5 <- ncs.sub5[!(ncs.sub5$specific.phobia==1 & is.na(ncs.sub5$specific.phobia.onset)==T),]
ncs.sub5 <- ncs.sub5[!(ncs.sub5$social.phobia==1 & is.na(ncs.sub5$social.phobia.onset)==T),]
ncs.sub5 <- ncs.sub5[!(ncs.sub5$ptsd==1 & is.na(ncs.sub5$ptsd.onset)==T),]
ncs.sub5 <- ncs.sub5[!(ncs.sub5$gad==1 & is.na(ncs.sub5$gad.onset)==T),]
ncs.sub5 <- ncs.sub5[!(ncs.sub5$mdd==1 & is.na(ncs.sub5$mdd.onset)==T),]
ncs.sub5 <- ncs.sub5[!(ncs.sub5$alcohol.abuse==1 & is.na(ncs.sub5$alcohol.abuse.onset)==T),]
ncs.sub5 <- ncs.sub5[!(ncs.sub5$alcohol.dep==1 & is.na(ncs.sub5$alcohol.dep.onset)==T),]
ncs.sub5 <- ncs.sub5[!(ncs.sub5$drug.abuse==1 & is.na(ncs.sub5$drug.abuse.onset)==T),]
ncs.comp <- ncs.sub5[!(ncs.sub5$drug.dep==1 & is.na(ncs.sub5$drug.dep.onset)==T),]


#Create new variables for the presence of mental disorders before or during a suicide attempt. I will use these new variables as predictors to make sure that the mental disorders preceded suicide attempts
ncs.comp$panic1 <- ifelse(ncs.comp$panic==1 & ncs.comp$panic.onset <= ncs.comp$last.attempt, 1, 0) 
ncs.comp$panic1[ncs.comp$panic==1 & ncs.comp$attempt==0] <- 1

ncs.comp$agoraphobia1 <- ifelse(ncs.comp$agoraphobia==1 & ncs.comp$agoraphobia.onset <= ncs.comp$last.attempt, 1, 0) 
ncs.comp$agoraphobia1[ncs.comp$agoraphobia==1 & ncs.comp$attempt==0] <- 1

ncs.comp$specific.phobia1 <- ifelse(ncs.comp$specific.phobia==1 & ncs.comp$specific.phobia.onset <= ncs.comp$last.attempt, 1, 0) 
ncs.comp$specific.phobia1[ncs.comp$specific.phobia==1 & ncs.comp$attempt==0] <- 1

ncs.comp$social.phobia1 <- ifelse(ncs.comp$social.phobia==1 & ncs.comp$social.phobia.onset <= ncs.comp$last.attempt, 1, 0) 
ncs.comp$social.phobia1[ncs.comp$social.phobia==1 & ncs.comp$attempt==0] <- 1

ncs.comp$ptsd1 <- ifelse(ncs.comp$ptsd==1 & ncs.comp$ptsd.onset <= ncs.comp$last.attempt, 1, 0) 
ncs.comp$ptsd1[ncs.comp$ptsd==1 & ncs.comp$attempt==0] <- 1

ncs.comp$gad1 <- ifelse(ncs.comp$gad==1 & ncs.comp$gad.onset <= ncs.comp$last.attempt, 1, 0) 
ncs.comp$gad1[ncs.comp$gad==1 & ncs.comp$attempt==0] <- 1

ncs.comp$mdd1 <- ifelse(ncs.comp$mdd==1 & ncs.comp$mdd.onset <= ncs.comp$last.attempt, 1, 0) 
ncs.comp$mdd1[ncs.comp$mdd==1 & ncs.comp$attempt==0] <- 1

ncs.comp$alcohol.abuse1 <- ifelse(ncs.comp$alcohol.abuse==1 & ncs.comp$alcohol.abuse.onset <= ncs.comp$last.attempt, 1, 0) 
ncs.comp$alcohol.abuse1[ncs.comp$alcohol.abuse==1 & ncs.comp$attempt==0] <- 1

ncs.comp$alcohol.dep1 <- ifelse(ncs.comp$alcohol.dep==1 & ncs.comp$alcohol.dep.onset <= ncs.comp$last.attempt, 1, 0) 
ncs.comp$alcohol.dep1[ncs.comp$alcohol.dep==1 & ncs.comp$attempt==0] <- 1

ncs.comp$drug.abuse1 <- ifelse(ncs.comp$drug.abuse==1 & ncs.comp$drug.abuse.onset <= ncs.comp$last.attempt, 1, 0) 
ncs.comp$drug.abuse1[ncs.comp$drug.abuse==1 & ncs.comp$attempt==0] <- 1

ncs.comp$drug.dep1 <- ifelse(ncs.comp$drug.dep==1 & ncs.comp$drug.dep.onset <= ncs.comp$last.attempt, 1, 0) 
ncs.comp$drug.dep1[ncs.comp$drug.dep==1 & ncs.comp$attempt==0] <- 1

#View data to see if I am doing what I think I'm doing:
#ncs.comp %>% filter(alcohol.abuse==1) %>% select(alcohol.abuse, alcohol.abuse.onset, alcohol.abuse1, attempt, last.attempt) 


#Subset data for only the variables I need for analysis
vars <- c("panic1", "agoraphobia1", "specific.phobia1", "social.phobia1", "ptsd1", "gad1", "mdd1", "alcohol.abuse1", "alcohol.dep1", "drug.abuse1", "drug.dep1", "attempt")

ncs.final <- ncs.comp[vars]

cols <- names(ncs.final)
ncs.final[cols] <- lapply(ncs.final[cols], factor) 



####################################################################################################################################
### Bias parameters
####################################################################################################################################
#The Kessler et al. JAMA paper does not provide cell counts, so I have to back calculate to obtain true positives, false negatives, false positives, and true negatives
#Source: Lifetime Prevalence and Age-of-Onset Distributions of DSM-IV Disorders in the National Comorbidity Survey Replication. Table 1.

n=325 #number of people in the clinical calibration sample

panic.se = 0.458
panic.sp = 0.985
panic.ppv=0.484
panic.prev <-  (panic.ppv*(1-panic.sp)) / (panic.se - (panic.ppv*panic.se) + (panic.ppv*(1-panic.sp))) #Prevalence
panic.tp <- n*panic.prev*panic.se #true positives
panic.fn <- n*panic.prev*(1-panic.se) #false negatives
panic.fp <- n*(1-panic.prev)*(1-panic.sp) #false positives
panic.tn <- n*(1-panic.prev)*panic.sp #true negatives
#panic.tp + panic.fn + panic.fp + panic.tn #check to make sure that the cells add up to 325
#panic.se.beta <- rbeta(n=1, shape1 = panic.tp+1, shape2 = panic.fn+1)
#panic.sp.beta <- rbeta(n=1, shape1 = panic.tn+1, shape2=panic.fp+1)

agoraphobia.se=0.626
agoraphobia.sp=0.991
agoraphobia.ppv=0.620
agoraphobia.prev <-  (agoraphobia.ppv*(1-agoraphobia.sp)) / (agoraphobia.se - (agoraphobia.ppv*agoraphobia.se) + (agoraphobia.ppv*(1-agoraphobia.sp)))
agoraphobia.tp <- n*agoraphobia.prev*agoraphobia.se #true positives
agoraphobia.fn <- n*agoraphobia.prev*(1-agoraphobia.se) #false negatives
agoraphobia.fp <- n*(1-agoraphobia.prev)*(1-agoraphobia.sp) #false positives
agoraphobia.tn <- n*(1-agoraphobia.prev)*agoraphobia.sp #true negatives
#agoraphobia.tp + agoraphobia.fn + agoraphobia.fp + agoraphobia.tn #check to make sure that the cells add up to 325
#agoraphobia.se.beta <- rbeta(n=1, shape1 = agoraphobia.tp+1, shape2 = agoraphobia.fn+1)
#agoraphobia.sp.beta <- rbeta(n=1, shape1 = agoraphobia.tn+1, shape2=agoraphobia.fp+1)

specific.phobia.se=0.452
specific.phobia.sp=0.885
specific.phobia.ppv=0.439
specific.phobia.prev <-  (specific.phobia.ppv*(1-specific.phobia.sp)) / (specific.phobia.se - (specific.phobia.ppv*specific.phobia.se) + (specific.phobia.ppv*(1-specific.phobia.sp)))
specific.phobia.tp <- n*specific.phobia.prev*specific.phobia.se #true positives
specific.phobia.fn <- n*specific.phobia.prev*(1-specific.phobia.se) #false negatives
specific.phobia.fp <- n*(1-specific.phobia.prev)*(1-specific.phobia.sp) #false positives
specific.phobia.tn <- n*(1-specific.phobia.prev)*specific.phobia.sp #true negatives
#specific.phobia.tp + specific.phobia.fn + specific.phobia.fp + specific.phobia.tn #check to make sure that the cells add up to 325
#specific.phobia.se.beta <- rbeta(n=1, shape1 = specific.phobia.tp+1, shape2 = specific.phobia.fn+1)
#specific.phobia.sp.beta <- rbeta(n=1, shape1 = specific.phobia.tn+1, shape2=specific.phobia.fp+1)

social.phobia.se=0.366
social.phobia.sp=0.936
social.phobia.ppv=0.539
social.phobia.prev <-  (social.phobia.ppv*(1-social.phobia.sp)) / (social.phobia.se - (social.phobia.ppv*social.phobia.se) + (social.phobia.ppv*(1-social.phobia.sp)))
social.phobia.tp <- n*social.phobia.prev*social.phobia.se #true positives
social.phobia.fn <- n*social.phobia.prev*(1-social.phobia.se) #false negatives
social.phobia.fp <- n*(1-social.phobia.prev)*(1-social.phobia.sp) #false positives
social.phobia.tn <- n*(1-social.phobia.prev)*social.phobia.sp #true negatives
#social.phobia.tp + social.phobia.fn + social.phobia.fp + social.phobia.tn #check to make sure that the cells add up to 325
#social.phobia.se.beta <- rbeta(n=1, shape1 = social.phobia.tp+1, shape2 = social.phobia.fn+1)
#social.phobia.sp.beta <- rbeta(n=1, shape1 = social.phobia.tn+1, shape2=social.phobia.fp+1)

ptsd.se=0.383
ptsd.sp=0.991
ptsd.ppv=0.861
ptsd.prev <-  (ptsd.ppv*(1-ptsd.sp)) / (ptsd.se - (ptsd.ppv*ptsd.se) + (ptsd.ppv*(1-ptsd.sp)))
ptsd.tp <- n*ptsd.prev*ptsd.se #true positives
ptsd.fn <- n*ptsd.prev*(1-ptsd.se) #false negatives
ptsd.fp <- n*(1-ptsd.prev)*(1-ptsd.sp) #false positives
ptsd.tn <- n*(1-ptsd.prev)*ptsd.sp #true negatives
#ptsd.tp + ptsd.fn + ptsd.fp + ptsd.tn #check to make sure that the cells add up to 325
#ptsd.se.beta <- rbeta(n=1, shape1 = ptsd.tp+1, shape2 = ptsd.fn+1)
#ptsd.sp.beta <- rbeta(n=1, shape1 = ptsd.tn+1, shape2=ptsd.fp+1)

gad.se=0.544
gad.sp=0.907
gad.ppv=0.745
gad.prev <-  (gad.ppv*(1-gad.sp)) / (gad.se - (gad.ppv*gad.se) + (gad.ppv*(1-gad.sp)))
gad.tp <- n*gad.prev*gad.se #true positives
gad.fn <- n*gad.prev*(1-gad.se) #false negatives
gad.fp <- n*(1-gad.prev)*(1-gad.sp) #false positives
gad.tn <- n*(1-gad.prev)*gad.sp #true negatives
#gad.tp + gad.fn + gad.fp + gad.tn #check to make sure that the cells add up to 325
#gad.se.beta <- rbeta(n=1, shape1 = gad.tp+1, shape2 = gad.fn+1)
#gad.sp.beta <- rbeta(n=1, shape1 = gad.tn+1, shape2=gad.fp+1)

mdd.se=0.553
mdd.sp=0.937
mdd.ppv=0.737
mdd.prev <-  (mdd.ppv*(1-mdd.sp)) / (mdd.se - (mdd.ppv*mdd.se) + (mdd.ppv*(1-mdd.sp)))
mdd.tp <- n*mdd.prev*mdd.se #true positives
mdd.fn <- n*mdd.prev*(1-mdd.se) #false negatives
mdd.fp <- n*(1-mdd.prev)*(1-mdd.sp) #false positives
mdd.tn <- n*(1-mdd.prev)*mdd.sp #true negatives
#mdd.tp + mdd.fn + mdd.fp + mdd.tn #check to make sure that the cells add up to 325
#mdd.se.beta <- rbeta(n=1, shape1 = mdd.tp+1, shape2 = mdd.fn+1)
#mdd.sp.beta <- rbeta(n=1, shape1 = mdd.tn+1, shape2=mdd.fp+1)

alcohol.abuse.se=0.641
alcohol.abuse.sp=0.981
alcohol.abuse.ppv=0.881
alcohol.abuse.prev <-  (alcohol.abuse.ppv*(1-alcohol.abuse.sp)) / (alcohol.abuse.se - (alcohol.abuse.ppv*alcohol.abuse.se) + (alcohol.abuse.ppv*(1-alcohol.abuse.sp)))
alcohol.abuse.tp <- n*alcohol.abuse.prev*alcohol.abuse.se #true positives
alcohol.abuse.fn <- n*alcohol.abuse.prev*(1-alcohol.abuse.se) #false negatives
alcohol.abuse.fp <- n*(1-alcohol.abuse.prev)*(1-alcohol.abuse.sp) #false positives
alcohol.abuse.tn <- n*(1-alcohol.abuse.prev)*alcohol.abuse.sp #true negatives
#alcohol.abuse.tp + alcohol.abuse.fn + alcohol.abuse.fp + alcohol.abuse.tn #check to make sure that the cells add up to 325
#alcohol.abuse.se.beta <- rbeta(n=1, shape1 = alcohol.abuse.tp+1, shape2 = alcohol.abuse.fn+1)
#alcohol.abuse.sp.beta <- rbeta(n=1, shape1 = alcohol.abuse.tn+1, shape2=alcohol.abuse.fp+1)

alcohol.dep.se=0.431
alcohol.dep.sp=0.999
alcohol.dep.ppv=0.987
alcohol.dep.prev <-  (alcohol.dep.ppv*(1-alcohol.dep.sp)) / (alcohol.dep.se - (alcohol.dep.ppv*alcohol.dep.se) + (alcohol.dep.ppv*(1-alcohol.dep.sp)))
alcohol.dep.tp <- n*alcohol.dep.prev*alcohol.dep.se #true positives
alcohol.dep.fn <- n*alcohol.dep.prev*(1-alcohol.dep.se) #false negatives
alcohol.dep.fp <- n*(1-alcohol.dep.prev)*(1-alcohol.dep.sp) #false positives
alcohol.dep.tn <- n*(1-alcohol.dep.prev)*alcohol.dep.sp #true negatives
#alcohol.dep.tp + alcohol.dep.fn + alcohol.dep.fp + alcohol.dep.tn #check to make sure that the cells add up to 325
#alcohol.dep.se.beta <- rbeta(n=1, shape1 = alcohol.dep.tp+1, shape2 = alcohol.dep.fn+1)
#alcohol.dep.sp.beta <- rbeta(n=1, shape1 = alcohol.dep.tn+1, shape2=alcohol.dep.fp+1)

drug.abuse.se=0.537
drug.abuse.sp=0.99
drug.abuse.ppv=0.882
drug.abuse.prev <-  (drug.abuse.ppv*(1-drug.abuse.sp)) / (drug.abuse.se - (drug.abuse.ppv*drug.abuse.se) + (drug.abuse.ppv*(1-drug.abuse.sp)))
drug.abuse.tp <- n*drug.abuse.prev*drug.abuse.se #true positives
drug.abuse.fn <- n*drug.abuse.prev*(1-drug.abuse.se) #false negatives
drug.abuse.fp <- n*(1-drug.abuse.prev)*(1-drug.abuse.sp) #false positives
drug.abuse.tn <- n*(1-drug.abuse.prev)*drug.abuse.sp #true negatives
#drug.abuse.tp + drug.abuse.fn + drug.abuse.fp + drug.abuse.tn #check to make sure that the cells add up to 325
#drug.abuse.se.beta <- rbeta(n=1, shape1 = drug.abuse.tp+1, shape2 = drug.abuse.fn+1)
#drug.abuse.sp.beta <- rbeta(n=1, shape1 = drug.abuse.tn+1, shape2=drug.abuse.fp+1)

drug.dep.se=0.25
drug.dep.sp=0.996
drug.dep.ppv=0.820
drug.dep.prev <-  (drug.dep.ppv*(1-drug.dep.sp)) / (drug.dep.se - (drug.dep.ppv*drug.dep.se) + (drug.dep.ppv*(1-drug.dep.sp)))
drug.dep.tp <- n*drug.dep.prev*drug.dep.se #true positives
drug.dep.fn <- n*drug.dep.prev*(1-drug.dep.se) #false negatives
drug.dep.fp <- n*(1-drug.dep.prev)*(1-drug.dep.sp) #false positives
drug.dep.tn <- n*(1-drug.dep.prev)*drug.dep.sp #true negatives
#drug.dep.tp + drug.dep.fn + drug.dep.fp + drug.dep.tn #check to make sure that the cells add up to 325
#drug.dep.se.beta <- rbeta(n=1, shape1 = drug.dep.tp+1, shape2 = drug.dep.fn+1)
#drug.dep.sp.beta <- rbeta(n=1, shape1 = drug.dep.tn+1, shape2=drug.dep.fp+1)


####################################################################################################################################
### Bias adjustment for exposure misclassification
####################################################################################################################################
ncsr.correction.func <- function(i){
  repeat {
  #Draw from beta distribution for bias parameters (PBA) 
  
  panic.se.beta <- rbeta(n=1, shape1 = panic.tp+1, shape2 = panic.fn+1)
  panic.sp.beta <- rbeta(n=1, shape1 = panic.tn+1, shape2=panic.fp+1)
  
  agoraphobia.se.beta <- rbeta(n=1, shape1 = agoraphobia.tp+1, shape2 = agoraphobia.fn+1)
  agoraphobia.sp.beta <- rbeta(n=1, shape1 = agoraphobia.tn+1, shape2=agoraphobia.fp+1)
  
  specific.phobia.se.beta <- rbeta(n=1, shape1 = specific.phobia.tp+1, shape2 = specific.phobia.fn+1)
  specific.phobia.sp.beta <- rbeta(n=1, shape1 = specific.phobia.tn+1, shape2=specific.phobia.fp+1)
  
  social.phobia.se.beta <- rbeta(n=1, shape1 = social.phobia.tp+1, shape2 = social.phobia.fn+1)
  social.phobia.sp.beta <- rbeta(n=1, shape1 = social.phobia.tn+1, shape2=social.phobia.fp+1)
  
  ptsd.se.beta <- rbeta(n=1, shape1 = ptsd.tp+1, shape2 = ptsd.fn+1)
  ptsd.sp.beta <- rbeta(n=1, shape1 = ptsd.tn+1, shape2=ptsd.fp+1)
  
  gad.se.beta <- rbeta(n=1, shape1 = gad.tp+1, shape2 = gad.fn+1)
  gad.sp.beta <- rbeta(n=1, shape1 = gad.tn+1, shape2=gad.fp+1)
  
  mdd.se.beta <- rbeta(n=1, shape1 = mdd.tp+1, shape2 = mdd.fn+1)
  mdd.sp.beta <- rbeta(n=1, shape1 = mdd.tn+1, shape2=mdd.fp+1)
  
  alcohol.abuse.se.beta <- rbeta(n=1, shape1 = alcohol.abuse.tp+1, shape2 = alcohol.abuse.fn+1)
  alcohol.abuse.sp.beta <- rbeta(n=1, shape1 = alcohol.abuse.tn+1, shape2=alcohol.abuse.fp+1)
  
  alcohol.dep.se.beta <- rbeta(n=1, shape1 = alcohol.dep.tp+1, shape2 = alcohol.dep.fn+1)
  alcohol.dep.sp.beta <- rbeta(n=1, shape1 = alcohol.dep.tn+1, shape2=alcohol.dep.fp+1)
  
  drug.abuse.se.beta <- rbeta(n=1, shape1 = drug.abuse.tp+1, shape2 = drug.abuse.fn+1)
  drug.abuse.sp.beta <- rbeta(n=1, shape1 = drug.abuse.tn+1, shape2=drug.abuse.fp+1)
  
  drug.dep.se.beta <- rbeta(n=1, shape1 = drug.dep.tp+1, shape2 = drug.dep.fn+1)
  drug.dep.sp.beta <- rbeta(n=1, shape1 = drug.dep.tn+1, shape2=drug.dep.fp+1)
  
  #Get the counts for each of the cells in the 2x2 table for the misclassified data set for each predictor
  a.panic1 <- length(which(ncs.final$attempt==1 & ncs.final$panic1==1))
  b.panic1 <- length(which(ncs.final$attempt==1 & ncs.final$panic1==0))
  c.panic1 <- length(which(ncs.final$attempt==0 & ncs.final$panic1==1))
  d.panic1 <- length(which(ncs.final$attempt==0 & ncs.final$panic1==0))
  panic1.dis <- sum(a.panic1, b.panic1)
  panic1.undis <- sum(c.panic1, d.panic1)
  
  a.agoraphobia1 <- length(which(ncs.final$attempt==1 & ncs.final$agoraphobia1==1))
  b.agoraphobia1 <- length(which(ncs.final$attempt==1 & ncs.final$agoraphobia1==0))
  c.agoraphobia1 <- length(which(ncs.final$attempt==0 & ncs.final$agoraphobia1==1))
  d.agoraphobia1 <- length(which(ncs.final$attempt==0 & ncs.final$agoraphobia1==0))
  agoraphobia1.dis <- sum(a.agoraphobia1, b.agoraphobia1)
  agoraphobia1.undis <- sum(c.agoraphobia1, d.agoraphobia1)
  
  a.specific.phobia1 <- length(which(ncs.final$attempt==1 & ncs.final$specific.phobia1==1))
  b.specific.phobia1 <- length(which(ncs.final$attempt==1 & ncs.final$specific.phobia1==0))
  c.specific.phobia1 <- length(which(ncs.final$attempt==0 & ncs.final$specific.phobia1==1))
  d.specific.phobia1 <- length(which(ncs.final$attempt==0 & ncs.final$specific.phobia1==0))
  specific.phobia1.dis <- sum(a.specific.phobia1, b.specific.phobia1)
  specific.phobia1.undis <- sum(c.specific.phobia1, d.specific.phobia1)
  
  a.social.phobia1 <- length(which(ncs.final$attempt==1 & ncs.final$social.phobia1==1))
  b.social.phobia1 <- length(which(ncs.final$attempt==1 & ncs.final$social.phobia1==0))
  c.social.phobia1 <- length(which(ncs.final$attempt==0 & ncs.final$social.phobia1==1))
  d.social.phobia1 <- length(which(ncs.final$attempt==0 & ncs.final$social.phobia1==0))
  social.phobia1.dis <- sum(a.social.phobia1, b.social.phobia1)
  social.phobia1.undis <- sum(c.social.phobia1, d.social.phobia1)
  
  a.ptsd1 <- length(which(ncs.final$attempt==1 & ncs.final$ptsd1==1))
  b.ptsd1 <- length(which(ncs.final$attempt==1 & ncs.final$ptsd1==0))
  c.ptsd1 <- length(which(ncs.final$attempt==0 & ncs.final$ptsd1==1))
  d.ptsd1 <- length(which(ncs.final$attempt==0 & ncs.final$ptsd1==0))
  ptsd1.dis <- sum(a.ptsd1, b.ptsd1)
  ptsd1.undis <- sum(c.ptsd1, d.ptsd1)
  
  a.gad1 <- length(which(ncs.final$attempt==1 & ncs.final$gad1==1))
  b.gad1 <- length(which(ncs.final$attempt==1 & ncs.final$gad1==0))
  c.gad1 <- length(which(ncs.final$attempt==0 & ncs.final$gad1==1))
  d.gad1 <- length(which(ncs.final$attempt==0 & ncs.final$gad1==0))
  gad1.dis <- sum(a.gad1, b.gad1)
  gad1.undis <- sum(c.gad1, d.gad1)
  
  a.mdd1 <- length(which(ncs.final$attempt==1 & ncs.final$mdd1==1))
  b.mdd1 <- length(which(ncs.final$attempt==1 & ncs.final$mdd1==0))
  c.mdd1 <- length(which(ncs.final$attempt==0 & ncs.final$mdd1==1))
  d.mdd1 <- length(which(ncs.final$attempt==0 & ncs.final$mdd1==0))
  mdd1.dis <- sum(a.mdd1, b.mdd1)
  mdd1.undis <- sum(c.mdd1, d.mdd1)
  
  a.alcohol.abuse1 <- length(which(ncs.final$attempt==1 & ncs.final$alcohol.abuse1==1))
  b.alcohol.abuse1 <- length(which(ncs.final$attempt==1 & ncs.final$alcohol.abuse1==0))
  c.alcohol.abuse1 <- length(which(ncs.final$attempt==0 & ncs.final$alcohol.abuse1==1))
  d.alcohol.abuse1 <- length(which(ncs.final$attempt==0 & ncs.final$alcohol.abuse1==0))
  alcohol.abuse1.dis <- sum(a.alcohol.abuse1, b.alcohol.abuse1)
  alcohol.abuse1.undis <- sum(c.alcohol.abuse1, d.alcohol.abuse1)
  
  a.alcohol.dep1 <- length(which(ncs.final$attempt==1 & ncs.final$alcohol.dep1==1))
  b.alcohol.dep1 <- length(which(ncs.final$attempt==1 & ncs.final$alcohol.dep1==0))
  c.alcohol.dep1 <- length(which(ncs.final$attempt==0 & ncs.final$alcohol.dep1==1))
  d.alcohol.dep1 <- length(which(ncs.final$attempt==0 & ncs.final$alcohol.dep1==0))
  alcohol.dep1.dis <- sum(a.alcohol.dep1, b.alcohol.dep1)
  alcohol.dep1.undis <- sum(c.alcohol.dep1, d.alcohol.dep1)
  
  a.drug.abuse1 <- length(which(ncs.final$attempt==1 & ncs.final$drug.abuse1==1))
  b.drug.abuse1 <- length(which(ncs.final$attempt==1 & ncs.final$drug.abuse1==0))
  c.drug.abuse1 <- length(which(ncs.final$attempt==0 & ncs.final$drug.abuse1==1))
  d.drug.abuse1 <- length(which(ncs.final$attempt==0 & ncs.final$drug.abuse1==0))
  drug.abuse1.dis <- sum(a.drug.abuse1, b.drug.abuse1)
  drug.abuse1.undis <- sum(c.drug.abuse1, d.drug.abuse1)
  
  a.drug.dep1 <- length(which(ncs.final$attempt==1 & ncs.final$drug.dep1==1))
  b.drug.dep1 <- length(which(ncs.final$attempt==1 & ncs.final$drug.dep1==0))
  c.drug.dep1 <- length(which(ncs.final$attempt==0 & ncs.final$drug.dep1==1))
  d.drug.dep1 <- length(which(ncs.final$attempt==0 & ncs.final$drug.dep1==0))
  drug.dep1.dis <- sum(a.drug.dep1, b.drug.dep1)
  drug.dep1.undis <- sum(c.drug.dep1, d.drug.dep1)
  
  #Calculate expected truth based on sensitivity and specificity
  a.panic1.expected <- (a.panic1-panic1.dis*(1-panic.sp.beta)) / (panic.se.beta-(1-panic.sp.beta))
  b.panic1.expected <- panic1.dis-a.panic1.expected
  c.panic1.expected <- (c.panic1-panic1.undis*(1-panic.sp.beta)) / (panic.se.beta-(1-panic.sp.beta))
  d.panic1.expected <- panic1.undis-c.panic1.expected
  
  a.agoraphobia1.expected <- (a.agoraphobia1-agoraphobia1.dis*(1-agoraphobia.sp.beta)) / (agoraphobia.se.beta-(1-agoraphobia.sp.beta))
  b.agoraphobia1.expected <- agoraphobia1.dis-a.agoraphobia1.expected
  c.agoraphobia1.expected <- (c.agoraphobia1-agoraphobia1.undis*(1-agoraphobia.sp.beta)) / (agoraphobia.se.beta-(1-agoraphobia.sp.beta))
  d.agoraphobia1.expected <- agoraphobia1.undis-c.agoraphobia1.expected
  
  a.specific.phobia1.expected <- (a.specific.phobia1-specific.phobia1.dis*(1-specific.phobia.sp.beta)) / (specific.phobia.se.beta-(1-specific.phobia.sp.beta))
  b.specific.phobia1.expected <- specific.phobia1.dis-a.specific.phobia1.expected
  c.specific.phobia1.expected <- (c.specific.phobia1-specific.phobia1.undis*(1-specific.phobia.sp.beta)) / (specific.phobia.se.beta-(1-specific.phobia.sp.beta))
  d.specific.phobia1.expected <- specific.phobia1.undis-c.specific.phobia1.expected
  
  a.social.phobia1.expected <- (a.social.phobia1-social.phobia1.dis*(1-social.phobia.sp.beta)) / (social.phobia.se.beta-(1-social.phobia.sp.beta))
  b.social.phobia1.expected <- social.phobia1.dis-a.social.phobia1.expected
  c.social.phobia1.expected <- (c.social.phobia1-social.phobia1.undis*(1-social.phobia.sp.beta)) / (social.phobia.se.beta-(1-social.phobia.sp.beta))
  d.social.phobia1.expected <- social.phobia1.undis-c.social.phobia1.expected
  
  a.ptsd1.expected <- (a.ptsd1-ptsd1.dis*(1-ptsd.sp.beta)) / (ptsd.se.beta-(1-ptsd.sp.beta))
  b.ptsd1.expected <- ptsd1.dis-a.ptsd1.expected
  c.ptsd1.expected <- (c.ptsd1-ptsd1.undis*(1-ptsd.sp.beta)) / (ptsd.se.beta-(1-ptsd.sp.beta))
  d.ptsd1.expected <- ptsd1.undis-c.ptsd1.expected
  
  a.gad1.expected <- (a.gad1-gad1.dis*(1-gad.sp.beta)) / (gad.se.beta-(1-gad.sp.beta))
  b.gad1.expected <- gad1.dis-a.gad1.expected
  c.gad1.expected <- (c.gad1-gad1.undis*(1-gad.sp.beta)) / (gad.se.beta-(1-gad.sp.beta))
  d.gad1.expected <- gad1.undis-c.gad1.expected
  
  a.mdd1.expected <- (a.mdd1-mdd1.dis*(1-mdd.sp.beta)) / (mdd.se.beta-(1-mdd.sp.beta))
  b.mdd1.expected <- mdd1.dis-a.mdd1.expected
  c.mdd1.expected <- (c.mdd1-mdd1.undis*(1-mdd.sp.beta)) / (mdd.se.beta-(1-mdd.sp.beta))
  d.mdd1.expected <- mdd1.undis-c.mdd1.expected
  
  a.alcohol.abuse1.expected <- (a.alcohol.abuse1-alcohol.abuse1.dis*(1-alcohol.abuse.sp.beta)) / (alcohol.abuse.se.beta-(1-alcohol.abuse.sp.beta))
  b.alcohol.abuse1.expected <- alcohol.abuse1.dis-a.alcohol.abuse1.expected
  c.alcohol.abuse1.expected <- (c.alcohol.abuse1-alcohol.abuse1.undis*(1-alcohol.abuse.sp.beta)) / (alcohol.abuse.se.beta-(1-alcohol.abuse.sp.beta))
  d.alcohol.abuse1.expected <- alcohol.abuse1.undis-c.alcohol.abuse1.expected
  
  a.alcohol.dep1.expected <- (a.alcohol.dep1-alcohol.dep1.dis*(1-alcohol.dep.sp.beta)) / (alcohol.dep.se.beta-(1-alcohol.dep.sp.beta))
  b.alcohol.dep1.expected <- alcohol.dep1.dis-a.alcohol.dep1.expected
  c.alcohol.dep1.expected <- (c.alcohol.dep1-alcohol.dep1.undis*(1-alcohol.dep.sp.beta)) / (alcohol.dep.se.beta-(1-alcohol.dep.sp.beta))
  d.alcohol.dep1.expected <- alcohol.dep1.undis-c.alcohol.dep1.expected
  
  a.drug.abuse1.expected <- (a.drug.abuse1-drug.abuse1.dis*(1-drug.abuse.sp.beta)) / (drug.abuse.se.beta-(1-drug.abuse.sp.beta))
  b.drug.abuse1.expected <- drug.abuse1.dis-a.drug.abuse1.expected
  c.drug.abuse1.expected <- (c.drug.abuse1-drug.abuse1.undis*(1-drug.abuse.sp.beta)) / (drug.abuse.se.beta-(1-drug.abuse.sp.beta))
  d.drug.abuse1.expected <- drug.abuse1.undis-c.drug.abuse1.expected
  
  a.drug.dep1.expected <- (a.drug.dep1-drug.dep1.dis*(1-drug.dep.sp.beta)) / (drug.dep.se.beta-(1-drug.dep.sp.beta))
  b.drug.dep1.expected <- drug.dep1.dis-a.drug.dep1.expected
  c.drug.dep1.expected <- (c.drug.dep1-drug.dep1.undis*(1-drug.dep.sp.beta)) / (drug.dep.se.beta-(1-drug.dep.sp.beta))
  d.drug.dep1.expected <- drug.dep1.undis-c.drug.dep1.expected
  
  #Count up number of negative values
  negative_a.panic1.expected <- ifelse(a.panic1.expected<0, 1, 0)
  negative_b.panic1.expected <- ifelse(b.panic1.expected<0, 1, 0)
  negative_c.panic1.expected <- ifelse(c.panic1.expected<0, 1, 0)
  negative_d.panic1.expected <- ifelse(d.panic1.expected<0, 1, 0)
  
  negative_a.agoraphobia1.expected <- ifelse(a.agoraphobia1.expected<0, 1, 0)
  negative_b.agoraphobia1.expected <- ifelse(b.agoraphobia1.expected<0, 1, 0)
  negative_c.agoraphobia1.expected <- ifelse(c.agoraphobia1.expected<0, 1, 0)
  negative_d.agoraphobia1.expected <- ifelse(d.agoraphobia1.expected<0, 1, 0)
  
  negative_a.specific.phobia1.expected <- ifelse(a.specific.phobia1.expected<0, 1, 0)
  negative_b.specific.phobia1.expected <- ifelse(b.specific.phobia1.expected<0, 1, 0)
  negative_c.specific.phobia1.expected <- ifelse(c.specific.phobia1.expected<0, 1, 0)
  negative_d.specific.phobia1.expected <- ifelse(d.specific.phobia1.expected<0, 1, 0)
  
  negative_a.social.phobia1.expected <- ifelse(a.social.phobia1.expected<0, 1, 0)
  negative_b.social.phobia1.expected <- ifelse(b.social.phobia1.expected<0, 1, 0)
  negative_c.social.phobia1.expected <- ifelse(c.social.phobia1.expected<0, 1, 0)
  negative_d.social.phobia1.expected <- ifelse(d.social.phobia1.expected<0, 1, 0)
  
  negative_a.ptsd1.expected <- ifelse(a.ptsd1.expected<0, 1, 0)
  negative_b.ptsd1.expected <- ifelse(b.ptsd1.expected<0, 1, 0)
  negative_c.ptsd1.expected <- ifelse(c.ptsd1.expected<0, 1, 0)
  negative_d.ptsd1.expected <- ifelse(d.ptsd1.expected<0, 1, 0)
  
  negative_a.gad1.expected <- ifelse(a.gad1.expected<0, 1, 0)
  negative_b.gad1.expected <- ifelse(b.gad1.expected<0, 1, 0)
  negative_c.gad1.expected <- ifelse(c.gad1.expected<0, 1, 0)
  negative_d.gad1.expected <- ifelse(d.gad1.expected<0, 1, 0)
  
  negative_a.mdd1.expected <- ifelse(a.mdd1.expected<0, 1, 0)
  negative_b.mdd1.expected <- ifelse(b.mdd1.expected<0, 1, 0)
  negative_c.mdd1.expected <- ifelse(c.mdd1.expected<0, 1, 0)
  negative_d.mdd1.expected <- ifelse(d.mdd1.expected<0, 1, 0)
  
  negative_a.alcohol.abuse1.expected <- ifelse(a.alcohol.abuse1.expected<0, 1, 0)
  negative_b.alcohol.abuse1.expected <- ifelse(b.alcohol.abuse1.expected<0, 1, 0)
  negative_c.alcohol.abuse1.expected <- ifelse(c.alcohol.abuse1.expected<0, 1, 0)
  negative_d.alcohol.abuse1.expected <- ifelse(d.alcohol.abuse1.expected<0, 1, 0)
  
  negative_a.alcohol.dep1.expected <- ifelse(a.alcohol.dep1.expected<0, 1, 0)
  negative_b.alcohol.dep1.expected <- ifelse(b.alcohol.dep1.expected<0, 1, 0)
  negative_c.alcohol.dep1.expected <- ifelse(c.alcohol.dep1.expected<0, 1, 0)
  negative_d.alcohol.dep1.expected <- ifelse(d.alcohol.dep1.expected<0, 1, 0)
  
  negative_a.drug.abuse1.expected <- ifelse(a.drug.abuse1.expected<0, 1, 0)
  negative_b.drug.abuse1.expected <- ifelse(b.drug.abuse1.expected<0, 1, 0)
  negative_c.drug.abuse1.expected <- ifelse(c.drug.abuse1.expected<0, 1, 0)
  negative_d.drug.abuse1.expected <- ifelse(d.drug.abuse1.expected<0, 1, 0)
  
  negative_a.drug.dep1.expected <- ifelse(a.drug.dep1.expected<0, 1, 0)
  negative_b.drug.dep1.expected <- ifelse(b.drug.dep1.expected<0, 1, 0)
  negative_c.drug.dep1.expected <- ifelse(c.drug.dep1.expected<0, 1, 0)
  negative_d.drug.dep1.expected <- ifelse(d.drug.dep1.expected<0, 1, 0)
  
  negative_total <- sum(negative_a.panic1.expected, negative_b.panic1.expected, negative_c.panic1.expected, negative_d.panic1.expected,
                        negative_a.agoraphobia1.expected, negative_b.agoraphobia1.expected, negative_c.agoraphobia1.expected, negative_d.agoraphobia1.expected,
                        negative_a.specific.phobia1.expected, negative_b.specific.phobia1.expected, negative_c.specific.phobia1.expected, negative_d.specific.phobia1.expected,
                        negative_a.social.phobia1.expected, negative_b.social.phobia1.expected, negative_c.social.phobia1.expected, negative_d.social.phobia1.expected,
                        negative_a.ptsd1.expected, negative_b.ptsd1.expected, negative_c.ptsd1.expected, negative_d.ptsd1.expected,
                        negative_a.gad1.expected, negative_b.gad1.expected, negative_c.gad1.expected, negative_d.gad1.expected,
                        negative_a.mdd1.expected, negative_b.mdd1.expected, negative_c.mdd1.expected, negative_d.mdd1.expected,
                        negative_a.alcohol.abuse1.expected, negative_b.alcohol.abuse1.expected, negative_c.alcohol.abuse1.expected, negative_d.alcohol.abuse1.expected,
                        negative_a.alcohol.dep1.expected, negative_b.alcohol.dep1.expected, negative_c.alcohol.dep1.expected, negative_d.alcohol.dep1.expected,
                        negative_a.drug.abuse1.expected, negative_b.drug.abuse1.expected, negative_c.drug.abuse1.expected, negative_d.drug.abuse1.expected,
                        negative_a.drug.dep1.expected, negative_b.drug.dep1.expected, negative_c.drug.dep1.expected, negative_d.drug.dep1.expected
  ) #merp
  if (negative_total==0) break
  }
  
  #From the expected truth, calculate false positives, false negatives, true positives, and true negatives for those with the outcome
  t.pos.d.panic1 <- panic.se.beta*a.panic1.expected
  t.neg.d.panic1 <- panic.sp.beta*b.panic1.expected
  f.neg.d.panic1 <- (1-panic.se.beta)*a.panic1.expected
  f.pos.d.panic1 <- (1-panic.sp.beta)*b.panic1.expected
  
  t.pos.d.agoraphobia1 <- agoraphobia.se.beta*a.agoraphobia1.expected
  t.neg.d.agoraphobia1 <- agoraphobia.sp.beta*b.agoraphobia1.expected
  f.neg.d.agoraphobia1 <- (1-agoraphobia.se.beta)*a.agoraphobia1.expected
  f.pos.d.agoraphobia1 <- (1-agoraphobia.sp.beta)*b.agoraphobia1.expected
  
  t.pos.d.specific.phobia1 <- specific.phobia.se.beta*a.specific.phobia1.expected
  t.neg.d.specific.phobia1 <- specific.phobia.sp.beta*b.specific.phobia1.expected
  f.neg.d.specific.phobia1 <- (1-specific.phobia.se.beta)*a.specific.phobia1.expected
  f.pos.d.specific.phobia1 <- (1-specific.phobia.sp.beta)*b.specific.phobia1.expected
  
  t.pos.d.social.phobia1 <- social.phobia.se.beta*a.social.phobia1.expected
  t.neg.d.social.phobia1 <- social.phobia.sp.beta*b.social.phobia1.expected
  f.neg.d.social.phobia1 <- (1-social.phobia.se.beta)*a.social.phobia1.expected
  f.pos.d.social.phobia1 <- (1-social.phobia.sp.beta)*b.social.phobia1.expected
  
  t.pos.d.ptsd1 <- ptsd.se.beta*a.ptsd1.expected
  t.neg.d.ptsd1 <- ptsd.sp.beta*b.ptsd1.expected
  f.neg.d.ptsd1 <- (1-ptsd.se.beta)*a.ptsd1.expected
  f.pos.d.ptsd1 <- (1-ptsd.sp.beta)*b.ptsd1.expected
  
  t.pos.d.gad1 <- gad.se.beta*a.gad1.expected
  t.neg.d.gad1 <- gad.sp.beta*b.gad1.expected
  f.neg.d.gad1 <- (1-gad.se.beta)*a.gad1.expected
  f.pos.d.gad1 <- (1-gad.sp.beta)*b.gad1.expected
  
  t.pos.d.mdd1 <- mdd.se.beta*a.mdd1.expected
  t.neg.d.mdd1 <- mdd.sp.beta*b.mdd1.expected
  f.neg.d.mdd1 <- (1-mdd.se.beta)*a.mdd1.expected
  f.pos.d.mdd1 <- (1-mdd.sp.beta)*b.mdd1.expected
  
  t.pos.d.alcohol.abuse1 <- alcohol.abuse.se.beta*a.alcohol.abuse1.expected
  t.neg.d.alcohol.abuse1 <- alcohol.abuse.sp.beta*b.alcohol.abuse1.expected
  f.neg.d.alcohol.abuse1 <- (1-alcohol.abuse.se.beta)*a.alcohol.abuse1.expected
  f.pos.d.alcohol.abuse1 <- (1-alcohol.abuse.sp.beta)*b.alcohol.abuse1.expected
  
  t.pos.d.alcohol.dep1 <- alcohol.dep.se.beta*a.alcohol.dep1.expected
  t.neg.d.alcohol.dep1 <- alcohol.dep.sp.beta*b.alcohol.dep1.expected
  f.neg.d.alcohol.dep1 <- (1-alcohol.dep.se.beta)*a.alcohol.dep1.expected
  f.pos.d.alcohol.dep1 <- (1-alcohol.dep.sp.beta)*b.alcohol.dep1.expected
  
  t.pos.d.drug.abuse1 <- drug.abuse.se.beta*a.drug.abuse1.expected
  t.neg.d.drug.abuse1 <- drug.abuse.sp.beta*b.drug.abuse1.expected
  f.neg.d.drug.abuse1 <- (1-drug.abuse.se.beta)*a.drug.abuse1.expected
  f.pos.d.drug.abuse1 <- (1-drug.abuse.sp.beta)*b.drug.abuse1.expected
  
  t.pos.d.drug.dep1 <- drug.dep.se.beta*a.drug.dep1.expected
  t.neg.d.drug.dep1 <- drug.dep.sp.beta*b.drug.dep1.expected
  f.neg.d.drug.dep1 <- (1-drug.dep.se.beta)*a.drug.dep1.expected
  f.pos.d.drug.dep1 <- (1-drug.dep.sp.beta)*b.drug.dep1.expected
  
  #From the expected truth, calculate false positives, false negatives, true positives, and true negatives for those withOUT the outcome
  t.pos.und.panic1 <- panic.se.beta*c.panic1.expected
  t.neg.und.panic1 <- panic.sp.beta*d.panic1.expected
  f.neg.und.panic1 <- (1-panic.se.beta)*c.panic1.expected
  f.pos.und.panic1 <- (1-panic.sp.beta)*d.panic1.expected
  
  t.pos.und.agoraphobia1 <- agoraphobia.se.beta*c.agoraphobia1.expected
  t.neg.und.agoraphobia1 <- agoraphobia.sp.beta*d.agoraphobia1.expected
  f.neg.und.agoraphobia1 <- (1-agoraphobia.se.beta)*c.agoraphobia1.expected
  f.pos.und.agoraphobia1 <- (1-agoraphobia.sp.beta)*d.agoraphobia1.expected
  
  t.pos.und.specific.phobia1 <- specific.phobia.se.beta*c.specific.phobia1.expected
  t.neg.und.specific.phobia1 <- specific.phobia.sp.beta*d.specific.phobia1.expected
  f.neg.und.specific.phobia1 <- (1-specific.phobia.se.beta)*c.specific.phobia1.expected
  f.pos.und.specific.phobia1 <- (1-specific.phobia.sp.beta)*d.specific.phobia1.expected
  
  t.pos.und.social.phobia1 <- social.phobia.se.beta*c.social.phobia1.expected
  t.neg.und.social.phobia1 <- social.phobia.sp.beta*d.social.phobia1.expected
  f.neg.und.social.phobia1 <- (1-social.phobia.se.beta)*c.social.phobia1.expected
  f.pos.und.social.phobia1 <- (1-social.phobia.sp.beta)*d.social.phobia1.expected
  
  t.pos.und.ptsd1 <- ptsd.se.beta*c.ptsd1.expected
  t.neg.und.ptsd1 <- ptsd.sp.beta*d.ptsd1.expected
  f.neg.und.ptsd1 <- (1-ptsd.se.beta)*c.ptsd1.expected
  f.pos.und.ptsd1 <- (1-ptsd.sp.beta)*d.ptsd1.expected
  
  t.pos.und.gad1 <- gad.se.beta*c.gad1.expected
  t.neg.und.gad1 <- gad.sp.beta*d.gad1.expected
  f.neg.und.gad1 <- (1-gad.se.beta)*c.gad1.expected
  f.pos.und.gad1 <- (1-gad.sp.beta)*d.gad1.expected
  
  t.pos.und.mdd1 <- mdd.se.beta*c.mdd1.expected
  t.neg.und.mdd1 <- mdd.sp.beta*d.mdd1.expected
  f.neg.und.mdd1 <- (1-mdd.se.beta)*c.mdd1.expected
  f.pos.und.mdd1 <- (1-mdd.sp.beta)*d.mdd1.expected
  
  t.pos.und.alcohol.abuse1 <- alcohol.abuse.se.beta*c.alcohol.abuse1.expected
  t.neg.und.alcohol.abuse1 <- alcohol.abuse.sp.beta*d.alcohol.abuse1.expected
  f.neg.und.alcohol.abuse1 <- (1-alcohol.abuse.se.beta)*c.alcohol.abuse1.expected
  f.pos.und.alcohol.abuse1 <- (1-alcohol.abuse.sp.beta)*d.alcohol.abuse1.expected
  
  t.pos.und.alcohol.dep1 <- alcohol.dep.se.beta*c.alcohol.dep1.expected
  t.neg.und.alcohol.dep1 <- alcohol.dep.sp.beta*d.alcohol.dep1.expected
  f.neg.und.alcohol.dep1 <- (1-alcohol.dep.se.beta)*c.alcohol.dep1.expected
  f.pos.und.alcohol.dep1 <- (1-alcohol.dep.sp.beta)*d.alcohol.dep1.expected
  
  t.pos.und.drug.abuse1 <- drug.abuse.se.beta*c.drug.abuse1.expected
  t.neg.und.drug.abuse1 <- drug.abuse.sp.beta*d.drug.abuse1.expected
  f.neg.und.drug.abuse1 <- (1-drug.abuse.se.beta)*c.drug.abuse1.expected
  f.pos.und.drug.abuse1 <- (1-drug.abuse.sp.beta)*d.drug.abuse1.expected
  
  t.pos.und.drug.dep1 <- drug.dep.se.beta*c.drug.dep1.expected
  t.neg.und.drug.dep1 <- drug.dep.sp.beta*d.drug.dep1.expected
  f.neg.und.drug.dep1 <- (1-drug.dep.se.beta)*c.drug.dep1.expected
  f.pos.und.drug.dep1 <- (1-drug.dep.sp.beta)*d.drug.dep1.expected
  
  #From expected truth, calculate PPV and NPV for D+ and D-
  ppv.d.panic1 <- t.pos.d.panic1 / sum(t.pos.d.panic1, f.pos.d.panic1)
  npv.d.panic1 <- t.neg.d.panic1 / sum(t.neg.d.panic1, f.neg.d.panic1)
  ppv.und.panic1 <- t.pos.und.panic1 / sum(t.pos.und.panic1, f.pos.und.panic1)
  npv.und.panic1 <- t.neg.und.panic1 / sum(t.neg.und.panic1, f.neg.und.panic1) 
  
  ppv.d.agoraphobia1 <- t.pos.d.agoraphobia1 / sum(t.pos.d.agoraphobia1, f.pos.d.agoraphobia1)
  npv.d.agoraphobia1 <- t.neg.d.agoraphobia1 / sum(t.neg.d.agoraphobia1, f.neg.d.agoraphobia1)
  ppv.und.agoraphobia1 <- t.pos.und.agoraphobia1 / sum(t.pos.und.agoraphobia1, f.pos.und.agoraphobia1)
  npv.und.agoraphobia1 <- t.neg.und.agoraphobia1 / sum(t.neg.und.agoraphobia1, f.neg.und.agoraphobia1) 
  
  ppv.d.specific.phobia1 <- t.pos.d.specific.phobia1 / sum(t.pos.d.specific.phobia1, f.pos.d.specific.phobia1)
  npv.d.specific.phobia1 <- t.neg.d.specific.phobia1 / sum(t.neg.d.specific.phobia1, f.neg.d.specific.phobia1)
  ppv.und.specific.phobia1 <- t.pos.und.specific.phobia1 / sum(t.pos.und.specific.phobia1, f.pos.und.specific.phobia1)
  npv.und.specific.phobia1 <- t.neg.und.specific.phobia1 / sum(t.neg.und.specific.phobia1, f.neg.und.specific.phobia1) 
  
  ppv.d.social.phobia1 <- t.pos.d.social.phobia1 / sum(t.pos.d.social.phobia1, f.pos.d.social.phobia1)
  npv.d.social.phobia1 <- t.neg.d.social.phobia1 / sum(t.neg.d.social.phobia1, f.neg.d.social.phobia1)
  ppv.und.social.phobia1 <- t.pos.und.social.phobia1 / sum(t.pos.und.social.phobia1, f.pos.und.social.phobia1)
  npv.und.social.phobia1 <- t.neg.und.social.phobia1 / sum(t.neg.und.social.phobia1, f.neg.und.social.phobia1) 
  
  ppv.d.ptsd1 <- t.pos.d.ptsd1 / sum(t.pos.d.ptsd1, f.pos.d.ptsd1)
  npv.d.ptsd1 <- t.neg.d.ptsd1 / sum(t.neg.d.ptsd1, f.neg.d.ptsd1)
  ppv.und.ptsd1 <- t.pos.und.ptsd1 / sum(t.pos.und.ptsd1, f.pos.und.ptsd1)
  npv.und.ptsd1 <- t.neg.und.ptsd1 / sum(t.neg.und.ptsd1, f.neg.und.ptsd1) 
  
  ppv.d.gad1 <- t.pos.d.gad1 / sum(t.pos.d.gad1, f.pos.d.gad1)
  npv.d.gad1 <- t.neg.d.gad1 / sum(t.neg.d.gad1, f.neg.d.gad1)
  ppv.und.gad1 <- t.pos.und.gad1 / sum(t.pos.und.gad1, f.pos.und.gad1)
  npv.und.gad1 <- t.neg.und.gad1 / sum(t.neg.und.gad1, f.neg.und.gad1) 
  
  ppv.d.mdd1 <- t.pos.d.mdd1 / sum(t.pos.d.mdd1, f.pos.d.mdd1)
  npv.d.mdd1 <- t.neg.d.mdd1 / sum(t.neg.d.mdd1, f.neg.d.mdd1)
  ppv.und.mdd1 <- t.pos.und.mdd1 / sum(t.pos.und.mdd1, f.pos.und.mdd1)
  npv.und.mdd1 <- t.neg.und.mdd1 / sum(t.neg.und.mdd1, f.neg.und.mdd1) 
  
  ppv.d.alcohol.abuse1 <- t.pos.d.alcohol.abuse1 / sum(t.pos.d.alcohol.abuse1, f.pos.d.alcohol.abuse1)
  npv.d.alcohol.abuse1 <- t.neg.d.alcohol.abuse1 / sum(t.neg.d.alcohol.abuse1, f.neg.d.alcohol.abuse1)
  ppv.und.alcohol.abuse1 <- t.pos.und.alcohol.abuse1 / sum(t.pos.und.alcohol.abuse1, f.pos.und.alcohol.abuse1)
  npv.und.alcohol.abuse1 <- t.neg.und.alcohol.abuse1 / sum(t.neg.und.alcohol.abuse1, f.neg.und.alcohol.abuse1) 
  
  ppv.d.alcohol.dep1 <- t.pos.d.alcohol.dep1 / sum(t.pos.d.alcohol.dep1, f.pos.d.alcohol.dep1)
  npv.d.alcohol.dep1 <- t.neg.d.alcohol.dep1 / sum(t.neg.d.alcohol.dep1, f.neg.d.alcohol.dep1)
  ppv.und.alcohol.dep1 <- t.pos.und.alcohol.dep1 / sum(t.pos.und.alcohol.dep1, f.pos.und.alcohol.dep1)
  npv.und.alcohol.dep1 <- t.neg.und.alcohol.dep1 / sum(t.neg.und.alcohol.dep1, f.neg.und.alcohol.dep1) 
  
  ppv.d.drug.abuse1 <- t.pos.d.drug.abuse1 / sum(t.pos.d.drug.abuse1, f.pos.d.drug.abuse1)
  npv.d.drug.abuse1 <- t.neg.d.drug.abuse1 / sum(t.neg.d.drug.abuse1, f.neg.d.drug.abuse1)
  ppv.und.drug.abuse1 <- t.pos.und.drug.abuse1 / sum(t.pos.und.drug.abuse1, f.pos.und.drug.abuse1)
  npv.und.drug.abuse1 <- t.neg.und.drug.abuse1 / sum(t.neg.und.drug.abuse1, f.neg.und.drug.abuse1) 

  ppv.d.drug.dep1 <- t.pos.d.drug.dep1 / sum(t.pos.d.drug.dep1, f.pos.d.drug.dep1)
  npv.d.drug.dep1 <- t.neg.d.drug.dep1 / sum(t.neg.d.drug.dep1, f.neg.d.drug.dep1)
  ppv.und.drug.dep1 <- t.pos.und.drug.dep1 / sum(t.pos.und.drug.dep1, f.pos.und.drug.dep1)
  npv.und.drug.dep1 <- t.neg.und.drug.dep1 / sum(t.neg.und.drug.dep1, f.neg.und.drug.dep1) 
  
  #Record level correction of data using PPV and NPV
  ncs.final.cor <- ncs.final 
  
  ncs.final.cor$panic1.expected <- NA
  ncs.final.cor$panic1.expected[ncs.final.cor$panic1==1 & ncs.final.cor$attempt==1] <- rbinom(length(which(ncs.final.cor$panic1==1 & ncs.final.cor$attempt==1)), 1, ppv.d.panic1)
  ncs.final.cor$panic1.expected[ncs.final.cor$panic1==0 & ncs.final.cor$attempt==1] <- rbinom(length(which(ncs.final.cor$panic1==0 & ncs.final.cor$attempt==1)), 1, 1-npv.d.panic1)
  ncs.final.cor$panic1.expected[ncs.final.cor$panic1==1 & ncs.final.cor$attempt==0] <- rbinom(length(which(ncs.final.cor$panic1==1 & ncs.final.cor$attempt==0)), 1, ppv.und.panic1)
  ncs.final.cor$panic1.expected[ncs.final.cor$panic1==0 & ncs.final.cor$attempt==0] <- rbinom(length(which(ncs.final.cor$panic1==0 & ncs.final.cor$attempt==0)), 1, 1-npv.und.panic1)
  
  ncs.final.cor$agoraphobia1.expected <- NA
  ncs.final.cor$agoraphobia1.expected[ncs.final.cor$agoraphobia1==1 & ncs.final.cor$attempt==1] <- rbinom(length(which(ncs.final.cor$agoraphobia1==1 & ncs.final.cor$attempt==1)), 1, ppv.d.agoraphobia1)
  ncs.final.cor$agoraphobia1.expected[ncs.final.cor$agoraphobia1==0 & ncs.final.cor$attempt==1] <- rbinom(length(which(ncs.final.cor$agoraphobia1==0 & ncs.final.cor$attempt==1)), 1, 1-npv.d.agoraphobia1)
  ncs.final.cor$agoraphobia1.expected[ncs.final.cor$agoraphobia1==1 & ncs.final.cor$attempt==0] <- rbinom(length(which(ncs.final.cor$agoraphobia1==1 & ncs.final.cor$attempt==0)), 1, ppv.und.agoraphobia1)
  ncs.final.cor$agoraphobia1.expected[ncs.final.cor$agoraphobia1==0 & ncs.final.cor$attempt==0] <- rbinom(length(which(ncs.final.cor$agoraphobia1==0 & ncs.final.cor$attempt==0)), 1, 1-npv.und.agoraphobia1)
  
  ncs.final.cor$specific.phobia1.expected <- NA
  ncs.final.cor$specific.phobia1.expected[ncs.final.cor$specific.phobia1==1 & ncs.final.cor$attempt==1] <- rbinom(length(which(ncs.final.cor$specific.phobia1==1 & ncs.final.cor$attempt==1)), 1, ppv.d.specific.phobia1)
  ncs.final.cor$specific.phobia1.expected[ncs.final.cor$specific.phobia1==0 & ncs.final.cor$attempt==1] <- rbinom(length(which(ncs.final.cor$specific.phobia1==0 & ncs.final.cor$attempt==1)), 1, 1-npv.d.specific.phobia1)
  ncs.final.cor$specific.phobia1.expected[ncs.final.cor$specific.phobia1==1 & ncs.final.cor$attempt==0] <- rbinom(length(which(ncs.final.cor$specific.phobia1==1 & ncs.final.cor$attempt==0)), 1, ppv.und.specific.phobia1)
  ncs.final.cor$specific.phobia1.expected[ncs.final.cor$specific.phobia1==0 & ncs.final.cor$attempt==0] <- rbinom(length(which(ncs.final.cor$specific.phobia1==0 & ncs.final.cor$attempt==0)), 1, 1-npv.und.specific.phobia1)
 
  ncs.final.cor$social.phobia1.expected <- NA
  ncs.final.cor$social.phobia1.expected[ncs.final.cor$social.phobia1==1 & ncs.final.cor$attempt==1] <- rbinom(length(which(ncs.final.cor$social.phobia1==1 & ncs.final.cor$attempt==1)), 1, ppv.d.social.phobia1)
  ncs.final.cor$social.phobia1.expected[ncs.final.cor$social.phobia1==0 & ncs.final.cor$attempt==1] <- rbinom(length(which(ncs.final.cor$social.phobia1==0 & ncs.final.cor$attempt==1)), 1, 1-npv.d.social.phobia1)
  ncs.final.cor$social.phobia1.expected[ncs.final.cor$social.phobia1==1 & ncs.final.cor$attempt==0] <- rbinom(length(which(ncs.final.cor$social.phobia1==1 & ncs.final.cor$attempt==0)), 1, ppv.und.social.phobia1)
  ncs.final.cor$social.phobia1.expected[ncs.final.cor$social.phobia1==0 & ncs.final.cor$attempt==0] <- rbinom(length(which(ncs.final.cor$social.phobia1==0 & ncs.final.cor$attempt==0)), 1, 1-npv.und.social.phobia1)
   
  ncs.final.cor$ptsd1.expected <- NA
  ncs.final.cor$ptsd1.expected[ncs.final.cor$ptsd1==1 & ncs.final.cor$attempt==1] <- rbinom(length(which(ncs.final.cor$ptsd1==1 & ncs.final.cor$attempt==1)), 1, ppv.d.ptsd1)
  ncs.final.cor$ptsd1.expected[ncs.final.cor$ptsd1==0 & ncs.final.cor$attempt==1] <- rbinom(length(which(ncs.final.cor$ptsd1==0 & ncs.final.cor$attempt==1)), 1, 1-npv.d.ptsd1)
  ncs.final.cor$ptsd1.expected[ncs.final.cor$ptsd1==1 & ncs.final.cor$attempt==0] <- rbinom(length(which(ncs.final.cor$ptsd1==1 & ncs.final.cor$attempt==0)), 1, ppv.und.ptsd1)
  ncs.final.cor$ptsd1.expected[ncs.final.cor$ptsd1==0 & ncs.final.cor$attempt==0] <- rbinom(length(which(ncs.final.cor$ptsd1==0 & ncs.final.cor$attempt==0)), 1, 1-npv.und.ptsd1)
  
  ncs.final.cor$gad1.expected <- NA
  ncs.final.cor$gad1.expected[ncs.final.cor$gad1==1 & ncs.final.cor$attempt==1] <- rbinom(length(which(ncs.final.cor$gad1==1 & ncs.final.cor$attempt==1)), 1, ppv.d.gad1)
  ncs.final.cor$gad1.expected[ncs.final.cor$gad1==0 & ncs.final.cor$attempt==1] <- rbinom(length(which(ncs.final.cor$gad1==0 & ncs.final.cor$attempt==1)), 1, 1-npv.d.gad1)
  ncs.final.cor$gad1.expected[ncs.final.cor$gad1==1 & ncs.final.cor$attempt==0] <- rbinom(length(which(ncs.final.cor$gad1==1 & ncs.final.cor$attempt==0)), 1, ppv.und.gad1)
  ncs.final.cor$gad1.expected[ncs.final.cor$gad1==0 & ncs.final.cor$attempt==0] <- rbinom(length(which(ncs.final.cor$gad1==0 & ncs.final.cor$attempt==0)), 1, 1-npv.und.gad1)
  
  ncs.final.cor$mdd1.expected <- NA
  ncs.final.cor$mdd1.expected[ncs.final.cor$mdd1==1 & ncs.final.cor$attempt==1] <- rbinom(length(which(ncs.final.cor$mdd1==1 & ncs.final.cor$attempt==1)), 1, ppv.d.mdd1)
  ncs.final.cor$mdd1.expected[ncs.final.cor$mdd1==0 & ncs.final.cor$attempt==1] <- rbinom(length(which(ncs.final.cor$mdd1==0 & ncs.final.cor$attempt==1)), 1, 1-npv.d.mdd1)
  ncs.final.cor$mdd1.expected[ncs.final.cor$mdd1==1 & ncs.final.cor$attempt==0] <- rbinom(length(which(ncs.final.cor$mdd1==1 & ncs.final.cor$attempt==0)), 1, ppv.und.mdd1)
  ncs.final.cor$mdd1.expected[ncs.final.cor$mdd1==0 & ncs.final.cor$attempt==0] <- rbinom(length(which(ncs.final.cor$mdd1==0 & ncs.final.cor$attempt==0)), 1, 1-npv.und.mdd1)
  
  ncs.final.cor$alcohol.abuse1.expected <- NA
  ncs.final.cor$alcohol.abuse1.expected[ncs.final.cor$alcohol.abuse1==1 & ncs.final.cor$attempt==1] <- rbinom(length(which(ncs.final.cor$alcohol.abuse1==1 & ncs.final.cor$attempt==1)), 1, ppv.d.alcohol.abuse1)
  ncs.final.cor$alcohol.abuse1.expected[ncs.final.cor$alcohol.abuse1==0 & ncs.final.cor$attempt==1] <- rbinom(length(which(ncs.final.cor$alcohol.abuse1==0 & ncs.final.cor$attempt==1)), 1, 1-npv.d.alcohol.abuse1)
  ncs.final.cor$alcohol.abuse1.expected[ncs.final.cor$alcohol.abuse1==1 & ncs.final.cor$attempt==0] <- rbinom(length(which(ncs.final.cor$alcohol.abuse1==1 & ncs.final.cor$attempt==0)), 1, ppv.und.alcohol.abuse1)
  ncs.final.cor$alcohol.abuse1.expected[ncs.final.cor$alcohol.abuse1==0 & ncs.final.cor$attempt==0] <- rbinom(length(which(ncs.final.cor$alcohol.abuse1==0 & ncs.final.cor$attempt==0)), 1, 1-npv.und.alcohol.abuse1)
  
  ncs.final.cor$alcohol.dep1.expected <- NA
  ncs.final.cor$alcohol.dep1.expected[ncs.final.cor$alcohol.dep1==1 & ncs.final.cor$attempt==1] <- rbinom(length(which(ncs.final.cor$alcohol.dep1==1 & ncs.final.cor$attempt==1)), 1, ppv.d.alcohol.dep1)
  ncs.final.cor$alcohol.dep1.expected[ncs.final.cor$alcohol.dep1==0 & ncs.final.cor$attempt==1] <- rbinom(length(which(ncs.final.cor$alcohol.dep1==0 & ncs.final.cor$attempt==1)), 1, 1-npv.d.alcohol.dep1)
  ncs.final.cor$alcohol.dep1.expected[ncs.final.cor$alcohol.dep1==1 & ncs.final.cor$attempt==0] <- rbinom(length(which(ncs.final.cor$alcohol.dep1==1 & ncs.final.cor$attempt==0)), 1, ppv.und.alcohol.dep1)
  ncs.final.cor$alcohol.dep1.expected[ncs.final.cor$alcohol.dep1==0 & ncs.final.cor$attempt==0] <- rbinom(length(which(ncs.final.cor$alcohol.dep1==0 & ncs.final.cor$attempt==0)), 1, 1-npv.und.alcohol.dep1)
  
  ncs.final.cor$drug.abuse1.expected <- NA
  ncs.final.cor$drug.abuse1.expected[ncs.final.cor$drug.abuse1==1 & ncs.final.cor$attempt==1] <- rbinom(length(which(ncs.final.cor$drug.abuse1==1 & ncs.final.cor$attempt==1)), 1, ppv.d.drug.abuse1)
  ncs.final.cor$drug.abuse1.expected[ncs.final.cor$drug.abuse1==0 & ncs.final.cor$attempt==1] <- rbinom(length(which(ncs.final.cor$drug.abuse1==0 & ncs.final.cor$attempt==1)), 1, 1-npv.d.drug.abuse1)
  ncs.final.cor$drug.abuse1.expected[ncs.final.cor$drug.abuse1==1 & ncs.final.cor$attempt==0] <- rbinom(length(which(ncs.final.cor$drug.abuse1==1 & ncs.final.cor$attempt==0)), 1, ppv.und.drug.abuse1)
  ncs.final.cor$drug.abuse1.expected[ncs.final.cor$drug.abuse1==0 & ncs.final.cor$attempt==0] <- rbinom(length(which(ncs.final.cor$drug.abuse1==0 & ncs.final.cor$attempt==0)), 1, 1-npv.und.drug.abuse1)
  
  ncs.final.cor$drug.dep1.expected <- NA
  ncs.final.cor$drug.dep1.expected[ncs.final.cor$drug.dep1==1 & ncs.final.cor$attempt==1] <- rbinom(length(which(ncs.final.cor$drug.dep1==1 & ncs.final.cor$attempt==1)), 1, ppv.d.drug.dep1)
  ncs.final.cor$drug.dep1.expected[ncs.final.cor$drug.dep1==0 & ncs.final.cor$attempt==1] <- rbinom(length(which(ncs.final.cor$drug.dep1==0 & ncs.final.cor$attempt==1)), 1, 1-npv.d.drug.dep1)
  ncs.final.cor$drug.dep1.expected[ncs.final.cor$drug.dep1==1 & ncs.final.cor$attempt==0] <- rbinom(length(which(ncs.final.cor$drug.dep1==1 & ncs.final.cor$attempt==0)), 1, ppv.und.drug.dep1)
  ncs.final.cor$drug.dep1.expected[ncs.final.cor$drug.dep1==0 & ncs.final.cor$attempt==0] <- rbinom(length(which(ncs.final.cor$drug.dep1==0 & ncs.final.cor$attempt==0)), 1, 1-npv.und.drug.dep1)
  
  vars <- c("panic1.expected", "agoraphobia1.expected", "specific.phobia1.expected", "social.phobia1.expected", "ptsd1.expected", "gad1.expected", "mdd1.expected", "alcohol.abuse1.expected", "alcohol.dep1.expected", "drug.abuse1.expected", "drug.dep1.expected", "attempt")
  
  ncs.final.cor <- ncs.final.cor[vars]
  ncs.final.cor <- data.frame(lapply(ncs.final.cor, as.factor))
  
  #Run random forest for corrected data set
  set.seed(1854)
  rf.ncs.final.cor <- randomForest(attempt ~., data=ncs.final.cor, importance=T, sampsize=c(411, 411))
  
  #Variable importance
  varimp <- importance(rf.ncs.final.cor, type=1)
  
  #AUC
  rf.ncs.final.cor.roc <- roc(ncs.final.cor$attempt, rf.ncs.final.cor$votes[,2])
  auc <- auc(rf.ncs.final.cor.roc) 
  
  #Other Performance metrics
  confusion.tab <- rf.ncs.final.cor$confusion
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
ncsr.correction.func.result <- t(sapply(1:1000, ncsr.correction.func)) #Ran 10 trials of 1000 simulations (10,000 simulations total)

write.csv(ncsr.correction.func.result, col.names=F,
          paste0("results/ncsr_predictor_correction_result", "_trial_", trial, "_",
                 format(Sys.time(), "%m%d%Y"), ".csv"),
          quote=F, row.names=F)

































