#QBA random forests
#Informative dataset #2
#Non-differential misclassification of outcome

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



################################# Non-differential misclassification of outcome ################################# 
dat2.nondiff.cor.outcome.func <- function(i, se, sp){
  
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
  dat2.nondiff.outcome1 <- dat2
  
  dat2.nondiff.outcome1$y.new <- NA
  dat2.nondiff.outcome1$y.new[dat2.nondiff.outcome1$y==1] <- rbinom(length(which(dat2.nondiff.outcome1$y==1)), 1, se)
  dat2.nondiff.outcome1$y.new[dat2.nondiff.outcome1$y==0] <- rbinom(length(which(dat2.nondiff.outcome1$y==0)), 1, 1-sp)
  
  #Subset data for misclassified variables and the other variables of interest
  vars <- c("x1", "x2", "x3", "x4", "x5", "y.new")
  dat.nondiff.outcome <- dat2.nondiff.outcome1[vars] 
  
  dat.nondiff.outcome$y.new <- as.factor(dat.nondiff.outcome$y.new)
  
  colnames(dat.nondiff.outcome) <- c("x1", "x2", "x3", "x4", "x5", "y")
  
  #Get the counts for each of the cells in the very large stratified 2x2 table for the misclassified data set for each predictor
  #X5=1 stratum
  a <- length(which(dat.nondiff.outcome$y==1 & dat.nondiff.outcome$x1==1 & dat.nondiff.outcome$x2==1 & dat.nondiff.outcome$x3==1 & dat.nondiff.outcome$x4==1 & dat.nondiff.outcome$x5==1))
  
  b <- length(which(dat.nondiff.outcome$y==1 & dat.nondiff.outcome$x1==0 & dat.nondiff.outcome$x2==1 & dat.nondiff.outcome$x3==1 & dat.nondiff.outcome$x4==1 & dat.nondiff.outcome$x5==1))
  
  c <- length(which(dat.nondiff.outcome$y==1 & dat.nondiff.outcome$x1==1 & dat.nondiff.outcome$x2==0 & dat.nondiff.outcome$x3==1 & dat.nondiff.outcome$x4==1 & dat.nondiff.outcome$x5==1))
  
  d <- length(which(dat.nondiff.outcome$y==1 & dat.nondiff.outcome$x1==0 & dat.nondiff.outcome$x2==0 & dat.nondiff.outcome$x3==1 & dat.nondiff.outcome$x4==1 & dat.nondiff.outcome$x5==1))
  
  e <- length(which(dat.nondiff.outcome$y==1 & dat.nondiff.outcome$x1==1 & dat.nondiff.outcome$x2==1 & dat.nondiff.outcome$x3==0 & dat.nondiff.outcome$x4==1 & dat.nondiff.outcome$x5==1))
  
  f <- length(which(dat.nondiff.outcome$y==1 & dat.nondiff.outcome$x1==0 & dat.nondiff.outcome$x2==1 & dat.nondiff.outcome$x3==0 & dat.nondiff.outcome$x4==1 & dat.nondiff.outcome$x5==1))
  
  g <- length(which(dat.nondiff.outcome$y==1 & dat.nondiff.outcome$x1==1 & dat.nondiff.outcome$x2==0 & dat.nondiff.outcome$x3==0 & dat.nondiff.outcome$x4==1 & dat.nondiff.outcome$x5==1))
  
  h <- length(which(dat.nondiff.outcome$y==1 & dat.nondiff.outcome$x1==0 & dat.nondiff.outcome$x2==0 & dat.nondiff.outcome$x3==0 & dat.nondiff.outcome$x4==1 & dat.nondiff.outcome$x5==1))
  
  i <- length(which(dat.nondiff.outcome$y==1 & dat.nondiff.outcome$x1==1 & dat.nondiff.outcome$x2==1 & dat.nondiff.outcome$x3==1 & dat.nondiff.outcome$x4==0 & dat.nondiff.outcome$x5==1))
  
  j <- length(which(dat.nondiff.outcome$y==1 & dat.nondiff.outcome$x1==0 & dat.nondiff.outcome$x2==1 & dat.nondiff.outcome$x3==1 & dat.nondiff.outcome$x4==0 & dat.nondiff.outcome$x5==1))
  
  k <- length(which(dat.nondiff.outcome$y==1 & dat.nondiff.outcome$x1==1 & dat.nondiff.outcome$x2==0 & dat.nondiff.outcome$x3==1 & dat.nondiff.outcome$x4==0 & dat.nondiff.outcome$x5==1))
  
  l <- length(which(dat.nondiff.outcome$y==1 & dat.nondiff.outcome$x1==0 & dat.nondiff.outcome$x2==0 & dat.nondiff.outcome$x3==1 & dat.nondiff.outcome$x4==0 & dat.nondiff.outcome$x5==1))
  
  m <- length(which(dat.nondiff.outcome$y==1 & dat.nondiff.outcome$x1==1 & dat.nondiff.outcome$x2==1 & dat.nondiff.outcome$x3==0 & dat.nondiff.outcome$x4==0 & dat.nondiff.outcome$x5==1))
  
  n <- length(which(dat.nondiff.outcome$y==1 & dat.nondiff.outcome$x1==0 & dat.nondiff.outcome$x2==1 & dat.nondiff.outcome$x3==0 & dat.nondiff.outcome$x4==0 & dat.nondiff.outcome$x5==1))
  
  o <- length(which(dat.nondiff.outcome$y==1 & dat.nondiff.outcome$x1==1 & dat.nondiff.outcome$x2==0 & dat.nondiff.outcome$x3==0 & dat.nondiff.outcome$x4==0 & dat.nondiff.outcome$x5==1))
  
  p <- length(which(dat.nondiff.outcome$y==1 & dat.nondiff.outcome$x1==0 & dat.nondiff.outcome$x2==0 & dat.nondiff.outcome$x3==0 & dat.nondiff.outcome$x4==0 & dat.nondiff.outcome$x5==1))
  
  q <- length(which(dat.nondiff.outcome$y==0 & dat.nondiff.outcome$x1==1 & dat.nondiff.outcome$x2==1 & dat.nondiff.outcome$x3==1 & dat.nondiff.outcome$x4==1 & dat.nondiff.outcome$x5==1))
  
  r <- length(which(dat.nondiff.outcome$y==0 & dat.nondiff.outcome$x1==0 & dat.nondiff.outcome$x2==1 & dat.nondiff.outcome$x3==1 & dat.nondiff.outcome$x4==1 & dat.nondiff.outcome$x5==1))
  
  s <- length(which(dat.nondiff.outcome$y==0 & dat.nondiff.outcome$x1==1 & dat.nondiff.outcome$x2==0 & dat.nondiff.outcome$x3==1 & dat.nondiff.outcome$x4==1 & dat.nondiff.outcome$x5==1))
  
  t <- length(which(dat.nondiff.outcome$y==0 & dat.nondiff.outcome$x1==0 & dat.nondiff.outcome$x2==0 & dat.nondiff.outcome$x3==1 & dat.nondiff.outcome$x4==1 & dat.nondiff.outcome$x5==1))
  
  u <- length(which(dat.nondiff.outcome$y==0 & dat.nondiff.outcome$x1==1 & dat.nondiff.outcome$x2==1 & dat.nondiff.outcome$x3==0 & dat.nondiff.outcome$x4==1 & dat.nondiff.outcome$x5==1))
  
  v <- length(which(dat.nondiff.outcome$y==0 & dat.nondiff.outcome$x1==0 & dat.nondiff.outcome$x2==1 & dat.nondiff.outcome$x3==0 & dat.nondiff.outcome$x4==1 & dat.nondiff.outcome$x5==1))
  
  w <- length(which(dat.nondiff.outcome$y==0 & dat.nondiff.outcome$x1==1 & dat.nondiff.outcome$x2==0 & dat.nondiff.outcome$x3==0 & dat.nondiff.outcome$x4==1 & dat.nondiff.outcome$x5==1))
  
  x <- length(which(dat.nondiff.outcome$y==0 & dat.nondiff.outcome$x1==0 & dat.nondiff.outcome$x2==0 & dat.nondiff.outcome$x3==0 & dat.nondiff.outcome$x4==1 & dat.nondiff.outcome$x5==1))
  
  y <- length(which(dat.nondiff.outcome$y==0 & dat.nondiff.outcome$x1==1 & dat.nondiff.outcome$x2==1 & dat.nondiff.outcome$x3==1 & dat.nondiff.outcome$x4==0 & dat.nondiff.outcome$x5==1))
  
  z1 <- length(which(dat.nondiff.outcome$y==0 & dat.nondiff.outcome$x1==0 & dat.nondiff.outcome$x2==1 & dat.nondiff.outcome$x3==1 & dat.nondiff.outcome$x4==0 & dat.nondiff.outcome$x5==1))
  
  z2 <- length(which(dat.nondiff.outcome$y==0 & dat.nondiff.outcome$x1==1 & dat.nondiff.outcome$x2==0 & dat.nondiff.outcome$x3==1 & dat.nondiff.outcome$x4==0 & dat.nondiff.outcome$x5==1))
  
  z3 <- length(which(dat.nondiff.outcome$y==0 & dat.nondiff.outcome$x1==0 & dat.nondiff.outcome$x2==0 & dat.nondiff.outcome$x3==1 & dat.nondiff.outcome$x4==0 & dat.nondiff.outcome$x5==1))
  
  z4 <- length(which(dat.nondiff.outcome$y==0 & dat.nondiff.outcome$x1==1 & dat.nondiff.outcome$x2==1 & dat.nondiff.outcome$x3==0 & dat.nondiff.outcome$x4==0 & dat.nondiff.outcome$x5==1))
  
  z5 <- length(which(dat.nondiff.outcome$y==0 & dat.nondiff.outcome$x1==0 & dat.nondiff.outcome$x2==1 & dat.nondiff.outcome$x3==0 & dat.nondiff.outcome$x4==0 & dat.nondiff.outcome$x5==1))
  
  z6 <- length(which(dat.nondiff.outcome$y==0 & dat.nondiff.outcome$x1==1 & dat.nondiff.outcome$x2==0 & dat.nondiff.outcome$x3==0 & dat.nondiff.outcome$x4==0 & dat.nondiff.outcome$x5==1))
  
  z7 <- length(which(dat.nondiff.outcome$y==0 & dat.nondiff.outcome$x1==0 & dat.nondiff.outcome$x2==0 & dat.nondiff.outcome$x3==0 & dat.nondiff.outcome$x4==0 & dat.nondiff.outcome$x5==1))
  
  
  #X5=0 stratum
  a1 <- length(which(dat.nondiff.outcome$y==1 & dat.nondiff.outcome$x1==1 & dat.nondiff.outcome$x2==1 & dat.nondiff.outcome$x3==1 & dat.nondiff.outcome$x4==1 & dat.nondiff.outcome$x5==0))
  
  b1 <- length(which(dat.nondiff.outcome$y==1 & dat.nondiff.outcome$x1==0 & dat.nondiff.outcome$x2==1 & dat.nondiff.outcome$x3==1 & dat.nondiff.outcome$x4==1 & dat.nondiff.outcome$x5==0))
  
  c1 <- length(which(dat.nondiff.outcome$y==1 & dat.nondiff.outcome$x1==1 & dat.nondiff.outcome$x2==0 & dat.nondiff.outcome$x3==1 & dat.nondiff.outcome$x4==1 & dat.nondiff.outcome$x5==0))
  
  d1 <- length(which(dat.nondiff.outcome$y==1 & dat.nondiff.outcome$x1==0 & dat.nondiff.outcome$x2==0 & dat.nondiff.outcome$x3==1 & dat.nondiff.outcome$x4==1 & dat.nondiff.outcome$x5==0))
  
  e1 <- length(which(dat.nondiff.outcome$y==1 & dat.nondiff.outcome$x1==1 & dat.nondiff.outcome$x2==1 & dat.nondiff.outcome$x3==0 & dat.nondiff.outcome$x4==1 & dat.nondiff.outcome$x5==0))
  
  f1 <- length(which(dat.nondiff.outcome$y==1 & dat.nondiff.outcome$x1==0 & dat.nondiff.outcome$x2==1 & dat.nondiff.outcome$x3==0 & dat.nondiff.outcome$x4==1 & dat.nondiff.outcome$x5==0))
  
  g1 <- length(which(dat.nondiff.outcome$y==1 & dat.nondiff.outcome$x1==1 & dat.nondiff.outcome$x2==0 & dat.nondiff.outcome$x3==0 & dat.nondiff.outcome$x4==1 & dat.nondiff.outcome$x5==0))
  
  h1 <- length(which(dat.nondiff.outcome$y==1 & dat.nondiff.outcome$x1==0 & dat.nondiff.outcome$x2==0 & dat.nondiff.outcome$x3==0 & dat.nondiff.outcome$x4==1 & dat.nondiff.outcome$x5==0))
  
  i1 <- length(which(dat.nondiff.outcome$y==1 & dat.nondiff.outcome$x1==1 & dat.nondiff.outcome$x2==1 & dat.nondiff.outcome$x3==1 & dat.nondiff.outcome$x4==0 & dat.nondiff.outcome$x5==0))
  
  j1 <- length(which(dat.nondiff.outcome$y==1 & dat.nondiff.outcome$x1==0 & dat.nondiff.outcome$x2==1 & dat.nondiff.outcome$x3==1 & dat.nondiff.outcome$x4==0 & dat.nondiff.outcome$x5==0))
  
  k1 <- length(which(dat.nondiff.outcome$y==1 & dat.nondiff.outcome$x1==1 & dat.nondiff.outcome$x2==0 & dat.nondiff.outcome$x3==1 & dat.nondiff.outcome$x4==0 & dat.nondiff.outcome$x5==0))
  
  l1 <- length(which(dat.nondiff.outcome$y==1 & dat.nondiff.outcome$x1==0 & dat.nondiff.outcome$x2==0 & dat.nondiff.outcome$x3==1 & dat.nondiff.outcome$x4==0 & dat.nondiff.outcome$x5==0))
  
  m1 <- length(which(dat.nondiff.outcome$y==1 & dat.nondiff.outcome$x1==1 & dat.nondiff.outcome$x2==1 & dat.nondiff.outcome$x3==0 & dat.nondiff.outcome$x4==0 & dat.nondiff.outcome$x5==0))
  
  n1 <- length(which(dat.nondiff.outcome$y==1 & dat.nondiff.outcome$x1==0 & dat.nondiff.outcome$x2==1 & dat.nondiff.outcome$x3==0 & dat.nondiff.outcome$x4==0 & dat.nondiff.outcome$x5==0))
  
  o1 <- length(which(dat.nondiff.outcome$y==1 & dat.nondiff.outcome$x1==1 & dat.nondiff.outcome$x2==0 & dat.nondiff.outcome$x3==0 & dat.nondiff.outcome$x4==0 & dat.nondiff.outcome$x5==0))
  
  p1 <- length(which(dat.nondiff.outcome$y==1 & dat.nondiff.outcome$x1==0 & dat.nondiff.outcome$x2==0 & dat.nondiff.outcome$x3==0 & dat.nondiff.outcome$x4==0 & dat.nondiff.outcome$x5==0))
  
  q1 <- length(which(dat.nondiff.outcome$y==0 & dat.nondiff.outcome$x1==1 & dat.nondiff.outcome$x2==1 & dat.nondiff.outcome$x3==1 & dat.nondiff.outcome$x4==1 & dat.nondiff.outcome$x5==0))
  
  r1 <- length(which(dat.nondiff.outcome$y==0 & dat.nondiff.outcome$x1==0 & dat.nondiff.outcome$x2==1 & dat.nondiff.outcome$x3==1 & dat.nondiff.outcome$x4==1 & dat.nondiff.outcome$x5==0))
  
  s1 <- length(which(dat.nondiff.outcome$y==0 & dat.nondiff.outcome$x1==1 & dat.nondiff.outcome$x2==0 & dat.nondiff.outcome$x3==1 & dat.nondiff.outcome$x4==1 & dat.nondiff.outcome$x5==0))
  
  t1 <- length(which(dat.nondiff.outcome$y==0 & dat.nondiff.outcome$x1==0 & dat.nondiff.outcome$x2==0 & dat.nondiff.outcome$x3==1 & dat.nondiff.outcome$x4==1 & dat.nondiff.outcome$x5==0))
  
  u1 <- length(which(dat.nondiff.outcome$y==0 & dat.nondiff.outcome$x1==1 & dat.nondiff.outcome$x2==1 & dat.nondiff.outcome$x3==0 & dat.nondiff.outcome$x4==1 & dat.nondiff.outcome$x5==0))
  
  v1 <- length(which(dat.nondiff.outcome$y==0 & dat.nondiff.outcome$x1==0 & dat.nondiff.outcome$x2==1 & dat.nondiff.outcome$x3==0 & dat.nondiff.outcome$x4==1 & dat.nondiff.outcome$x5==0))
  
  w1 <- length(which(dat.nondiff.outcome$y==0 & dat.nondiff.outcome$x1==1 & dat.nondiff.outcome$x2==0 & dat.nondiff.outcome$x3==0 & dat.nondiff.outcome$x4==1 & dat.nondiff.outcome$x5==0))
  
  x1 <- length(which(dat.nondiff.outcome$y==0 & dat.nondiff.outcome$x1==0 & dat.nondiff.outcome$x2==0 & dat.nondiff.outcome$x3==0 & dat.nondiff.outcome$x4==1 & dat.nondiff.outcome$x5==0))
  
  y1 <- length(which(dat.nondiff.outcome$y==0 & dat.nondiff.outcome$x1==1 & dat.nondiff.outcome$x2==1 & dat.nondiff.outcome$x3==1 & dat.nondiff.outcome$x4==0 & dat.nondiff.outcome$x5==0))
  
  z11 <- length(which(dat.nondiff.outcome$y==0 & dat.nondiff.outcome$x1==0 & dat.nondiff.outcome$x2==1 & dat.nondiff.outcome$x3==1 & dat.nondiff.outcome$x4==0 & dat.nondiff.outcome$x5==0))
  
  z12 <- length(which(dat.nondiff.outcome$y==0 & dat.nondiff.outcome$x1==1 & dat.nondiff.outcome$x2==0 & dat.nondiff.outcome$x3==1 & dat.nondiff.outcome$x4==0 & dat.nondiff.outcome$x5==0))
  
  z13 <- length(which(dat.nondiff.outcome$y==0 & dat.nondiff.outcome$x1==0 & dat.nondiff.outcome$x2==0 & dat.nondiff.outcome$x3==1 & dat.nondiff.outcome$x4==0 & dat.nondiff.outcome$x5==0))
  
  z14 <- length(which(dat.nondiff.outcome$y==0 & dat.nondiff.outcome$x1==1 & dat.nondiff.outcome$x2==1 & dat.nondiff.outcome$x3==0 & dat.nondiff.outcome$x4==0 & dat.nondiff.outcome$x5==0))
  
  z15 <- length(which(dat.nondiff.outcome$y==0 & dat.nondiff.outcome$x1==0 & dat.nondiff.outcome$x2==1 & dat.nondiff.outcome$x3==0 & dat.nondiff.outcome$x4==0 & dat.nondiff.outcome$x5==0))
  
  z16 <- length(which(dat.nondiff.outcome$y==0 & dat.nondiff.outcome$x1==1 & dat.nondiff.outcome$x2==0 & dat.nondiff.outcome$x3==0 & dat.nondiff.outcome$x4==0 & dat.nondiff.outcome$x5==0))
  
  z17 <- length(which(dat.nondiff.outcome$y==0 & dat.nondiff.outcome$x1==0 & dat.nondiff.outcome$x2==0 & dat.nondiff.outcome$x3==0 & dat.nondiff.outcome$x4==0 & dat.nondiff.outcome$x5==0))
  
  
  
  
  #Calculate expected truth based on sensitivity and specificity
  #X5=1 stratum
  a.expected <- (a-(a+q)*(1-sp)) / (se-(1-sp))
  b.expected <- (b-(b+r)*(1-sp)) / (se-(1-sp))
  q.expected <- (a+q)-a.expected
  r.expected <- (b+r)-b.expected
  
  c.expected <- (c-(c+s)*(1-sp)) / (se-(1-sp))
  d.expected <- (d-(d+t)*(1-sp)) / (se-(1-sp))
  s.expected <- (c+s)-c.expected
  t.expected <- (d+t)-d.expected
  
  e.expected <- (e-(e+u)*(1-sp)) / (se-(1-sp))
  f.expected <- (f-(f+v)*(1-sp)) / (se-(1-sp))
  u.expected <- (e+u)-e.expected
  v.expected <- (f+v)-f.expected
  
  g.expected <- (g-(g+w)*(1-sp)) / (se-(1-sp))
  h.expected <- (h-(h+x)*(1-sp)) / (se-(1-sp))
  w.expected <- (g+w)-g.expected
  x.expected <- (h+x)-h.expected
  
  i.expected <- (i-(i+y)*(1-sp)) / (se-(1-sp))
  j.expected <- (j-(j+z1)*(1-sp)) / (se-(1-sp))
  y.expected <- (i+y)-i.expected
  z1.expected <- (j+z1)-j.expected
  
  k.expected <- (k-(k+z2)*(1-sp)) / (se-(1-sp))
  l.expected <- (l-(l+z3)*(1-sp)) / (se-(1-sp))
  z2.expected <- (k+z2)-k.expected
  z3.expected <- (l+z3)-l.expected
  
  m.expected <- (m-(m+z4)*(1-sp)) / (se-(1-sp))
  n.expected <- (n-(n+z5)*(1-sp)) / (se-(1-sp))
  z4.expected <- (m+z4)-m.expected
  z5.expected <- (n+z5)-n.expected
  
  o.expected <- (o-(o+z6)*(1-sp)) / (se-(1-sp))
  p.expected <- (p-(p+z7)*(1-sp)) / (se-(1-sp))
  z6.expected <- (o+z6)-o.expected
  z7.expected <- (p+z7)-p.expected
  
  
  #X5=0 stratum
  a1.expected <- (a1-(a1+q1)*(1-sp)) / (se-(1-sp))
  b1.expected <- (b1-(b1+r1)*(1-sp)) / (se-(1-sp))
  q1.expected <- (a1+q1)-a1.expected
  r1.expected <- (b1+r1)-b1.expected
  
  c1.expected <- (c1-(c1+s1)*(1-sp)) / (se-(1-sp))
  d1.expected <- (d1-(d1+t1)*(1-sp)) / (se-(1-sp))
  s1.expected <- (c1+s1)-c1.expected
  t1.expected <- (d1+t1)-d1.expected
  
  e1.expected <- (e1-(e1+u1)*(1-sp)) / (se-(1-sp))
  f1.expected <- (f1-(f1+v1)*(1-sp)) / (se-(1-sp))
  u1.expected <- (e1+u1)-e1.expected
  v1.expected <- (f1+v1)-f1.expected
  
  g1.expected <- (g1-(g1+w1)*(1-sp)) / (se-(1-sp))
  h1.expected <- (h1-(h1+x)*(1-sp)) / (se-(1-sp))
  w1.expected <- (g1+w1)-g1.expected
  x1.expected <- (h1+x1)-h1.expected
  
  i1.expected <- (i1-(i1+y1)*(1-sp)) / (se-(1-sp))
  j1.expected <- (j1-(j1+z11)*(1-sp)) / (se-(1-sp))
  y1.expected <- (i1+y1)-i1.expected
  z11.expected <- (j1+z11)-j1.expected
  
  k1.expected <- (k1-(k1+z12)*(1-sp)) / (se-(1-sp))
  l1.expected <- (l1-(l1+z13)*(1-sp)) / (se-(1-sp))
  z12.expected <- (k1+z12)-k1.expected
  z13.expected <- (l1+z13)-l1.expected
  
  m1.expected <- (m1-(m1+z14)*(1-sp)) / (se-(1-sp))
  n1.expected <- (n1-(n1+z15)*(1-sp)) / (se-(1-sp))
  z14.expected <- (m1+z14)-m1.expected
  z15.expected <- (n1+z15)-n1.expected
  
  o1.expected <- (o1-(o1+z16)*(1-sp)) / (se-(1-sp))
  p1.expected <- (p1-(p1+z17)*(1-sp)) / (se-(1-sp))
  z16.expected <- (o1+z16)-o1.expected
  z17.expected <- (p+z17)-p.expected
  
  
  #Count up number of negative values
  #X5=1 stratum
  negative.a.expected <- ifelse(a.expected<0, 1, 0)
  negative.b.expected <- ifelse(b.expected<0, 1, 0)
  negative.c.expected <- ifelse(c.expected<0, 1, 0)
  negative.d.expected <- ifelse(d.expected<0, 1, 0)
  negative.e.expected <- ifelse(e.expected<0, 1, 0)
  negative.f.expected <- ifelse(f.expected<0, 1, 0)
  negative.g.expected <- ifelse(g.expected<0, 1, 0)
  negative.h.expected <- ifelse(h.expected<0, 1, 0)
  negative.i.expected <- ifelse(i.expected<0, 1, 0)
  negative.j.expected <- ifelse(j.expected<0, 1, 0)
  negative.k.expected <- ifelse(k.expected<0, 1, 0)
  negative.l.expected <- ifelse(l.expected<0, 1, 0)
  negative.m.expected <- ifelse(m.expected<0, 1, 0)
  negative.n.expected <- ifelse(n.expected<0, 1, 0)
  negative.o.expected <- ifelse(o.expected<0, 1, 0)
  negative.p.expected <- ifelse(p.expected<0, 1, 0)
  negative.q.expected <- ifelse(q.expected<0, 1, 0)
  negative.r.expected <- ifelse(r.expected<0, 1, 0)
  negative.s.expected <- ifelse(s.expected<0, 1, 0)
  negative.t.expected <- ifelse(t.expected<0, 1, 0)
  negative.u.expected <- ifelse(u.expected<0, 1, 0)
  negative.v.expected <- ifelse(v.expected<0, 1, 0)
  negative.w.expected <- ifelse(w.expected<0, 1, 0)
  negative.x.expected <- ifelse(x.expected<0, 1, 0)
  negative.y.expected <- ifelse(y.expected<0, 1, 0)
  negative.z1.expected <- ifelse(z1.expected<0, 1, 0)
  negative.z2.expected <- ifelse(z2.expected<0, 1, 0)
  negative.z3.expected <- ifelse(z3.expected<0, 1, 0)
  negative.z4.expected <- ifelse(z4.expected<0, 1, 0)
  negative.z5.expected <- ifelse(z5.expected<0, 1, 0)
  negative.z6.expected <- ifelse(z6.expected<0, 1, 0)
  negative.z7.expected <- ifelse(z7.expected<0, 1, 0)
  
  #X5=0 stratum
  negative.a1.expected <- ifelse(a1.expected<0, 1, 0)
  negative.b1.expected <- ifelse(b1.expected<0, 1, 0)
  negative.c1.expected <- ifelse(c1.expected<0, 1, 0)
  negative.d1.expected <- ifelse(d1.expected<0, 1, 0)
  negative.e1.expected <- ifelse(e1.expected<0, 1, 0)
  negative.f1.expected <- ifelse(f1.expected<0, 1, 0)
  negative.g1.expected <- ifelse(g1.expected<0, 1, 0)
  negative.h1.expected <- ifelse(h1.expected<0, 1, 0)
  negative.i1.expected <- ifelse(i1.expected<0, 1, 0)
  negative.j1.expected <- ifelse(j1.expected<0, 1, 0)
  negative.k1.expected <- ifelse(k1.expected<0, 1, 0)
  negative.l1.expected <- ifelse(l1.expected<0, 1, 0)
  negative.m1.expected <- ifelse(m1.expected<0, 1, 0)
  negative.n1.expected <- ifelse(n1.expected<0, 1, 0)
  negative.o1.expected <- ifelse(o1.expected<0, 1, 0)
  negative.p1.expected <- ifelse(p1.expected<0, 1, 0)
  negative.q1.expected <- ifelse(q1.expected<0, 1, 0)
  negative.r1.expected <- ifelse(r1.expected<0, 1, 0)
  negative.s1.expected <- ifelse(s1.expected<0, 1, 0)
  negative.t1.expected <- ifelse(t1.expected<0, 1, 0)
  negative.u1.expected <- ifelse(u1.expected<0, 1, 0)
  negative.v1.expected <- ifelse(v1.expected<0, 1, 0)
  negative.w1.expected <- ifelse(w1.expected<0, 1, 0)
  negative.x1.expected <- ifelse(x1.expected<0, 1, 0)
  negative.y1.expected <- ifelse(y1.expected<0, 1, 0)
  negative.z11.expected <- ifelse(z11.expected<0, 1, 0)
  negative.z12.expected <- ifelse(z12.expected<0, 1, 0)
  negative.z13.expected <- ifelse(z13.expected<0, 1, 0)
  negative.z14.expected <- ifelse(z14.expected<0, 1, 0)
  negative.z15.expected <- ifelse(z15.expected<0, 1, 0)
  negative.z16.expected <- ifelse(z16.expected<0, 1, 0)
  negative.z17.expected <- ifelse(z17.expected<0, 1, 0)
  
  
  negative_total <- sum(negative.a.expected, negative.b.expected, negative.c.expected, negative.d.expected, negative.e.expected, negative.f.expected, negative.g.expected, negative.h.expected, negative.i.expected, negative.j.expected, negative.k.expected, negative.l.expected, negative.m.expected, negative.n.expected, negative.o.expected, negative.p.expected, negative.q.expected, negative.r.expected, negative.s.expected, negative.t.expected, negative.u.expected, negative.v.expected, negative.w.expected, negative.x.expected, negative.y.expected, negative.z1.expected, negative.z2.expected, negative.z3.expected, negative.z4.expected, negative.z5.expected, negative.z6.expected, negative.z7.expected,
                        negative.a1.expected, negative.b1.expected, negative.c1.expected, negative.d1.expected, negative.e1.expected, negative.f1.expected, negative.g1.expected, negative.h1.expected, negative.i1.expected, negative.j1.expected, negative.k1.expected, negative.l1.expected, negative.m1.expected, negative.n1.expected, negative.o1.expected, negative.p1.expected, negative.q1.expected, negative.r1.expected, negative.s1.expected, negative.t1.expected, negative.u1.expected, negative.v1.expected, negative.w1.expected, negative.x1.expected, negative.y1.expected, negative.z11.expected, negative.z12.expected, negative.z13.expected, negative.z14.expected, negative.z15.expected, negative.z16.expected, negative.z17.expected) #merp
  
  if (negative_total==0) break
  }
  
  #X5=1 stratum
  #From the expected truth, calculate false positives, false negatives, true positives, and true negatives for those who were exposed
  t.pos.exp.combo1 <- se*a.expected
  t.neg.exp.combo1 <- sp*q.expected
  f.pos.exp.combo1 <- (1-sp)*q.expected
  f.neg.exp.combo1 <- (1-se)*a.expected
  
  t.pos.exp.combo2 <- se*c.expected
  t.neg.exp.combo2 <- sp*s.expected
  f.pos.exp.combo2 <- (1-sp)*s.expected
  f.neg.exp.combo2 <- (1-se)*c.expected
  
  t.pos.exp.combo3 <- se*e.expected
  t.neg.exp.combo3 <- sp*u.expected
  f.pos.exp.combo3 <- (1-sp)*u.expected
  f.neg.exp.combo3 <- (1-se)*e.expected
  
  t.pos.exp.combo4 <- se*g.expected
  t.neg.exp.combo4 <- sp*w.expected
  f.pos.exp.combo4 <- (1-sp)*w.expected
  f.neg.exp.combo4 <- (1-se)*g.expected
  
  t.pos.exp.combo5 <- se*i.expected
  t.neg.exp.combo5 <- sp*y.expected
  f.pos.exp.combo5 <- (1-sp)*y.expected
  f.neg.exp.combo5 <- (1-se)*i.expected
  
  t.pos.exp.combo6 <- se*k.expected
  t.neg.exp.combo6 <- sp*z2.expected
  f.pos.exp.combo6 <- (1-sp)*z2.expected
  f.neg.exp.combo6 <- (1-se)*k.expected
  
  t.pos.exp.combo7 <- se*m.expected
  t.neg.exp.combo7 <- sp*z4.expected
  f.pos.exp.combo7 <- (1-sp)*z4.expected
  f.neg.exp.combo7 <- (1-se)*m.expected
  
  t.pos.exp.combo8 <- se*o.expected
  t.neg.exp.combo8 <- sp*z6.expected
  f.pos.exp.combo8 <- (1-sp)*z6.expected
  f.neg.exp.combo8 <- (1-se)*o.expected
  
  #From the expected truth, calculate false positives, false negatives, true positives, and true negatives for those who were UNexposed
  t.pos.unexp.combo1 <- se*b.expected
  t.neg.unexp.combo1 <- sp*r.expected
  f.pos.unexp.combo1 <- (1-sp)*r.expected
  f.neg.unexp.combo1 <- (1-se)*b.expected
  
  t.pos.unexp.combo2 <- se*d.expected
  t.neg.unexp.combo2 <- sp*t.expected
  f.pos.unexp.combo2 <- (1-sp)*t.expected
  f.neg.unexp.combo2 <- (1-se)*d.expected
  
  t.pos.unexp.combo3 <- se*f.expected
  t.neg.unexp.combo3 <- sp*v.expected
  f.pos.unexp.combo3 <- (1-sp)*v.expected
  f.neg.unexp.combo3 <- (1-se)*f.expected
  
  t.pos.unexp.combo4 <- se*h.expected
  t.neg.unexp.combo4 <- sp*x.expected
  f.pos.unexp.combo4 <- (1-sp)*x.expected
  f.neg.unexp.combo4 <- (1-se)*h.expected
  
  t.pos.unexp.combo5 <- se*j.expected
  t.neg.unexp.combo5 <- sp*z1.expected
  f.pos.unexp.combo5 <- (1-sp)*z1.expected
  f.neg.unexp.combo5 <- (1-se)*j.expected
  
  t.pos.unexp.combo6 <- se*l.expected
  t.neg.unexp.combo6 <- sp*z3.expected
  f.pos.unexp.combo6 <- (1-sp)*z3.expected
  f.neg.unexp.combo6 <- (1-se)*l.expected
  
  t.pos.unexp.combo7 <- se*n.expected
  t.neg.unexp.combo7 <- sp*z5.expected
  f.pos.unexp.combo7 <- (1-sp)*z5.expected
  f.neg.unexp.combo7 <- (1-se)*n.expected
  
  t.pos.unexp.combo8 <- se*p.expected
  t.neg.unexp.combo8 <- sp*z7.expected
  f.pos.unexp.combo8 <- (1-sp)*z7.expected
  f.neg.unexp.combo8 <- (1-se)*p.expected
  
  #X5=0 stratum
  #From the expected truth, calculate false positives, false negatives, true positives, and true negatives for those who were exposed
  t.pos.exp.combo1a <- se*a1.expected
  t.neg.exp.combo1a <- sp*q1.expected
  f.pos.exp.combo1a <- (1-sp)*q1.expected
  f.neg.exp.combo1a <- (1-se)*a1.expected
  
  t.pos.exp.combo2a <- se*c1.expected
  t.neg.exp.combo2a <- sp*s1.expected
  f.pos.exp.combo2a <- (1-sp)*s1.expected
  f.neg.exp.combo2a <- (1-se)*c1.expected
  
  t.pos.exp.combo3a <- se*e1.expected
  t.neg.exp.combo3a <- sp*u1.expected
  f.pos.exp.combo3a <- (1-sp)*u1.expected
  f.neg.exp.combo3a <- (1-se)*e1.expected
  
  t.pos.exp.combo4a <- se*g1.expected
  t.neg.exp.combo4a <- sp*w1.expected
  f.pos.exp.combo4a <- (1-sp)*w1.expected
  f.neg.exp.combo4a <- (1-se)*g1.expected
  
  t.pos.exp.combo5a <- se*i1.expected
  t.neg.exp.combo5a <- sp*y1.expected
  f.pos.exp.combo5a <- (1-sp)*y1.expected
  f.neg.exp.combo5a <- (1-se)*i1.expected
  
  t.pos.exp.combo6a <- se*k1.expected
  t.neg.exp.combo6a <- sp*z12.expected
  f.pos.exp.combo6a <- (1-sp)*z12.expected
  f.neg.exp.combo6a <- (1-se)*k1.expected
  
  t.pos.exp.combo7a <- se*m1.expected
  t.neg.exp.combo7a <- sp*z14.expected
  f.pos.exp.combo7a <- (1-sp)*z14.expected
  f.neg.exp.combo7a <- (1-se)*m1.expected
  
  t.pos.exp.combo8a <- se*o1.expected
  t.neg.exp.combo8a <- sp*z16.expected
  f.pos.exp.combo8a <- (1-sp)*z16.expected
  f.neg.exp.combo8a <- (1-se)*o1.expected
  
  #From the expected truth, calculate false positives, false negatives, true positives, and true negatives for those who were UNexposed
  t.pos.unexp.combo1a <- se*b1.expected
  t.neg.unexp.combo1a <- sp*r1.expected
  f.pos.unexp.combo1a <- (1-sp)*r1.expected
  f.neg.unexp.combo1a <- (1-se)*b1.expected
  
  t.pos.unexp.combo2a <- se*d1.expected
  t.neg.unexp.combo2a <- sp*t1.expected
  f.pos.unexp.combo2a <- (1-sp)*t1.expected
  f.neg.unexp.combo2a <- (1-se)*d1.expected
  
  t.pos.unexp.combo3a <- se*f1.expected
  t.neg.unexp.combo3a <- sp*v1.expected
  f.pos.unexp.combo3a <- (1-sp)*v1.expected
  f.neg.unexp.combo3a <- (1-se)*f1.expected
  
  t.pos.unexp.combo4a <- se*h1.expected
  t.neg.unexp.combo4a <- sp*x1.expected
  f.pos.unexp.combo4a <- (1-sp)*x1.expected
  f.neg.unexp.combo4a <- (1-se)*h1.expected
  
  t.pos.unexp.combo5a <- se*j1.expected
  t.neg.unexp.combo5a <- sp*z11.expected
  f.pos.unexp.combo5a <- (1-sp)*z11.expected
  f.neg.unexp.combo5a <- (1-se)*j1.expected
  
  t.pos.unexp.combo6a <- se*l1.expected
  t.neg.unexp.combo6a <- sp*z13.expected
  f.pos.unexp.combo6a <- (1-sp)*z13.expected
  f.neg.unexp.combo6a <- (1-se)*l1.expected
  
  t.pos.unexp.combo7a <- se*n1.expected
  t.neg.unexp.combo7a <- sp*z15.expected
  f.pos.unexp.combo7a <- (1-sp)*z15.expected
  f.neg.unexp.combo7a <- (1-se)*n1.expected
  
  t.pos.unexp.combo8a <- se*p1.expected
  t.neg.unexp.combo8a <- sp*z17.expected
  f.pos.unexp.combo8a <- (1-sp)*z17.expected
  f.neg.unexp.combo8a <- (1-se)*p1.expected
  
  #From expected truth, calculate PPV and NPV for E+ and E-
  #X5=1 stratum
  ppv.exp.combo1 <- t.pos.exp.combo1 / sum(t.pos.exp.combo1, f.pos.exp.combo1)
  npv.exp.combo1 <- t.neg.exp.combo1 / sum(t.neg.exp.combo1, f.neg.exp.combo1)
  ppv.unexp.combo1 <- t.pos.unexp.combo1 / sum(t.pos.unexp.combo1, f.pos.unexp.combo1)
  npv.unexp.combo1 <- t.neg.unexp.combo1 / sum(t.neg.unexp.combo1, f.neg.unexp.combo1)
  
  ppv.exp.combo2 <- t.pos.exp.combo2 / sum(t.pos.exp.combo2, f.pos.exp.combo2)
  npv.exp.combo2 <- t.neg.exp.combo2 / sum(t.neg.exp.combo2, f.neg.exp.combo2)
  ppv.unexp.combo2 <- t.pos.unexp.combo2 / sum(t.pos.unexp.combo2, f.pos.unexp.combo2)
  npv.unexp.combo2 <- t.neg.unexp.combo2 / sum(t.neg.unexp.combo2, f.neg.unexp.combo2)
  
  ppv.exp.combo3 <- t.pos.exp.combo3 / sum(t.pos.exp.combo3, f.pos.exp.combo3)
  npv.exp.combo3 <- t.neg.exp.combo3 / sum(t.neg.exp.combo3, f.neg.exp.combo3)
  ppv.unexp.combo3 <- t.pos.unexp.combo3 / sum(t.pos.unexp.combo3, f.pos.unexp.combo3)
  npv.unexp.combo3 <- t.neg.unexp.combo3 / sum(t.neg.unexp.combo3, f.neg.unexp.combo3)
  
  ppv.exp.combo4 <- t.pos.exp.combo4 / sum(t.pos.exp.combo4, f.pos.exp.combo4)
  npv.exp.combo4 <- t.neg.exp.combo4 / sum(t.neg.exp.combo4, f.neg.exp.combo4)
  ppv.unexp.combo4 <- t.pos.unexp.combo4 / sum(t.pos.unexp.combo4, f.pos.unexp.combo4)
  npv.unexp.combo4 <- t.neg.unexp.combo4 / sum(t.neg.unexp.combo4, f.neg.unexp.combo4)
  
  ppv.exp.combo5 <- t.pos.exp.combo5 / sum(t.pos.exp.combo5, f.pos.exp.combo5)
  npv.exp.combo5 <- t.neg.exp.combo5 / sum(t.neg.exp.combo5, f.neg.exp.combo5)
  ppv.unexp.combo5 <- t.pos.unexp.combo5 / sum(t.pos.unexp.combo5, f.pos.unexp.combo5)
  npv.unexp.combo5 <- t.neg.unexp.combo5 / sum(t.neg.unexp.combo5, f.neg.unexp.combo5)
  
  ppv.exp.combo6 <- t.pos.exp.combo6 / sum(t.pos.exp.combo6, f.pos.exp.combo6)
  npv.exp.combo6 <- t.neg.exp.combo6 / sum(t.neg.exp.combo6, f.neg.exp.combo6)
  ppv.unexp.combo6 <- t.pos.unexp.combo6 / sum(t.pos.unexp.combo6, f.pos.unexp.combo6)
  npv.unexp.combo6 <- t.neg.unexp.combo6 / sum(t.neg.unexp.combo6, f.neg.unexp.combo6)
  
  ppv.exp.combo7 <- t.pos.exp.combo7 / sum(t.pos.exp.combo7, f.pos.exp.combo7)
  npv.exp.combo7 <- t.neg.exp.combo7 / sum(t.neg.exp.combo7, f.neg.exp.combo7)
  ppv.unexp.combo7 <- t.pos.unexp.combo7 / sum(t.pos.unexp.combo7, f.pos.unexp.combo7)
  npv.unexp.combo7 <- t.neg.unexp.combo7 / sum(t.neg.unexp.combo7, f.neg.unexp.combo7)
  
  ppv.exp.combo8 <- t.pos.exp.combo8 / sum(t.pos.exp.combo8, f.pos.exp.combo8)
  npv.exp.combo8 <- t.neg.exp.combo8 / sum(t.neg.exp.combo8, f.neg.exp.combo8)
  ppv.unexp.combo8 <- t.pos.unexp.combo8 / sum(t.pos.unexp.combo8, f.pos.unexp.combo8)
  npv.unexp.combo8 <- t.neg.unexp.combo8 / sum(t.neg.unexp.combo8, f.neg.unexp.combo8)
  
  #From expected truth, calculate PPV and NPV for E+ and E-
  #X5=0 stratum
  ppv.exp.combo1a <- t.pos.exp.combo1a / sum(t.pos.exp.combo1a, f.pos.exp.combo1a)
  npv.exp.combo1a <- t.neg.exp.combo1a / sum(t.neg.exp.combo1a, f.neg.exp.combo1a)
  ppv.unexp.combo1a <- t.pos.unexp.combo1a / sum(t.pos.unexp.combo1a, f.pos.unexp.combo1a)
  npv.unexp.combo1a <- t.neg.unexp.combo1a / sum(t.neg.unexp.combo1a, f.neg.unexp.combo1a)
  
  ppv.exp.combo2a <- t.pos.exp.combo2a / sum(t.pos.exp.combo2a, f.pos.exp.combo2a)
  npv.exp.combo2a <- t.neg.exp.combo2a / sum(t.neg.exp.combo2a, f.neg.exp.combo2a)
  ppv.unexp.combo2a <- t.pos.unexp.combo2a / sum(t.pos.unexp.combo2a, f.pos.unexp.combo2a)
  npv.unexp.combo2a <- t.neg.unexp.combo2a / sum(t.neg.unexp.combo2a, f.neg.unexp.combo2a)
  
  ppv.exp.combo3a <- t.pos.exp.combo3a / sum(t.pos.exp.combo3a, f.pos.exp.combo3a)
  npv.exp.combo3a <- t.neg.exp.combo3a / sum(t.neg.exp.combo3a, f.neg.exp.combo3a)
  ppv.unexp.combo3a <- t.pos.unexp.combo3a / sum(t.pos.unexp.combo3a, f.pos.unexp.combo3a)
  npv.unexp.combo3a <- t.neg.unexp.combo3a / sum(t.neg.unexp.combo3a, f.neg.unexp.combo3a)
  
  ppv.exp.combo4a <- t.pos.exp.combo4a / sum(t.pos.exp.combo4a, f.pos.exp.combo4a)
  npv.exp.combo4a <- t.neg.exp.combo4a / sum(t.neg.exp.combo4a, f.neg.exp.combo4a)
  ppv.unexp.combo4a <- t.pos.unexp.combo4a / sum(t.pos.unexp.combo4a, f.pos.unexp.combo4a)
  npv.unexp.combo4a <- t.neg.unexp.combo4a / sum(t.neg.unexp.combo4a, f.neg.unexp.combo4a)
  
  ppv.exp.combo5a <- t.pos.exp.combo5a / sum(t.pos.exp.combo5a, f.pos.exp.combo5a)
  npv.exp.combo5a <- t.neg.exp.combo5a / sum(t.neg.exp.combo5a, f.neg.exp.combo5a)
  ppv.unexp.combo5a <- t.pos.unexp.combo5a / sum(t.pos.unexp.combo5a, f.pos.unexp.combo5a)
  npv.unexp.combo5a <- t.neg.unexp.combo5a / sum(t.neg.unexp.combo5a, f.neg.unexp.combo5a)
  
  ppv.exp.combo6a <- t.pos.exp.combo6a / sum(t.pos.exp.combo6a, f.pos.exp.combo6a)
  npv.exp.combo6a <- t.neg.exp.combo6a / sum(t.neg.exp.combo6a, f.neg.exp.combo6a)
  ppv.unexp.combo6a <- t.pos.unexp.combo6a / sum(t.pos.unexp.combo6a, f.pos.unexp.combo6a)
  npv.unexp.combo6a <- t.neg.unexp.combo6a / sum(t.neg.unexp.combo6a, f.neg.unexp.combo6a)
  
  ppv.exp.combo7a <- t.pos.exp.combo7a / sum(t.pos.exp.combo7a, f.pos.exp.combo7a)
  npv.exp.combo7a <- t.neg.exp.combo7a / sum(t.neg.exp.combo7a, f.neg.exp.combo7a)
  ppv.unexp.combo7a <- t.pos.unexp.combo7a / sum(t.pos.unexp.combo7a, f.pos.unexp.combo7a)
  npv.unexp.combo7a <- t.neg.unexp.combo7a / sum(t.neg.unexp.combo7a, f.neg.unexp.combo7a)
  
  ppv.exp.combo8a <- t.pos.exp.combo8a / sum(t.pos.exp.combo8a, f.pos.exp.combo8a)
  npv.exp.combo8a <- t.neg.exp.combo8a / sum(t.neg.exp.combo8a, f.neg.exp.combo8a)
  ppv.unexp.combo8a <- t.pos.unexp.combo8a / sum(t.pos.unexp.combo8a, f.pos.unexp.combo8a)
  npv.unexp.combo8a <- t.neg.unexp.combo8a / sum(t.neg.unexp.combo8a, f.neg.unexp.combo8a)
  
  
  #Record level correction of data using PPV and NPV
  dat.nondiff.outcome.cor <- dat.nondiff.outcome
  
  dat.nondiff.outcome.cor$y.expected <- NA
  
  
  #X5=1 stratum
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==1] <- rbinom(length(which(dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==1)), 1, ppv.exp.combo1)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==1] <- rbinom(length(which(dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==1)), 1, ppv.unexp.combo1)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==1] <- rbinom(length(which(dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==1)), 1, 1-npv.exp.combo1)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==1] <- rbinom(length(which(dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==1)), 1, 1-npv.unexp.combo1)
  
  
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==1] <- rbinom(length(which(dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==1)), 1, ppv.exp.combo2)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==1] <- rbinom(length(which(dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==1)), 1, ppv.unexp.combo2)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==1] <- rbinom(length(which(dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==1)), 1, 1-npv.exp.combo2)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==1] <- rbinom(length(which(dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==1)), 1, 1-npv.unexp.combo2)
  
  
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==1] <- rbinom(length(which(dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==1)), 1, ppv.exp.combo3)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==1] <- rbinom(length(which(dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==1)), 1, ppv.unexp.combo3)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==1] <- rbinom(length(which(dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==1)), 1, 1-npv.exp.combo3)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==1] <- rbinom(length(which(dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==1)), 1, 1-npv.unexp.combo3)
  
  
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==1] <- rbinom(length(which(dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==1)), 1, ppv.exp.combo4)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==1] <- rbinom(length(which(dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==1)), 1, ppv.unexp.combo4)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==1] <- rbinom(length(which(dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==1)), 1, 1-npv.exp.combo4)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==1] <- rbinom(length(which(dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==1)), 1, 1-npv.unexp.combo4)
  
  
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==1] <- rbinom(length(which(dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==1)), 1, ppv.exp.combo5)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==1] <- rbinom(length(which(dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==1)), 1, ppv.unexp.combo5)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==1] <- rbinom(length(which(dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==1)), 1, 1-npv.exp.combo5)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==1] <- rbinom(length(which(dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==1)), 1, 1-npv.unexp.combo5)
  
  
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==1] <- rbinom(length(which(dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==1)), 1, ppv.exp.combo6)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==1] <- rbinom(length(which(dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==1)), 1, ppv.unexp.combo6)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==1] <- rbinom(length(which(dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==1)), 1, 1-npv.exp.combo6)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==1] <- rbinom(length(which(dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==1)), 1, 1-npv.unexp.combo6)
  
  
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==1] <- rbinom(length(which(dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==1)), 1, ppv.exp.combo7)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==1] <- rbinom(length(which(dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==1)), 1, ppv.unexp.combo7)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==1] <- rbinom(length(which(dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==1)), 1, 1-npv.exp.combo7)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==1] <- rbinom(length(which(dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==1)), 1, 1-npv.unexp.combo7)
  
  
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==1] <- rbinom(length(which(dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==1)), 1, ppv.exp.combo8)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==1] <- rbinom(length(which(dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==1)), 1, ppv.unexp.combo8)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==1] <- rbinom(length(which(dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==1)), 1, 1-npv.exp.combo8)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==1] <- rbinom(length(which(dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==1)), 1, 1-npv.unexp.combo8)
  
  
  #X5=0 stratum
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==0] <- rbinom(length(which(dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==0)), 1, ppv.exp.combo1a)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==0] <- rbinom(length(which(dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==0)), 1, ppv.unexp.combo1a)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==0] <- rbinom(length(which(dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==0)), 1, 1-npv.exp.combo1a)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==0] <- rbinom(length(which(dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==0)), 1, 1-npv.unexp.combo1a)
  
  
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==0] <- rbinom(length(which(dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==0)), 1, ppv.exp.combo2a)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==0] <- rbinom(length(which(dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==0)), 1, ppv.unexp.combo2a)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==0] <- rbinom(length(which(dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==0)), 1, 1-npv.exp.combo2a)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==0] <- rbinom(length(which(dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==0)), 1, 1-npv.unexp.combo2a)
  
  
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==0] <- rbinom(length(which(dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==0)), 1, ppv.exp.combo3a)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==0] <- rbinom(length(which(dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==0)), 1, ppv.unexp.combo3a)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==0] <- rbinom(length(which(dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==0)), 1, 1-npv.exp.combo3a)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==0] <- rbinom(length(which(dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==0)), 1, 1-npv.unexp.combo3a)
  
  
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==0] <- rbinom(length(which(dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==0)), 1, ppv.exp.combo4a)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==0] <- rbinom(length(which(dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==0)), 1, ppv.unexp.combo4a)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==0] <- rbinom(length(which(dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==0)), 1, 1-npv.exp.combo4a)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==0] <- rbinom(length(which(dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==1 & dat.nondiff.outcome.cor$x5==0)), 1, 1-npv.unexp.combo4a)
  
  
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==0] <- rbinom(length(which(dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==0)), 1, ppv.exp.combo5a)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==0] <- rbinom(length(which(dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==0)), 1, ppv.unexp.combo5a)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==0] <- rbinom(length(which(dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==0)), 1, 1-npv.exp.combo5a)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==0] <- rbinom(length(which(dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==0)), 1, 1-npv.unexp.combo5a)
  
  
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==0] <- rbinom(length(which(dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==0)), 1, ppv.exp.combo6a)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==0] <- rbinom(length(which(dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==0)), 1, ppv.unexp.combo6a)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==0] <- rbinom(length(which(dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==0)), 1, 1-npv.exp.combo6a)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==0] <- rbinom(length(which(dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==1 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==0)), 1, 1-npv.unexp.combo6a)
  
  
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==0] <- rbinom(length(which(dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==0)), 1, ppv.exp.combo7a)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==0] <- rbinom(length(which(dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==0)), 1, ppv.unexp.combo7a)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==0] <- rbinom(length(which(dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==0)), 1, 1-npv.exp.combo7a)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==0] <- rbinom(length(which(dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==1 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==0)), 1, 1-npv.unexp.combo7a)
  
  
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==0] <- rbinom(length(which(dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==0)), 1, ppv.exp.combo8a)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==0] <- rbinom(length(which(dat.nondiff.outcome.cor$y==1 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==0)), 1, ppv.unexp.combo8a)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==0] <- rbinom(length(which(dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==1 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==0)), 1, 1-npv.exp.combo8a)
  
  dat.nondiff.outcome.cor$y.expected[dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==0] <- rbinom(length(which(dat.nondiff.outcome.cor$y==0 & dat.nondiff.outcome.cor$x1==0 & dat.nondiff.outcome.cor$x2==0 & dat.nondiff.outcome.cor$x3==0 & dat.nondiff.outcome.cor$x4==0 & dat.nondiff.outcome.cor$x5==0)), 1, 1-npv.unexp.combo8a)
  
  
  
  
  vars <- c("x1", "x2", "x3", "x4", "x5", "y.expected")
  dat.nondiff.outcome.cor <- dat.nondiff.outcome.cor[vars]
  dat.nondiff.outcome.cor <- data.frame(lapply(dat.nondiff.outcome.cor, as.factor)) 
  
  #Run random forest for misclassified data set #1 with nondiff misclass. of outcome
  rf.dat2.nondiff.outcome <- randomForest(y.expected ~., data=dat.nondiff.outcome.cor, importance=T, sampsize=c(length(which(dat.nondiff.outcome.cor$y.expected==1)), length(which(dat.nondiff.outcome.cor$y.expected==1))))
  
  #Variable importance
  importance(rf.dat2.nondiff.outcome, type=1)
}

#Repeat the whole process
dat2.nondiff.misc.outcome.func.result <- t(sapply(1:10000, dat2.nondiff.cor.outcome.func, se=myse,sp=mysp)) 
  
  