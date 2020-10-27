#QBA random forests - differential misclassification of outcome for simulated data set #2
#November 26, 2019
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

################################# Informative dataset #2 #################################

dat.diff.cor.outcome.func <- function(i, se1, sp1, se0, sp0){
  
  repeat {
    
    n=10000
    x1=rbinom(n, 1, 0.10)
    x2=rbinom(n, 1, 0.35+0.1*x1)
    x3=rbinom(n, 1, 0.20+0.15*x2)
    x4=rbinom(n, 1, 0.25+0.05*x3)
    y=rbinom(n, 1, 0.05+0.05*x1+0.38*x2+0.35*x3+0.17*x4)
    
    dat <- as.data.frame(cbind(x1, x2, x3, x4, y))
    
    dat <- data.frame(lapply(dat, factor))
    
    dat.diff.outcome <- dat
    
    dat.diff.outcome$y.new <- NA
    dat.diff.outcome$y.new[dat.diff.outcome$y==1 & dat.diff.outcome$x1==1] <- rbinom(length(which(dat.diff.outcome$y==1 & dat.diff.outcome$x1==1)), 1, se1)
    dat.diff.outcome$y.new[dat.diff.outcome$y==1 & dat.diff.outcome$x1==0] <- rbinom(length(which(dat.diff.outcome$y==1 & dat.diff.outcome$x1==0)), 1, se0)
    dat.diff.outcome$y.new[dat.diff.outcome$y==0 & dat.diff.outcome$x1==1] <- rbinom(length(which(dat.diff.outcome$y==0 & dat.diff.outcome$x1==1)), 1, 1-sp1)
    dat.diff.outcome$y.new[dat.diff.outcome$y==0 & dat.diff.outcome$x1==0] <- rbinom(length(which(dat.diff.outcome$y==0 & dat.diff.outcome$x1==0)), 1, 1-sp0)
    
    
    
    #Subset data for misclassified variables and the other variables of interest
    vars <- c("x1", "x2", "x3", "x4", "y.new")
    dat.diff.outcome <- dat.diff.outcome[vars] 
    
    dat.diff.outcome$y.new <- as.factor(dat.diff.outcome$y.new)
    
    colnames(dat.diff.outcome) <- c("x1", "x2", "x3", "x4", "y")
    
    #Get the counts for each of the cells in the very large stratified 2x2 table for the misclassified data set for each predictor
   
    a <- length(which(dat.diff.outcome$y==1 & dat.diff.outcome$x1==1 & dat.diff.outcome$x2==1 & dat.diff.outcome$x3==1 & dat.diff.outcome$x4==1))
    
    b <- length(which(dat.diff.outcome$y==1 & dat.diff.outcome$x1==0 & dat.diff.outcome$x2==1 & dat.diff.outcome$x3==1 & dat.diff.outcome$x4==1))
    
    c <- length(which(dat.diff.outcome$y==1 & dat.diff.outcome$x1==1 & dat.diff.outcome$x2==0 & dat.diff.outcome$x3==1 & dat.diff.outcome$x4==1))
    
    d <- length(which(dat.diff.outcome$y==1 & dat.diff.outcome$x1==0 & dat.diff.outcome$x2==0 & dat.diff.outcome$x3==1 & dat.diff.outcome$x4==1))
    
    e <- length(which(dat.diff.outcome$y==1 & dat.diff.outcome$x1==1 & dat.diff.outcome$x2==1 & dat.diff.outcome$x3==0 & dat.diff.outcome$x4==1))
    
    f <- length(which(dat.diff.outcome$y==1 & dat.diff.outcome$x1==0 & dat.diff.outcome$x2==1 & dat.diff.outcome$x3==0 & dat.diff.outcome$x4==1))
    
    g <- length(which(dat.diff.outcome$y==1 & dat.diff.outcome$x1==1 & dat.diff.outcome$x2==0 & dat.diff.outcome$x3==0 & dat.diff.outcome$x4==1))
    
    h <- length(which(dat.diff.outcome$y==1 & dat.diff.outcome$x1==0 & dat.diff.outcome$x2==0 & dat.diff.outcome$x3==0 & dat.diff.outcome$x4==1))
    
    i <- length(which(dat.diff.outcome$y==1 & dat.diff.outcome$x1==1 & dat.diff.outcome$x2==1 & dat.diff.outcome$x3==1 & dat.diff.outcome$x4==0))
    
    j <- length(which(dat.diff.outcome$y==1 & dat.diff.outcome$x1==0 & dat.diff.outcome$x2==1 & dat.diff.outcome$x3==1 & dat.diff.outcome$x4==0))
    
    k <- length(which(dat.diff.outcome$y==1 & dat.diff.outcome$x1==1 & dat.diff.outcome$x2==0 & dat.diff.outcome$x3==1 & dat.diff.outcome$x4==0))
    
    l <- length(which(dat.diff.outcome$y==1 & dat.diff.outcome$x1==0 & dat.diff.outcome$x2==0 & dat.diff.outcome$x3==1 & dat.diff.outcome$x4==0))
    
    m <- length(which(dat.diff.outcome$y==1 & dat.diff.outcome$x1==1 & dat.diff.outcome$x2==1 & dat.diff.outcome$x3==0 & dat.diff.outcome$x4==0))
    
    n <- length(which(dat.diff.outcome$y==1 & dat.diff.outcome$x1==0 & dat.diff.outcome$x2==1 & dat.diff.outcome$x3==0 & dat.diff.outcome$x4==0))
    
    o <- length(which(dat.diff.outcome$y==1 & dat.diff.outcome$x1==1 & dat.diff.outcome$x2==0 & dat.diff.outcome$x3==0 & dat.diff.outcome$x4==0))
    
    p <- length(which(dat.diff.outcome$y==1 & dat.diff.outcome$x1==0 & dat.diff.outcome$x2==0 & dat.diff.outcome$x3==0 & dat.diff.outcome$x4==0))
    
    q <- length(which(dat.diff.outcome$y==0 & dat.diff.outcome$x1==1 & dat.diff.outcome$x2==1 & dat.diff.outcome$x3==1 & dat.diff.outcome$x4==1))
    
    r <- length(which(dat.diff.outcome$y==0 & dat.diff.outcome$x1==0 & dat.diff.outcome$x2==1 & dat.diff.outcome$x3==1 & dat.diff.outcome$x4==1))
    
    s <- length(which(dat.diff.outcome$y==0 & dat.diff.outcome$x1==1 & dat.diff.outcome$x2==0 & dat.diff.outcome$x3==1 & dat.diff.outcome$x4==1))
    
    t <- length(which(dat.diff.outcome$y==0 & dat.diff.outcome$x1==0 & dat.diff.outcome$x2==0 & dat.diff.outcome$x3==1 & dat.diff.outcome$x4==1))
    
    u <- length(which(dat.diff.outcome$y==0 & dat.diff.outcome$x1==1 & dat.diff.outcome$x2==1 & dat.diff.outcome$x3==0 & dat.diff.outcome$x4==1))
    
    v <- length(which(dat.diff.outcome$y==0 & dat.diff.outcome$x1==0 & dat.diff.outcome$x2==1 & dat.diff.outcome$x3==0 & dat.diff.outcome$x4==1))
    
    w <- length(which(dat.diff.outcome$y==0 & dat.diff.outcome$x1==1 & dat.diff.outcome$x2==0 & dat.diff.outcome$x3==0 & dat.diff.outcome$x4==1))
    
    x <- length(which(dat.diff.outcome$y==0 & dat.diff.outcome$x1==0 & dat.diff.outcome$x2==0 & dat.diff.outcome$x3==0 & dat.diff.outcome$x4==1))
    
    y <- length(which(dat.diff.outcome$y==0 & dat.diff.outcome$x1==1 & dat.diff.outcome$x2==1 & dat.diff.outcome$x3==1 & dat.diff.outcome$x4==0))
    
    z1 <- length(which(dat.diff.outcome$y==0 & dat.diff.outcome$x1==0 & dat.diff.outcome$x2==1 & dat.diff.outcome$x3==1 & dat.diff.outcome$x4==0))
    
    z2 <- length(which(dat.diff.outcome$y==0 & dat.diff.outcome$x1==1 & dat.diff.outcome$x2==0 & dat.diff.outcome$x3==1 & dat.diff.outcome$x4==0))
    
    z3 <- length(which(dat.diff.outcome$y==0 & dat.diff.outcome$x1==0 & dat.diff.outcome$x2==0 & dat.diff.outcome$x3==1 & dat.diff.outcome$x4==0))
    
    z4 <- length(which(dat.diff.outcome$y==0 & dat.diff.outcome$x1==1 & dat.diff.outcome$x2==1 & dat.diff.outcome$x3==0 & dat.diff.outcome$x4==0))
    
    z5 <- length(which(dat.diff.outcome$y==0 & dat.diff.outcome$x1==0 & dat.diff.outcome$x2==1 & dat.diff.outcome$x3==0 & dat.diff.outcome$x4==0))
    
    z6 <- length(which(dat.diff.outcome$y==0 & dat.diff.outcome$x1==1 & dat.diff.outcome$x2==0 & dat.diff.outcome$x3==0 & dat.diff.outcome$x4==0))
    
    z7 <- length(which(dat.diff.outcome$y==0 & dat.diff.outcome$x1==0 & dat.diff.outcome$x2==0 & dat.diff.outcome$x3==0 & dat.diff.outcome$x4==0))
    
    
    #Calculate expected truth based on sensitivity and specificity
    a.expected <- (a-(a+q)*(1-sp1)) / (se1-(1-sp1))
    b.expected <- (b-(b+r)*(1-sp0)) / (se0-(1-sp0))
    q.expected <- (a+q)-a.expected
    r.expected <- (b+r)-b.expected
    
    c.expected <- (c-(c+s)*(1-sp1)) / (se1-(1-sp1))
    d.expected <- (d-(d+t)*(1-sp0)) / (se0-(1-sp0))
    s.expected <- (c+s)-c.expected
    t.expected <- (d+t)-d.expected
    
    e.expected <- (e-(e+u)*(1-sp1)) / (se1-(1-sp1))
    f.expected <- (f-(f+v)*(1-sp0)) / (se0-(1-sp0))
    u.expected <- (e+u)-e.expected
    v.expected <- (f+v)-f.expected
    
    g.expected <- (g-(g+w)*(1-sp1)) / (se1-(1-sp1))
    h.expected <- (h-(h+x)*(1-sp0)) / (se1-(1-sp0))
    w.expected <- (g+w)-g.expected
    x.expected <- (h+x)-h.expected
    
    i.expected <- (i-(i+y)*(1-sp1)) / (se1-(1-sp1))
    j.expected <- (j-(j+z1)*(1-sp0)) / (se0-(1-sp0))
    y.expected <- (i+y)-i.expected
    z1.expected <- (j+z1)-j.expected
    
    k.expected <- (k-(k+z2)*(1-sp1)) / (se1-(1-sp1))
    l.expected <- (l-(l+z3)*(1-sp0)) / (se0-(1-sp0))
    z2.expected <- (k+z2)-k.expected
    z3.expected <- (l+z3)-l.expected
    
    m.expected <- (m-(m+z4)*(1-sp1)) / (se1-(1-sp1))
    n.expected <- (n-(n+z5)*(1-sp0)) / (se0-(1-sp0))
    z4.expected <- (m+z4)-m.expected
    z5.expected <- (n+z5)-n.expected
    
    o.expected <- (o-(o+z6)*(1-sp1)) / (se1-(1-sp1))
    p.expected <- (p-(p+z7)*(1-sp0)) / (se0-(1-sp0))
    z6.expected <- (o+z6)-o.expected
    z7.expected <- (p+z7)-p.expected
    
    
    #Count up number of negative values
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
    
    negative_total <- sum(negative.a.expected, negative.b.expected, negative.c.expected, negative.d.expected, negative.e.expected, negative.f.expected, negative.g.expected, negative.h.expected, negative.i.expected, negative.j.expected, negative.k.expected, negative.l.expected, negative.m.expected, negative.n.expected, negative.o.expected, negative.p.expected, negative.q.expected, negative.r.expected, negative.s.expected, negative.t.expected, negative.u.expected, negative.v.expected, negative.w.expected, negative.x.expected, negative.y.expected, negative.z1.expected, negative.z2.expected, negative.z3.expected, negative.z4.expected, negative.z5.expected, negative.z6.expected, negative.z7.expected
                      ) #merp
    
    if (negative_total==0) break
  }
  
  #From the expected truth, calculate false positives, false negatives, true positives, and true negatives for those who were exposed
  t.pos.exp.combo1 <- se1*a.expected
  t.neg.exp.combo1 <- sp1*q.expected
  f.pos.exp.combo1 <- (1-sp1)*q.expected
  f.neg.exp.combo1 <- (1-se1)*a.expected
  
  t.pos.exp.combo2 <- se0*b.expected
  t.neg.exp.combo2 <- sp0*r.expected
  f.pos.exp.combo2 <- (1-sp0)*r.expected
  f.neg.exp.combo2 <- (1-se0)*b.expected
  
  t.pos.exp.combo3 <- se1*c.expected
  t.neg.exp.combo3 <- sp1*s.expected
  f.pos.exp.combo3 <- (1-sp1)*s.expected
  f.neg.exp.combo3 <- (1-se1)*c.expected
  
  t.pos.exp.combo4 <- se0*d.expected
  t.neg.exp.combo4 <- sp0*t.expected
  f.pos.exp.combo4 <- (1-sp0)*t.expected
  f.neg.exp.combo4 <- (1-se0)*d.expected
  
  t.pos.exp.combo5 <- se1*e.expected
  t.neg.exp.combo5 <- sp1*u.expected
  f.pos.exp.combo5 <- (1-sp1)*u.expected
  f.neg.exp.combo5 <- (1-se1)*e.expected
  
  t.pos.exp.combo6 <- se0*f.expected
  t.neg.exp.combo6 <- sp0*v.expected
  f.pos.exp.combo6 <- (1-sp0)*v.expected
  f.neg.exp.combo6 <- (1-se0)*f.expected
  
  t.pos.exp.combo7 <- se1*g.expected
  t.neg.exp.combo7 <- sp1*w.expected
  f.pos.exp.combo7 <- (1-sp1)*w.expected
  f.neg.exp.combo7 <- (1-se1)*g.expected
  
  t.pos.exp.combo8 <- se0*h.expected
  t.neg.exp.combo8 <- sp0*x.expected
  f.pos.exp.combo8 <- (1-sp0)*x.expected
  f.neg.exp.combo8 <- (1-se0)*h.expected
  
  t.pos.exp.combo9 <- se1*i.expected
  t.neg.exp.combo9 <- sp1*y.expected
  f.pos.exp.combo9 <- (1-sp1)*y.expected
  f.neg.exp.combo9 <- (1-se1)*i.expected
  
  t.pos.exp.combo10 <- se0*j.expected
  t.neg.exp.combo10 <- sp0*z1.expected
  f.pos.exp.combo10 <- (1-sp0)*z1.expected
  f.neg.exp.combo10 <- (1-se0)*j.expected
  
  t.pos.exp.combo11 <- se1*k.expected
  t.neg.exp.combo11 <- sp1*z2.expected
  f.pos.exp.combo11 <- (1-sp1)*z2.expected
  f.neg.exp.combo11 <- (1-se1)*k.expected
  
  t.pos.exp.combo12 <- se0*l.expected
  t.neg.exp.combo12 <- sp0*z3.expected
  f.pos.exp.combo12 <- (1-sp0)*z3.expected
  f.neg.exp.combo12 <- (1-se0)*l.expected
  
  t.pos.exp.combo13 <- se1*m.expected
  t.neg.exp.combo13 <- sp1*z4.expected
  f.pos.exp.combo13 <- (1-sp1)*z4.expected
  f.neg.exp.combo13 <- (1-se1)*m.expected
  
  t.pos.exp.combo14 <- se0*n.expected
  t.neg.exp.combo14 <- sp0*z5.expected
  f.pos.exp.combo14 <- (1-sp0)*z5.expected
  f.neg.exp.combo14 <- (1-se0)*n.expected
  
  t.pos.exp.combo15 <- se1*o.expected
  t.neg.exp.combo15 <- sp1*z6.expected
  f.pos.exp.combo15 <- (1-sp1)*z6.expected
  f.neg.exp.combo15 <- (1-se1)*o.expected
  
  t.pos.exp.combo16 <- se0*p.expected
  t.neg.exp.combo16 <- sp0*z7.expected
  f.pos.exp.combo16 <- (1-sp0)*z7.expected
  f.neg.exp.combo16 <- (1-se0)*p.expected
  
  

  #From expected truth, calculate PPV and NPV for E+ and E-
  ppv.exp.combo1 <- t.pos.exp.combo1 / sum(t.pos.exp.combo1, f.pos.exp.combo1)
  npv.exp.combo1 <- t.neg.exp.combo1 / sum(t.neg.exp.combo1, f.neg.exp.combo1)
  
  ppv.exp.combo2 <- t.pos.exp.combo2 / sum(t.pos.exp.combo2, f.pos.exp.combo2)
  npv.exp.combo2 <- t.neg.exp.combo2 / sum(t.neg.exp.combo2, f.neg.exp.combo2)
  
  ppv.exp.combo3 <- t.pos.exp.combo3 / sum(t.pos.exp.combo3, f.pos.exp.combo3)
  npv.exp.combo3 <- t.neg.exp.combo3 / sum(t.neg.exp.combo3, f.neg.exp.combo3)
  
  ppv.exp.combo4 <- t.pos.exp.combo4 / sum(t.pos.exp.combo4, f.pos.exp.combo4)
  npv.exp.combo4 <- t.neg.exp.combo4 / sum(t.neg.exp.combo4, f.neg.exp.combo4)
  
  ppv.exp.combo5 <- t.pos.exp.combo5 / sum(t.pos.exp.combo5, f.pos.exp.combo5)
  npv.exp.combo5 <- t.neg.exp.combo5 / sum(t.neg.exp.combo5, f.neg.exp.combo5)
  
  ppv.exp.combo6 <- t.pos.exp.combo6 / sum(t.pos.exp.combo6, f.pos.exp.combo6)
  npv.exp.combo6 <- t.neg.exp.combo6 / sum(t.neg.exp.combo6, f.neg.exp.combo6)
  
  ppv.exp.combo7 <- t.pos.exp.combo7 / sum(t.pos.exp.combo7, f.pos.exp.combo7)
  npv.exp.combo7 <- t.neg.exp.combo7 / sum(t.neg.exp.combo7, f.neg.exp.combo7)
  
  ppv.exp.combo8 <- t.pos.exp.combo8 / sum(t.pos.exp.combo8, f.pos.exp.combo8)
  npv.exp.combo8 <- t.neg.exp.combo8 / sum(t.neg.exp.combo8, f.neg.exp.combo8)
  
  ppv.exp.combo9 <- t.pos.exp.combo9 / sum(t.pos.exp.combo9, f.pos.exp.combo9)
  npv.exp.combo9 <- t.neg.exp.combo9 / sum(t.neg.exp.combo9, f.neg.exp.combo9)
  
  ppv.exp.combo10 <- t.pos.exp.combo10 / sum(t.pos.exp.combo10, f.pos.exp.combo10)
  npv.exp.combo10 <- t.neg.exp.combo10 / sum(t.neg.exp.combo10, f.neg.exp.combo10)
  
  ppv.exp.combo11 <- t.pos.exp.combo11 / sum(t.pos.exp.combo11, f.pos.exp.combo11)
  npv.exp.combo11 <- t.neg.exp.combo11 / sum(t.neg.exp.combo11, f.neg.exp.combo11)
  
  ppv.exp.combo12 <- t.pos.exp.combo12 / sum(t.pos.exp.combo12, f.pos.exp.combo12)
  npv.exp.combo12 <- t.neg.exp.combo12 / sum(t.neg.exp.combo12, f.neg.exp.combo12)
  
  ppv.exp.combo13 <- t.pos.exp.combo13 / sum(t.pos.exp.combo13, f.pos.exp.combo13)
  npv.exp.combo13 <- t.neg.exp.combo13 / sum(t.neg.exp.combo13, f.neg.exp.combo13)
  
  ppv.exp.combo14 <- t.pos.exp.combo14 / sum(t.pos.exp.combo14, f.pos.exp.combo14)
  npv.exp.combo14 <- t.neg.exp.combo14 / sum(t.neg.exp.combo14, f.neg.exp.combo14)
  
  ppv.exp.combo15 <- t.pos.exp.combo15 / sum(t.pos.exp.combo15, f.pos.exp.combo15)
  npv.exp.combo15 <- t.neg.exp.combo15 / sum(t.neg.exp.combo15, f.neg.exp.combo15)
  
  ppv.exp.combo16 <- t.pos.exp.combo16 / sum(t.pos.exp.combo16, f.pos.exp.combo16)
  npv.exp.combo16 <- t.neg.exp.combo16 / sum(t.neg.exp.combo16, f.neg.exp.combo16)
  
  
  #Record level correction of data using PPV and NPV
  dat.diff.outcome.cor <- dat.diff.outcome
  
  dat.diff.outcome.cor$y.expected <- NA
  

  dat.diff.outcome.cor$y.expected[dat.diff.outcome.cor$y==1 & dat.diff.outcome.cor$x1==1 & dat.diff.outcome.cor$x2==1 & dat.diff.outcome.cor$x3==1 & dat.diff.outcome.cor$x4==1] <- rbinom(length(which(dat.diff.outcome.cor$y==1 & dat.diff.outcome.cor$x1==1 & dat.diff.outcome.cor$x2==1 & dat.diff.outcome.cor$x3==1 & dat.diff.outcome.cor$x4==1)), 1, ppv.exp.combo1)
  
  dat.diff.outcome.cor$y.expected[dat.diff.outcome.cor$y==1 & dat.diff.outcome.cor$x1==0 & dat.diff.outcome.cor$x2==1 & dat.diff.outcome.cor$x3==1 & dat.diff.outcome.cor$x4==1] <- rbinom(length(which(dat.diff.outcome.cor$y==1 & dat.diff.outcome.cor$x1==0 & dat.diff.outcome.cor$x2==1 & dat.diff.outcome.cor$x3==1 & dat.diff.outcome.cor$x4==1)), 1, ppv.exp.combo2)
  
  dat.diff.outcome.cor$y.expected[dat.diff.outcome.cor$y==0 & dat.diff.outcome.cor$x1==1 & dat.diff.outcome.cor$x2==1 & dat.diff.outcome.cor$x3==1 & dat.diff.outcome.cor$x4==1] <- rbinom(length(which(dat.diff.outcome.cor$y==0 & dat.diff.outcome.cor$x1==1 & dat.diff.outcome.cor$x2==1 & dat.diff.outcome.cor$x3==1 & dat.diff.outcome.cor$x4==1)), 1, 1-npv.exp.combo1)
  
  dat.diff.outcome.cor$y.expected[dat.diff.outcome.cor$y==0 & dat.diff.outcome.cor$x1==0 & dat.diff.outcome.cor$x2==1 & dat.diff.outcome.cor$x3==1 & dat.diff.outcome.cor$x4==1] <- rbinom(length(which(dat.diff.outcome.cor$y==0 & dat.diff.outcome.cor$x1==0 & dat.diff.outcome.cor$x2==1 & dat.diff.outcome.cor$x3==1 & dat.diff.outcome.cor$x4==1)), 1, 1-npv.exp.combo2)
  
  
  
  dat.diff.outcome.cor$y.expected[dat.diff.outcome.cor$y==1 & dat.diff.outcome.cor$x1==1 & dat.diff.outcome.cor$x2==0 & dat.diff.outcome.cor$x3==1 & dat.diff.outcome.cor$x4==1] <- rbinom(length(which(dat.diff.outcome.cor$y==1 & dat.diff.outcome.cor$x1==1 & dat.diff.outcome.cor$x2==0 & dat.diff.outcome.cor$x3==1 & dat.diff.outcome.cor$x4==1)), 1, ppv.exp.combo3)
  
  dat.diff.outcome.cor$y.expected[dat.diff.outcome.cor$y==1 & dat.diff.outcome.cor$x1==0 & dat.diff.outcome.cor$x2==0 & dat.diff.outcome.cor$x3==1 & dat.diff.outcome.cor$x4==1] <- rbinom(length(which(dat.diff.outcome.cor$y==1 & dat.diff.outcome.cor$x1==0 & dat.diff.outcome.cor$x2==0 & dat.diff.outcome.cor$x3==1 & dat.diff.outcome.cor$x4==1)), 1, ppv.exp.combo4)
  
  dat.diff.outcome.cor$y.expected[dat.diff.outcome.cor$y==0 & dat.diff.outcome.cor$x1==1 & dat.diff.outcome.cor$x2==0 & dat.diff.outcome.cor$x3==1 & dat.diff.outcome.cor$x4==1] <- rbinom(length(which(dat.diff.outcome.cor$y==0 & dat.diff.outcome.cor$x1==1 & dat.diff.outcome.cor$x2==0 & dat.diff.outcome.cor$x3==1 & dat.diff.outcome.cor$x4==1)), 1, 1-npv.exp.combo3)
  
  dat.diff.outcome.cor$y.expected[dat.diff.outcome.cor$y==0 & dat.diff.outcome.cor$x1==0 & dat.diff.outcome.cor$x2==0 & dat.diff.outcome.cor$x3==1 & dat.diff.outcome.cor$x4==1] <- rbinom(length(which(dat.diff.outcome.cor$y==0 & dat.diff.outcome.cor$x1==0 & dat.diff.outcome.cor$x2==0 & dat.diff.outcome.cor$x3==1 & dat.diff.outcome.cor$x4==1)), 1, 1-npv.exp.combo4)
  
  
  
  dat.diff.outcome.cor$y.expected[dat.diff.outcome.cor$y==1 & dat.diff.outcome.cor$x1==1 & dat.diff.outcome.cor$x2==1 & dat.diff.outcome.cor$x3==0 & dat.diff.outcome.cor$x4==1] <- rbinom(length(which(dat.diff.outcome.cor$y==1 & dat.diff.outcome.cor$x1==1 & dat.diff.outcome.cor$x2==1 & dat.diff.outcome.cor$x3==0 & dat.diff.outcome.cor$x4==1)), 1, ppv.exp.combo5)
  
  dat.diff.outcome.cor$y.expected[dat.diff.outcome.cor$y==1 & dat.diff.outcome.cor$x1==0 & dat.diff.outcome.cor$x2==1 & dat.diff.outcome.cor$x3==0 & dat.diff.outcome.cor$x4==1] <- rbinom(length(which(dat.diff.outcome.cor$y==1 & dat.diff.outcome.cor$x1==0 & dat.diff.outcome.cor$x2==1 & dat.diff.outcome.cor$x3==0 & dat.diff.outcome.cor$x4==1)), 1, ppv.exp.combo6)
  
  dat.diff.outcome.cor$y.expected[dat.diff.outcome.cor$y==0 & dat.diff.outcome.cor$x1==1 & dat.diff.outcome.cor$x2==1 & dat.diff.outcome.cor$x3==0 & dat.diff.outcome.cor$x4==1] <- rbinom(length(which(dat.diff.outcome.cor$y==0 & dat.diff.outcome.cor$x1==1 & dat.diff.outcome.cor$x2==1 & dat.diff.outcome.cor$x3==0 & dat.diff.outcome.cor$x4==1)), 1, 1-npv.exp.combo5)
  
  dat.diff.outcome.cor$y.expected[dat.diff.outcome.cor$y==0 & dat.diff.outcome.cor$x1==0 & dat.diff.outcome.cor$x2==1 & dat.diff.outcome.cor$x3==0 & dat.diff.outcome.cor$x4==1] <- rbinom(length(which(dat.diff.outcome.cor$y==0 & dat.diff.outcome.cor$x1==0 & dat.diff.outcome.cor$x2==1 & dat.diff.outcome.cor$x3==0 & dat.diff.outcome.cor$x4==1)), 1, 1-npv.exp.combo6)
  
  
  
  dat.diff.outcome.cor$y.expected[dat.diff.outcome.cor$y==1 & dat.diff.outcome.cor$x1==1 & dat.diff.outcome.cor$x2==0 & dat.diff.outcome.cor$x3==0 & dat.diff.outcome.cor$x4==1] <- rbinom(length(which(dat.diff.outcome.cor$y==1 & dat.diff.outcome.cor$x1==1 & dat.diff.outcome.cor$x2==0 & dat.diff.outcome.cor$x3==0 & dat.diff.outcome.cor$x4==1)), 1, ppv.exp.combo7)
  
  dat.diff.outcome.cor$y.expected[dat.diff.outcome.cor$y==1 & dat.diff.outcome.cor$x1==0 & dat.diff.outcome.cor$x2==0 & dat.diff.outcome.cor$x3==0 & dat.diff.outcome.cor$x4==1] <- rbinom(length(which(dat.diff.outcome.cor$y==1 & dat.diff.outcome.cor$x1==0 & dat.diff.outcome.cor$x2==0 & dat.diff.outcome.cor$x3==0 & dat.diff.outcome.cor$x4==1)), 1, ppv.exp.combo8)
  
  dat.diff.outcome.cor$y.expected[dat.diff.outcome.cor$y==0 & dat.diff.outcome.cor$x1==1 & dat.diff.outcome.cor$x2==0 & dat.diff.outcome.cor$x3==0 & dat.diff.outcome.cor$x4==1] <- rbinom(length(which(dat.diff.outcome.cor$y==0 & dat.diff.outcome.cor$x1==1 & dat.diff.outcome.cor$x2==0 & dat.diff.outcome.cor$x3==0 & dat.diff.outcome.cor$x4==1)), 1, 1-npv.exp.combo7)
  
  dat.diff.outcome.cor$y.expected[dat.diff.outcome.cor$y==0 & dat.diff.outcome.cor$x1==0 & dat.diff.outcome.cor$x2==0 & dat.diff.outcome.cor$x3==0 & dat.diff.outcome.cor$x4==1] <- rbinom(length(which(dat.diff.outcome.cor$y==0 & dat.diff.outcome.cor$x1==0 & dat.diff.outcome.cor$x2==0 & dat.diff.outcome.cor$x3==0 & dat.diff.outcome.cor$x4==1)), 1, 1-npv.exp.combo8)
  
  
  
  dat.diff.outcome.cor$y.expected[dat.diff.outcome.cor$y==1 & dat.diff.outcome.cor$x1==1 & dat.diff.outcome.cor$x2==1 & dat.diff.outcome.cor$x3==1 & dat.diff.outcome.cor$x4==0] <- rbinom(length(which(dat.diff.outcome.cor$y==1 & dat.diff.outcome.cor$x1==1 & dat.diff.outcome.cor$x2==1 & dat.diff.outcome.cor$x3==1 & dat.diff.outcome.cor$x4==0)), 1, ppv.exp.combo9)
  
  dat.diff.outcome.cor$y.expected[dat.diff.outcome.cor$y==1 & dat.diff.outcome.cor$x1==0 & dat.diff.outcome.cor$x2==1 & dat.diff.outcome.cor$x3==1 & dat.diff.outcome.cor$x4==0] <- rbinom(length(which(dat.diff.outcome.cor$y==1 & dat.diff.outcome.cor$x1==0 & dat.diff.outcome.cor$x2==1 & dat.diff.outcome.cor$x3==1 & dat.diff.outcome.cor$x4==0)), 1, ppv.exp.combo10)
  
  dat.diff.outcome.cor$y.expected[dat.diff.outcome.cor$y==0 & dat.diff.outcome.cor$x1==1 & dat.diff.outcome.cor$x2==1 & dat.diff.outcome.cor$x3==1 & dat.diff.outcome.cor$x4==0] <- rbinom(length(which(dat.diff.outcome.cor$y==0 & dat.diff.outcome.cor$x1==1 & dat.diff.outcome.cor$x2==1 & dat.diff.outcome.cor$x3==1 & dat.diff.outcome.cor$x4==0)), 1, 1-npv.exp.combo9)
  
  dat.diff.outcome.cor$y.expected[dat.diff.outcome.cor$y==0 & dat.diff.outcome.cor$x1==0 & dat.diff.outcome.cor$x2==1 & dat.diff.outcome.cor$x3==1 & dat.diff.outcome.cor$x4==0] <- rbinom(length(which(dat.diff.outcome.cor$y==0 & dat.diff.outcome.cor$x1==0 & dat.diff.outcome.cor$x2==1 & dat.diff.outcome.cor$x3==1 & dat.diff.outcome.cor$x4==0)), 1, 1-npv.exp.combo10)
  
  
  
  dat.diff.outcome.cor$y.expected[dat.diff.outcome.cor$y==1 & dat.diff.outcome.cor$x1==1 & dat.diff.outcome.cor$x2==0 & dat.diff.outcome.cor$x3==1 & dat.diff.outcome.cor$x4==0] <- rbinom(length(which(dat.diff.outcome.cor$y==1 & dat.diff.outcome.cor$x1==1 & dat.diff.outcome.cor$x2==0 & dat.diff.outcome.cor$x3==1 & dat.diff.outcome.cor$x4==0)), 1, ppv.exp.combo11)
  
  dat.diff.outcome.cor$y.expected[dat.diff.outcome.cor$y==1 & dat.diff.outcome.cor$x1==0 & dat.diff.outcome.cor$x2==0 & dat.diff.outcome.cor$x3==1 & dat.diff.outcome.cor$x4==0] <- rbinom(length(which(dat.diff.outcome.cor$y==1 & dat.diff.outcome.cor$x1==0 & dat.diff.outcome.cor$x2==0 & dat.diff.outcome.cor$x3==1 & dat.diff.outcome.cor$x4==0)), 1, ppv.exp.combo12)
  
  dat.diff.outcome.cor$y.expected[dat.diff.outcome.cor$y==0 & dat.diff.outcome.cor$x1==1 & dat.diff.outcome.cor$x2==0 & dat.diff.outcome.cor$x3==1 & dat.diff.outcome.cor$x4==0] <- rbinom(length(which(dat.diff.outcome.cor$y==0 & dat.diff.outcome.cor$x1==1 & dat.diff.outcome.cor$x2==0 & dat.diff.outcome.cor$x3==1 & dat.diff.outcome.cor$x4==0)), 1, 1-npv.exp.combo11)
  
  dat.diff.outcome.cor$y.expected[dat.diff.outcome.cor$y==0 & dat.diff.outcome.cor$x1==0 & dat.diff.outcome.cor$x2==0 & dat.diff.outcome.cor$x3==1 & dat.diff.outcome.cor$x4==0] <- rbinom(length(which(dat.diff.outcome.cor$y==0 & dat.diff.outcome.cor$x1==0 & dat.diff.outcome.cor$x2==0 & dat.diff.outcome.cor$x3==1 & dat.diff.outcome.cor$x4==0)), 1, 1-npv.exp.combo12)
  
  
  
  dat.diff.outcome.cor$y.expected[dat.diff.outcome.cor$y==1 & dat.diff.outcome.cor$x1==1 & dat.diff.outcome.cor$x2==1 & dat.diff.outcome.cor$x3==0 & dat.diff.outcome.cor$x4==0] <- rbinom(length(which(dat.diff.outcome.cor$y==1 & dat.diff.outcome.cor$x1==1 & dat.diff.outcome.cor$x2==1 & dat.diff.outcome.cor$x3==0 & dat.diff.outcome.cor$x4==0)), 1, ppv.exp.combo13)
  
  dat.diff.outcome.cor$y.expected[dat.diff.outcome.cor$y==1 & dat.diff.outcome.cor$x1==0 & dat.diff.outcome.cor$x2==1 & dat.diff.outcome.cor$x3==0 & dat.diff.outcome.cor$x4==0] <- rbinom(length(which(dat.diff.outcome.cor$y==1 & dat.diff.outcome.cor$x1==0 & dat.diff.outcome.cor$x2==1 & dat.diff.outcome.cor$x3==0 & dat.diff.outcome.cor$x4==0)), 1, ppv.exp.combo14)
  
  dat.diff.outcome.cor$y.expected[dat.diff.outcome.cor$y==0 & dat.diff.outcome.cor$x1==1 & dat.diff.outcome.cor$x2==1 & dat.diff.outcome.cor$x3==0 & dat.diff.outcome.cor$x4==0] <- rbinom(length(which(dat.diff.outcome.cor$y==0 & dat.diff.outcome.cor$x1==1 & dat.diff.outcome.cor$x2==1 & dat.diff.outcome.cor$x3==0 & dat.diff.outcome.cor$x4==0)), 1, 1-npv.exp.combo13)
  
  dat.diff.outcome.cor$y.expected[dat.diff.outcome.cor$y==0 & dat.diff.outcome.cor$x1==0 & dat.diff.outcome.cor$x2==1 & dat.diff.outcome.cor$x3==0 & dat.diff.outcome.cor$x4==0] <- rbinom(length(which(dat.diff.outcome.cor$y==0 & dat.diff.outcome.cor$x1==0 & dat.diff.outcome.cor$x2==1 & dat.diff.outcome.cor$x3==0 & dat.diff.outcome.cor$x4==0)), 1, 1-npv.exp.combo14)
  
  
  
  dat.diff.outcome.cor$y.expected[dat.diff.outcome.cor$y==1 & dat.diff.outcome.cor$x1==1 & dat.diff.outcome.cor$x2==0 & dat.diff.outcome.cor$x3==0 & dat.diff.outcome.cor$x4==0] <- rbinom(length(which(dat.diff.outcome.cor$y==1 & dat.diff.outcome.cor$x1==1 & dat.diff.outcome.cor$x2==0 & dat.diff.outcome.cor$x3==0 & dat.diff.outcome.cor$x4==0)), 1, ppv.exp.combo15)
  
  dat.diff.outcome.cor$y.expected[dat.diff.outcome.cor$y==1 & dat.diff.outcome.cor$x1==0 & dat.diff.outcome.cor$x2==0 & dat.diff.outcome.cor$x3==0 & dat.diff.outcome.cor$x4==0] <- rbinom(length(which(dat.diff.outcome.cor$y==1 & dat.diff.outcome.cor$x1==0 & dat.diff.outcome.cor$x2==0 & dat.diff.outcome.cor$x3==0 & dat.diff.outcome.cor$x4==0)), 1, ppv.exp.combo16)
  
  dat.diff.outcome.cor$y.expected[dat.diff.outcome.cor$y==0 & dat.diff.outcome.cor$x1==1 & dat.diff.outcome.cor$x2==0 & dat.diff.outcome.cor$x3==0 & dat.diff.outcome.cor$x4==0] <- rbinom(length(which(dat.diff.outcome.cor$y==0 & dat.diff.outcome.cor$x1==1 & dat.diff.outcome.cor$x2==0 & dat.diff.outcome.cor$x3==0 & dat.diff.outcome.cor$x4==0)), 1, 1-npv.exp.combo15)
  
  dat.diff.outcome.cor$y.expected[dat.diff.outcome.cor$y==0 & dat.diff.outcome.cor$x1==0 & dat.diff.outcome.cor$x2==0 & dat.diff.outcome.cor$x3==0 & dat.diff.outcome.cor$x4==0] <- rbinom(length(which(dat.diff.outcome.cor$y==0 & dat.diff.outcome.cor$x1==0 & dat.diff.outcome.cor$x2==0 & dat.diff.outcome.cor$x3==0 & dat.diff.outcome.cor$x4==0)), 1, 1-npv.exp.combo16)
  
  
  
  vars <- c("x1", "x2", "x3", "x4", "y.expected")
  dat.diff.outcome.cor <- dat.diff.outcome.cor[vars]
  dat.diff.outcome.cor <- data.frame(lapply(dat.diff.outcome.cor, as.factor)) 
  
  #Run random forest for misclassified data set #1 with diff misclass. of outcome
  rf.dat.diff.outcome2 <- randomForest(y.expected ~., data=dat.diff.outcome.cor, importance=T, sampsize=c(length(which(dat.diff.outcome.cor$y.expected==1)), length(which(dat.diff.outcome.cor$y.expected==1))))
  
  #Varimp
  varimp <- importance(rf.dat.diff.outcome2, type=1)
  
  #AUC
  rf.dat.diff.outcome2.roc <- roc(dat.diff.outcome.cor$y.expected, rf.dat.diff.outcome2$votes[,2])
  auc <- auc(rf.dat.diff.outcome2.roc) 
  
  #Other Performance metrics
  confusion.tab <- rf.dat.diff.outcome2$confusion
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
dat.diff.cor.outcome.func.result <- t(sapply(1:1000, dat.diff.cor.outcome.func, se1=myse1, sp1=mysp1, se0=myse0, sp0=mysp0))

write.csv(dat.diff.cor.outcome.func.result, #Took out: , colnames=F,
          paste0("results/dat2_diff_misc_outcome_myse_", myse1, "_mysp1_", mysp1, "_myse0_", myse0, "_mysp0_", mysp0, "_", 
                 format(Sys.time(), "%m%d%Y_%s"), ".csv"),
          quote=F, row.names=F)




