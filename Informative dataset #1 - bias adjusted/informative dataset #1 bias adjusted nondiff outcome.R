#QBA random forests
#Informative dataset #1
#Bias adjustment for non-differential misclassification of outcome for dataset assessing prediction accuracy

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


################################# Non-differential outcome misclassification ################################# 
dat.nondiff.cor.outcome.func <- function(i, se, sp){
  
  repeat {
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
    dat.nondiff.outcome$y.new <- as.factor(dat.nondiff.outcome$y.new)
    
    #Subset data for misclassified variables and the other variables of interest
    vars <- c("x1", "x2", "x3", "x4", "y.new")
    dat.nondiff.outcome2<- dat.nondiff.outcome[vars] 
    
    colnames(dat.nondiff.outcome2) <- c("x1", "x2", "x3", "x4", "y")
    
    #Get the counts for each of the cells in the very large stratified 2x2 table for the misclassified data set for each predictor
    a <- length(which(dat.nondiff.outcome2$y==1 & dat.nondiff.outcome2$x1==1 & dat.nondiff.outcome2$x2==1 & dat.nondiff.outcome2$x3==1 & dat.nondiff.outcome2$x4==1))
    
    b <- length(which(dat.nondiff.outcome2$y==1 & dat.nondiff.outcome2$x1==0 & dat.nondiff.outcome2$x2==1 & dat.nondiff.outcome2$x3==1 & dat.nondiff.outcome2$x4==1))
    
    c <- length(which(dat.nondiff.outcome2$y==1 & dat.nondiff.outcome2$x1==1 & dat.nondiff.outcome2$x2==0 & dat.nondiff.outcome2$x3==1 & dat.nondiff.outcome2$x4==1))
    
    d <- length(which(dat.nondiff.outcome2$y==1 & dat.nondiff.outcome2$x1==0 & dat.nondiff.outcome2$x2==0 & dat.nondiff.outcome2$x3==1 & dat.nondiff.outcome2$x4==1))
    
    e <- length(which(dat.nondiff.outcome2$y==1 & dat.nondiff.outcome2$x1==1 & dat.nondiff.outcome2$x2==1 & dat.nondiff.outcome2$x3==0 & dat.nondiff.outcome2$x4==1))
    
    f <- length(which(dat.nondiff.outcome2$y==1 & dat.nondiff.outcome2$x1==0 & dat.nondiff.outcome2$x2==1 & dat.nondiff.outcome2$x3==0 & dat.nondiff.outcome2$x4==1))
    
    g <- length(which(dat.nondiff.outcome2$y==1 & dat.nondiff.outcome2$x1==1 & dat.nondiff.outcome2$x2==0 & dat.nondiff.outcome2$x3==0 & dat.nondiff.outcome2$x4==1))
    
    h <- length(which(dat.nondiff.outcome2$y==1 & dat.nondiff.outcome2$x1==0 & dat.nondiff.outcome2$x2==0 & dat.nondiff.outcome2$x3==0 & dat.nondiff.outcome2$x4==1))
    
    i <- length(which(dat.nondiff.outcome2$y==1 & dat.nondiff.outcome2$x1==1 & dat.nondiff.outcome2$x2==1 & dat.nondiff.outcome2$x3==1 & dat.nondiff.outcome2$x4==0))
    
    j <- length(which(dat.nondiff.outcome2$y==1 & dat.nondiff.outcome2$x1==0 & dat.nondiff.outcome2$x2==1 & dat.nondiff.outcome2$x3==1 & dat.nondiff.outcome2$x4==0))
    
    k <- length(which(dat.nondiff.outcome2$y==1 & dat.nondiff.outcome2$x1==1 & dat.nondiff.outcome2$x2==0 & dat.nondiff.outcome2$x3==1 & dat.nondiff.outcome2$x4==0))
    
    l <- length(which(dat.nondiff.outcome2$y==1 & dat.nondiff.outcome2$x1==0 & dat.nondiff.outcome2$x2==0 & dat.nondiff.outcome2$x3==1 & dat.nondiff.outcome2$x4==0))
    
    m <- length(which(dat.nondiff.outcome2$y==1 & dat.nondiff.outcome2$x1==1 & dat.nondiff.outcome2$x2==1 & dat.nondiff.outcome2$x3==0 & dat.nondiff.outcome2$x4==0))
    
    n <- length(which(dat.nondiff.outcome2$y==1 & dat.nondiff.outcome2$x1==0 & dat.nondiff.outcome2$x2==1 & dat.nondiff.outcome2$x3==0 & dat.nondiff.outcome2$x4==0))
    
    o <- length(which(dat.nondiff.outcome2$y==1 & dat.nondiff.outcome2$x1==1 & dat.nondiff.outcome2$x2==0 & dat.nondiff.outcome2$x3==0 & dat.nondiff.outcome2$x4==0))
    
    p <- length(which(dat.nondiff.outcome2$y==1 & dat.nondiff.outcome2$x1==0 & dat.nondiff.outcome2$x2==0 & dat.nondiff.outcome2$x3==0 & dat.nondiff.outcome2$x4==0))
    
    q <- length(which(dat.nondiff.outcome2$y==0 & dat.nondiff.outcome2$x1==1 & dat.nondiff.outcome2$x2==1 & dat.nondiff.outcome2$x3==1 & dat.nondiff.outcome2$x4==1))
    
    r <- length(which(dat.nondiff.outcome2$y==0 & dat.nondiff.outcome2$x1==0 & dat.nondiff.outcome2$x2==1 & dat.nondiff.outcome2$x3==1 & dat.nondiff.outcome2$x4==1))
    
    s <- length(which(dat.nondiff.outcome2$y==0 & dat.nondiff.outcome2$x1==1 & dat.nondiff.outcome2$x2==0 & dat.nondiff.outcome2$x3==1 & dat.nondiff.outcome2$x4==1))
    
    t <- length(which(dat.nondiff.outcome2$y==0 & dat.nondiff.outcome2$x1==0 & dat.nondiff.outcome2$x2==0 & dat.nondiff.outcome2$x3==1 & dat.nondiff.outcome2$x4==1))
    
    u <- length(which(dat.nondiff.outcome2$y==0 & dat.nondiff.outcome2$x1==1 & dat.nondiff.outcome2$x2==1 & dat.nondiff.outcome2$x3==0 & dat.nondiff.outcome2$x4==1))
    
    v <- length(which(dat.nondiff.outcome2$y==0 & dat.nondiff.outcome2$x1==0 & dat.nondiff.outcome2$x2==1 & dat.nondiff.outcome2$x3==0 & dat.nondiff.outcome2$x4==1))
    
    w <- length(which(dat.nondiff.outcome2$y==0 & dat.nondiff.outcome2$x1==1 & dat.nondiff.outcome2$x2==0 & dat.nondiff.outcome2$x3==0 & dat.nondiff.outcome2$x4==1))
    
    x <- length(which(dat.nondiff.outcome2$y==0 & dat.nondiff.outcome2$x1==0 & dat.nondiff.outcome2$x2==0 & dat.nondiff.outcome2$x3==0 & dat.nondiff.outcome2$x4==1))
    
    y <- length(which(dat.nondiff.outcome2$y==0 & dat.nondiff.outcome2$x1==1 & dat.nondiff.outcome2$x2==1 & dat.nondiff.outcome2$x3==1 & dat.nondiff.outcome2$x4==0))
    
    z1 <- length(which(dat.nondiff.outcome2$y==0 & dat.nondiff.outcome2$x1==0 & dat.nondiff.outcome2$x2==1 & dat.nondiff.outcome2$x3==1 & dat.nondiff.outcome2$x4==0))
    
    z2 <- length(which(dat.nondiff.outcome2$y==0 & dat.nondiff.outcome2$x1==1 & dat.nondiff.outcome2$x2==0 & dat.nondiff.outcome2$x3==1 & dat.nondiff.outcome2$x4==0))
    
    z3 <- length(which(dat.nondiff.outcome2$y==0 & dat.nondiff.outcome2$x1==0 & dat.nondiff.outcome2$x2==0 & dat.nondiff.outcome2$x3==1 & dat.nondiff.outcome2$x4==0))
    
    z4 <- length(which(dat.nondiff.outcome2$y==0 & dat.nondiff.outcome2$x1==1 & dat.nondiff.outcome2$x2==1 & dat.nondiff.outcome2$x3==0 & dat.nondiff.outcome2$x4==0))
    
    z5 <- length(which(dat.nondiff.outcome2$y==0 & dat.nondiff.outcome2$x1==0 & dat.nondiff.outcome2$x2==1 & dat.nondiff.outcome2$x3==0 & dat.nondiff.outcome2$x4==0))
    
    z6 <- length(which(dat.nondiff.outcome2$y==0 & dat.nondiff.outcome2$x1==1 & dat.nondiff.outcome2$x2==0 & dat.nondiff.outcome2$x3==0 & dat.nondiff.outcome2$x4==0))
    
    z7 <- length(which(dat.nondiff.outcome2$y==0 & dat.nondiff.outcome2$x1==0 & dat.nondiff.outcome2$x2==0 & dat.nondiff.outcome2$x3==0 & dat.nondiff.outcome2$x4==0))
    
    #Calculate expected truth based on sensitivity and specificity
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
    
    
    negative_total <- sum(negative.a.expected, negative.b.expected, negative.c.expected, negative.d.expected, negative.e.expected, negative.f.expected, negative.g.expected, negative.h.expected, negative.i.expected, negative.j.expected, negative.k.expected, negative.l.expected, negative.m.expected, negative.n.expected, negative.o.expected, negative.p.expected, negative.q.expected, negative.r.expected, negative.s.expected, negative.t.expected, negative.u.expected, negative.v.expected, negative.w.expected, negative.x.expected, negative.y.expected, negative.z1.expected, negative.z2.expected, negative.z3.expected, negative.z4.expected, negative.z5.expected, negative.z6.expected, negative.z7.expected) #merp
    if (negative_total==0) break
  }
  
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
  
  #From expected truth, calculate PPV and NPV for E+ and E-
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
  
  
  #Record level correction of data using PPV and NPV
  dat.nondiff.outcome2.cor <- dat.nondiff.outcome2
  
  dat.nondiff.outcome2.cor$y.expected <- NA
  
  dat.nondiff.outcome2.cor$y.expected[dat.nondiff.outcome2.cor$y==1 & dat.nondiff.outcome2.cor$x1==1 & dat.nondiff.outcome2.cor$x2==1 & dat.nondiff.outcome2.cor$x3==1 & dat.nondiff.outcome2.cor$x4==1] <- rbinom(length(which(dat.nondiff.outcome2.cor$y==1 & dat.nondiff.outcome2.cor$x1==1 & dat.nondiff.outcome2.cor$x2==1 & dat.nondiff.outcome2.cor$x3==1 & dat.nondiff.outcome2.cor$x4==1)), 1, ppv.exp.combo1)
  
  dat.nondiff.outcome2.cor$y.expected[dat.nondiff.outcome2.cor$y==1 & dat.nondiff.outcome2.cor$x1==0 & dat.nondiff.outcome2.cor$x2==1 & dat.nondiff.outcome2.cor$x3==1 & dat.nondiff.outcome2.cor$x4==1] <- rbinom(length(which(dat.nondiff.outcome2.cor$y==1 & dat.nondiff.outcome2.cor$x1==0 & dat.nondiff.outcome2.cor$x2==1 & dat.nondiff.outcome2.cor$x3==1 & dat.nondiff.outcome2.cor$x4==1)), 1, ppv.unexp.combo1)
  
  dat.nondiff.outcome2.cor$y.expected[dat.nondiff.outcome2.cor$y==0 & dat.nondiff.outcome2.cor$x1==1 & dat.nondiff.outcome2.cor$x2==1 & dat.nondiff.outcome2.cor$x3==1 & dat.nondiff.outcome2.cor$x4==1] <- rbinom(length(which(dat.nondiff.outcome2.cor$y==0 & dat.nondiff.outcome2.cor$x1==1 & dat.nondiff.outcome2.cor$x2==1 & dat.nondiff.outcome2.cor$x3==1 & dat.nondiff.outcome2.cor$x4==1)), 1, 1-npv.exp.combo1)
  
  dat.nondiff.outcome2.cor$y.expected[dat.nondiff.outcome2.cor$y==0 & dat.nondiff.outcome2.cor$x1==0 & dat.nondiff.outcome2.cor$x2==1 & dat.nondiff.outcome2.cor$x3==1 & dat.nondiff.outcome2.cor$x4==1] <- rbinom(length(which(dat.nondiff.outcome2.cor$y==0 & dat.nondiff.outcome2.cor$x1==0 & dat.nondiff.outcome2.cor$x2==1 & dat.nondiff.outcome2.cor$x3==1 & dat.nondiff.outcome2.cor$x4==1)), 1, 1-npv.unexp.combo1)
  
  
  
  dat.nondiff.outcome2.cor$y.expected[dat.nondiff.outcome2.cor$y==1 & dat.nondiff.outcome2.cor$x1==1 & dat.nondiff.outcome2.cor$x2==0 & dat.nondiff.outcome2.cor$x3==1 & dat.nondiff.outcome2.cor$x4==1] <- rbinom(length(which(dat.nondiff.outcome2.cor$y==1 & dat.nondiff.outcome2.cor$x1==1 & dat.nondiff.outcome2.cor$x2==0 & dat.nondiff.outcome2.cor$x3==1 & dat.nondiff.outcome2.cor$x4==1)), 1, ppv.exp.combo2)
  
  dat.nondiff.outcome2.cor$y.expected[dat.nondiff.outcome2.cor$y==1 & dat.nondiff.outcome2.cor$x1==0 & dat.nondiff.outcome2.cor$x2==0 & dat.nondiff.outcome2.cor$x3==1 & dat.nondiff.outcome2.cor$x4==1] <- rbinom(length(which(dat.nondiff.outcome2.cor$y==1 & dat.nondiff.outcome2.cor$x1==0 & dat.nondiff.outcome2.cor$x2==0 & dat.nondiff.outcome2.cor$x3==1 & dat.nondiff.outcome2.cor$x4==1)), 1, ppv.unexp.combo2)
  
  dat.nondiff.outcome2.cor$y.expected[dat.nondiff.outcome2.cor$y==0 & dat.nondiff.outcome2.cor$x1==1 & dat.nondiff.outcome2.cor$x2==0 & dat.nondiff.outcome2.cor$x3==1 & dat.nondiff.outcome2.cor$x4==1] <- rbinom(length(which(dat.nondiff.outcome2.cor$y==0 & dat.nondiff.outcome2.cor$x1==1 & dat.nondiff.outcome2.cor$x2==0 & dat.nondiff.outcome2.cor$x3==1 & dat.nondiff.outcome2.cor$x4==1)), 1, 1-npv.exp.combo2)
  
  dat.nondiff.outcome2.cor$y.expected[dat.nondiff.outcome2.cor$y==0 & dat.nondiff.outcome2.cor$x1==0 & dat.nondiff.outcome2.cor$x2==0 & dat.nondiff.outcome2.cor$x3==1 & dat.nondiff.outcome2.cor$x4==1] <- rbinom(length(which(dat.nondiff.outcome2.cor$y==0 & dat.nondiff.outcome2.cor$x1==0 & dat.nondiff.outcome2.cor$x2==0 & dat.nondiff.outcome2.cor$x3==1 & dat.nondiff.outcome2.cor$x4==1)), 1, 1-npv.unexp.combo2)
  
  
  
  dat.nondiff.outcome2.cor$y.expected[dat.nondiff.outcome2.cor$y==1 & dat.nondiff.outcome2.cor$x1==1 & dat.nondiff.outcome2.cor$x2==1 & dat.nondiff.outcome2.cor$x3==0 & dat.nondiff.outcome2.cor$x4==1] <- rbinom(length(which(dat.nondiff.outcome2.cor$y==1 & dat.nondiff.outcome2.cor$x1==1 & dat.nondiff.outcome2.cor$x2==1 & dat.nondiff.outcome2.cor$x3==0 & dat.nondiff.outcome2.cor$x4==1)), 1, ppv.exp.combo3)
  
  dat.nondiff.outcome2.cor$y.expected[dat.nondiff.outcome2.cor$y==1 & dat.nondiff.outcome2.cor$x1==0 & dat.nondiff.outcome2.cor$x2==1 & dat.nondiff.outcome2.cor$x3==0 & dat.nondiff.outcome2.cor$x4==1] <- rbinom(length(which(dat.nondiff.outcome2.cor$y==1 & dat.nondiff.outcome2.cor$x1==0 & dat.nondiff.outcome2.cor$x2==1 & dat.nondiff.outcome2.cor$x3==0 & dat.nondiff.outcome2.cor$x4==1)), 1, ppv.unexp.combo3)
  
  dat.nondiff.outcome2.cor$y.expected[dat.nondiff.outcome2.cor$y==0 & dat.nondiff.outcome2.cor$x1==1 & dat.nondiff.outcome2.cor$x2==1 & dat.nondiff.outcome2.cor$x3==0 & dat.nondiff.outcome2.cor$x4==1] <- rbinom(length(which(dat.nondiff.outcome2.cor$y==0 & dat.nondiff.outcome2.cor$x1==1 & dat.nondiff.outcome2.cor$x2==1 & dat.nondiff.outcome2.cor$x3==0 & dat.nondiff.outcome2.cor$x4==1)), 1, 1-npv.exp.combo3)
  
  dat.nondiff.outcome2.cor$y.expected[dat.nondiff.outcome2.cor$y==0 & dat.nondiff.outcome2.cor$x1==0 & dat.nondiff.outcome2.cor$x2==1 & dat.nondiff.outcome2.cor$x3==0 & dat.nondiff.outcome2.cor$x4==1] <- rbinom(length(which(dat.nondiff.outcome2.cor$y==0 & dat.nondiff.outcome2.cor$x1==0 & dat.nondiff.outcome2.cor$x2==1 & dat.nondiff.outcome2.cor$x3==0 & dat.nondiff.outcome2.cor$x4==1)), 1, 1-npv.unexp.combo3)
  
  
  
  dat.nondiff.outcome2.cor$y.expected[dat.nondiff.outcome2.cor$y==1 & dat.nondiff.outcome2.cor$x1==1 & dat.nondiff.outcome2.cor$x2==0 & dat.nondiff.outcome2.cor$x3==0 & dat.nondiff.outcome2.cor$x4==1] <- rbinom(length(which(dat.nondiff.outcome2.cor$y==1 & dat.nondiff.outcome2.cor$x1==1 & dat.nondiff.outcome2.cor$x2==0 & dat.nondiff.outcome2.cor$x3==0 & dat.nondiff.outcome2.cor$x4==1)), 1, ppv.exp.combo4)
  
  dat.nondiff.outcome2.cor$y.expected[dat.nondiff.outcome2.cor$y==1 & dat.nondiff.outcome2.cor$x1==0 & dat.nondiff.outcome2.cor$x2==0 & dat.nondiff.outcome2.cor$x3==0 & dat.nondiff.outcome2.cor$x4==1] <- rbinom(length(which(dat.nondiff.outcome2.cor$y==1 & dat.nondiff.outcome2.cor$x1==0 & dat.nondiff.outcome2.cor$x2==0 & dat.nondiff.outcome2.cor$x3==0 & dat.nondiff.outcome2.cor$x4==1)), 1, ppv.unexp.combo4)
  
  dat.nondiff.outcome2.cor$y.expected[dat.nondiff.outcome2.cor$y==0 & dat.nondiff.outcome2.cor$x1==1 & dat.nondiff.outcome2.cor$x2==0 & dat.nondiff.outcome2.cor$x3==0 & dat.nondiff.outcome2.cor$x4==1] <- rbinom(length(which(dat.nondiff.outcome2.cor$y==0 & dat.nondiff.outcome2.cor$x1==1 & dat.nondiff.outcome2.cor$x2==0 & dat.nondiff.outcome2.cor$x3==0 & dat.nondiff.outcome2.cor$x4==1)), 1, 1-npv.exp.combo4)
  
  dat.nondiff.outcome2.cor$y.expected[dat.nondiff.outcome2.cor$y==0 & dat.nondiff.outcome2.cor$x1==0 & dat.nondiff.outcome2.cor$x2==0 & dat.nondiff.outcome2.cor$x3==0 & dat.nondiff.outcome2.cor$x4==1] <- rbinom(length(which(dat.nondiff.outcome2.cor$y==0 & dat.nondiff.outcome2.cor$x1==0 & dat.nondiff.outcome2.cor$x2==0 & dat.nondiff.outcome2.cor$x3==0 & dat.nondiff.outcome2.cor$x4==1)), 1, 1-npv.unexp.combo4)
  
  
  
  dat.nondiff.outcome2.cor$y.expected[dat.nondiff.outcome2.cor$y==1 & dat.nondiff.outcome2.cor$x1==1 & dat.nondiff.outcome2.cor$x2==1 & dat.nondiff.outcome2.cor$x3==1 & dat.nondiff.outcome2.cor$x4==0] <- rbinom(length(which(dat.nondiff.outcome2.cor$y==1 & dat.nondiff.outcome2.cor$x1==1 & dat.nondiff.outcome2.cor$x2==1 & dat.nondiff.outcome2.cor$x3==1 & dat.nondiff.outcome2.cor$x4==0)), 1, ppv.exp.combo5)
  
  dat.nondiff.outcome2.cor$y.expected[dat.nondiff.outcome2.cor$y==1 & dat.nondiff.outcome2.cor$x1==0 & dat.nondiff.outcome2.cor$x2==1 & dat.nondiff.outcome2.cor$x3==1 & dat.nondiff.outcome2.cor$x4==0] <- rbinom(length(which(dat.nondiff.outcome2.cor$y==1 & dat.nondiff.outcome2.cor$x1==0 & dat.nondiff.outcome2.cor$x2==1 & dat.nondiff.outcome2.cor$x3==1 & dat.nondiff.outcome2.cor$x4==0)), 1, ppv.unexp.combo5)
  
  dat.nondiff.outcome2.cor$y.expected[dat.nondiff.outcome2.cor$y==0 & dat.nondiff.outcome2.cor$x1==1 & dat.nondiff.outcome2.cor$x2==1 & dat.nondiff.outcome2.cor$x3==1 & dat.nondiff.outcome2.cor$x4==0] <- rbinom(length(which(dat.nondiff.outcome2.cor$y==0 & dat.nondiff.outcome2.cor$x1==1 & dat.nondiff.outcome2.cor$x2==1 & dat.nondiff.outcome2.cor$x3==1 & dat.nondiff.outcome2.cor$x4==0)), 1, 1-npv.exp.combo5)
  
  dat.nondiff.outcome2.cor$y.expected[dat.nondiff.outcome2.cor$y==0 & dat.nondiff.outcome2.cor$x1==0 & dat.nondiff.outcome2.cor$x2==1 & dat.nondiff.outcome2.cor$x3==1 & dat.nondiff.outcome2.cor$x4==0] <- rbinom(length(which(dat.nondiff.outcome2.cor$y==0 & dat.nondiff.outcome2.cor$x1==0 & dat.nondiff.outcome2.cor$x2==1 & dat.nondiff.outcome2.cor$x3==1 & dat.nondiff.outcome2.cor$x4==0)), 1, 1-npv.unexp.combo5)
  
  
  
  dat.nondiff.outcome2.cor$y.expected[dat.nondiff.outcome2.cor$y==1 & dat.nondiff.outcome2.cor$x1==1 & dat.nondiff.outcome2.cor$x2==0 & dat.nondiff.outcome2.cor$x3==1 & dat.nondiff.outcome2.cor$x4==0] <- rbinom(length(which(dat.nondiff.outcome2.cor$y==1 & dat.nondiff.outcome2.cor$x1==1 & dat.nondiff.outcome2.cor$x2==0 & dat.nondiff.outcome2.cor$x3==1 & dat.nondiff.outcome2.cor$x4==0)), 1, ppv.exp.combo6)
  
  dat.nondiff.outcome2.cor$y.expected[dat.nondiff.outcome2.cor$y==1 & dat.nondiff.outcome2.cor$x1==0 & dat.nondiff.outcome2.cor$x2==0 & dat.nondiff.outcome2.cor$x3==1 & dat.nondiff.outcome2.cor$x4==0] <- rbinom(length(which(dat.nondiff.outcome2.cor$y==1 & dat.nondiff.outcome2.cor$x1==0 & dat.nondiff.outcome2.cor$x2==0 & dat.nondiff.outcome2.cor$x3==1 & dat.nondiff.outcome2.cor$x4==0)), 1, ppv.unexp.combo6)
  
  dat.nondiff.outcome2.cor$y.expected[dat.nondiff.outcome2.cor$y==0 & dat.nondiff.outcome2.cor$x1==1 & dat.nondiff.outcome2.cor$x2==0 & dat.nondiff.outcome2.cor$x3==1 & dat.nondiff.outcome2.cor$x4==0] <- rbinom(length(which(dat.nondiff.outcome2.cor$y==0 & dat.nondiff.outcome2.cor$x1==1 & dat.nondiff.outcome2.cor$x2==0 & dat.nondiff.outcome2.cor$x3==1 & dat.nondiff.outcome2.cor$x4==0)), 1, 1-npv.exp.combo6)
  
  dat.nondiff.outcome2.cor$y.expected[dat.nondiff.outcome2.cor$y==0 & dat.nondiff.outcome2.cor$x1==0 & dat.nondiff.outcome2.cor$x2==0 & dat.nondiff.outcome2.cor$x3==1 & dat.nondiff.outcome2.cor$x4==0] <- rbinom(length(which(dat.nondiff.outcome2.cor$y==0 & dat.nondiff.outcome2.cor$x1==0 & dat.nondiff.outcome2.cor$x2==0 & dat.nondiff.outcome2.cor$x3==1 & dat.nondiff.outcome2.cor$x4==0)), 1, 1-npv.unexp.combo6)
  
  
  
  dat.nondiff.outcome2.cor$y.expected[dat.nondiff.outcome2.cor$y==1 & dat.nondiff.outcome2.cor$x1==1 & dat.nondiff.outcome2.cor$x2==1 & dat.nondiff.outcome2.cor$x3==0 & dat.nondiff.outcome2.cor$x4==0] <- rbinom(length(which(dat.nondiff.outcome2.cor$y==1 & dat.nondiff.outcome2.cor$x1==1 & dat.nondiff.outcome2.cor$x2==1 & dat.nondiff.outcome2.cor$x3==0 & dat.nondiff.outcome2.cor$x4==0)), 1, ppv.exp.combo7)
  
  dat.nondiff.outcome2.cor$y.expected[dat.nondiff.outcome2.cor$y==1 & dat.nondiff.outcome2.cor$x1==0 & dat.nondiff.outcome2.cor$x2==1 & dat.nondiff.outcome2.cor$x3==0 & dat.nondiff.outcome2.cor$x4==0] <- rbinom(length(which(dat.nondiff.outcome2.cor$y==1 & dat.nondiff.outcome2.cor$x1==0 & dat.nondiff.outcome2.cor$x2==1 & dat.nondiff.outcome2.cor$x3==0 & dat.nondiff.outcome2.cor$x4==0)), 1, ppv.unexp.combo7)
  
  dat.nondiff.outcome2.cor$y.expected[dat.nondiff.outcome2.cor$y==0 & dat.nondiff.outcome2.cor$x1==1 & dat.nondiff.outcome2.cor$x2==1 & dat.nondiff.outcome2.cor$x3==0 & dat.nondiff.outcome2.cor$x4==0] <- rbinom(length(which(dat.nondiff.outcome2.cor$y==0 & dat.nondiff.outcome2.cor$x1==1 & dat.nondiff.outcome2.cor$x2==1 & dat.nondiff.outcome2.cor$x3==0 & dat.nondiff.outcome2.cor$x4==0)), 1, 1-npv.exp.combo7)
  
  dat.nondiff.outcome2.cor$y.expected[dat.nondiff.outcome2.cor$y==0 & dat.nondiff.outcome2.cor$x1==0 & dat.nondiff.outcome2.cor$x2==1 & dat.nondiff.outcome2.cor$x3==0 & dat.nondiff.outcome2.cor$x4==0] <- rbinom(length(which(dat.nondiff.outcome2.cor$y==0 & dat.nondiff.outcome2.cor$x1==0 & dat.nondiff.outcome2.cor$x2==1 & dat.nondiff.outcome2.cor$x3==0 & dat.nondiff.outcome2.cor$x4==0)), 1, 1-npv.unexp.combo7)
  
  
  
  dat.nondiff.outcome2.cor$y.expected[dat.nondiff.outcome2.cor$y==1 & dat.nondiff.outcome2.cor$x1==1 & dat.nondiff.outcome2.cor$x2==0 & dat.nondiff.outcome2.cor$x3==0 & dat.nondiff.outcome2.cor$x4==0] <- rbinom(length(which(dat.nondiff.outcome2.cor$y==1 & dat.nondiff.outcome2.cor$x1==1 & dat.nondiff.outcome2.cor$x2==0 & dat.nondiff.outcome2.cor$x3==0 & dat.nondiff.outcome2.cor$x4==0)), 1, ppv.exp.combo8)
  
  dat.nondiff.outcome2.cor$y.expected[dat.nondiff.outcome2.cor$y==1 & dat.nondiff.outcome2.cor$x1==0 & dat.nondiff.outcome2.cor$x2==0 & dat.nondiff.outcome2.cor$x3==0 & dat.nondiff.outcome2.cor$x4==0] <- rbinom(length(which(dat.nondiff.outcome2.cor$y==1 & dat.nondiff.outcome2.cor$x1==0 & dat.nondiff.outcome2.cor$x2==0 & dat.nondiff.outcome2.cor$x3==0 & dat.nondiff.outcome2.cor$x4==0)), 1, ppv.unexp.combo8)
  
  dat.nondiff.outcome2.cor$y.expected[dat.nondiff.outcome2.cor$y==0 & dat.nondiff.outcome2.cor$x1==1 & dat.nondiff.outcome2.cor$x2==0 & dat.nondiff.outcome2.cor$x3==0 & dat.nondiff.outcome2.cor$x4==0] <- rbinom(length(which(dat.nondiff.outcome2.cor$y==0 & dat.nondiff.outcome2.cor$x1==1 & dat.nondiff.outcome2.cor$x2==0 & dat.nondiff.outcome2.cor$x3==0 & dat.nondiff.outcome2.cor$x4==0)), 1, 1-npv.exp.combo8)
  
  dat.nondiff.outcome2.cor$y.expected[dat.nondiff.outcome2.cor$y==0 & dat.nondiff.outcome2.cor$x1==0 & dat.nondiff.outcome2.cor$x2==0 & dat.nondiff.outcome2.cor$x3==0 & dat.nondiff.outcome2.cor$x4==0] <- rbinom(length(which(dat.nondiff.outcome2.cor$y==0 & dat.nondiff.outcome2.cor$x1==0 & dat.nondiff.outcome2.cor$x2==0 & dat.nondiff.outcome2.cor$x3==0 & dat.nondiff.outcome2.cor$x4==0)), 1, 1-npv.unexp.combo8)
  
  vars <- c("x1", "x2", "x3", "x4", "y.expected")
  dat.nondiff.outcome2.cor <- dat.nondiff.outcome2.cor[vars]
  dat.nondiff.outcome2.cor <- data.frame(lapply(dat.nondiff.outcome2.cor, as.factor)) 
    
    
  #Run random forest 
  rf.dat.nondiff.outcome <- randomForest(y.expected ~., data=dat.nondiff.outcome2.cor, importance=T, sampsize=c(length(which(dat.nondiff.outcome2.cor$y.expected==1)), length(which(dat.nondiff.outcome2.cor$y.expected==1))))
  
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
dat.nondiff.cor.outcome.func.result <- t(sapply(1:10000, dat.nondiff.cor.outcome.func, se=myse,sp=mysp))  
  
  