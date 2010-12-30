nlsfit.gb2 <- function(z, w=1){

  d     <- data.frame(inc=z,w=w)
	d     <- d[!is.na(d$inc),]
   
# Truncate at 0
  inc   <- d$inc[d$inc > 0]
  w     <- d$w[d$inc > 0]

# Initial values under Fisk
  par0  <- fisk(inc, w)
  #a0 <- par0[1]
  #b0 <- par0[2]

# Empirical estimates of the indicators
indicEall  <- main.emp(inc, w)
indicE <- c(indicEall[1],indicEall[3:6])

# nls fit
fit <- nls(indicE ~ c(main.gb2(0.6, a, b, p, q)[1], main.gb2(0.6, a, b, p, q)[3:6]),
           start = list(a=par0[1], b=par0[2], p=par0[3], q=par0[4]), weights = c(0.001, 1, 1, 0.5, 0.5), trace=FALSE, algorithm = "port",
		       lower = c(0.01, 0.01, 0.01, 0.01), upper = c(2*par0[1], 2*par0[2], 5, 5), control = nls.control(maxiter = 1000, tol = 1e-06, minFactor = 1/1024,
           printEval = FALSE, warnOnly = FALSE))

an <- coef(fit)[[1]]
bn <- coef(fit)[[2]]
pn <- coef(fit)[[3]]
qn <- coef(fit)[[4]]

# likelihood
nlik <- loglp.gb2(inc, an, bn, pn, qn, w)

# GB2 indicators
indicN <- main.gb2(0.6, an, bn, pn, qn)

type=c("Emp. est","NLS")
results <- data.frame(type=type,
        median=c(indicE[1],indicN[1]),
        ARPR=c(indicE[2],indicN[3]),
        RMPG=c(indicE[3],indicN[4]),
        QSR=c(indicE[4],indicN[5]),
        GINI=c(indicE[5],indicN[6]),
        likelihood=c(0,nlik),
        a=c(0,an), b=c(0,bn) ,p=c(0,pn), q=c(0,qn))

return(list(data.frame(results), fit))
}

nlsfit2.gb2 <-
function(z, w=1, a.fit, b.fit, p.fit, q.fit, cva.fit){

  d  <- data.frame(inc=z,w=w)
  d  <- d[!is.na(d$inc),]
   
# Truncate at 0
  inc   <- d$inc[d$inc > 0]
  w     <- d$w[d$inc > 0]

# Empirical estimates of the indicators
  indicEall  <- main.emp(inc, w)
  indicE <- indicEall[3:6]
  med <- indicEall[1] #the median

# nls fit 1
  bound1 <- a.fit*max(0.2,1-2*cva.fit)         
  bound2 <- a.fit*min(1.8,1+2*cva.fit)

  fit1 <- nls(indicE ~ main2.gb2(0.6, a, 1, ap, aq)[3:6], weights = c(0.1,0.1,1,1), start = list( a=a.fit, ap=a.fit*p.fit, aq=a.fit*q.fit), 
          trace=FALSE, algorithm = "port", lower = c(bound1, 1, 2), upper = c(bound2, 100, 100), 
          control = nls.control(maxiter = 1000, tol = 1e-06, minFactor = 1/1024, printEval = FALSE, warnOnly = TRUE))

  an <- coef(fit1)[[1]]
  pn <- coef(fit1)[[2]]/an
  qn <- coef(fit1)[[3]]/an

# nls fit 2
  fit2 <- nls(med ~ qgb2(0.5, an, b, pn, qn), start = list(b=b.fit), 
          trace=FALSE, algorithm = "port", lower = c(0.01), upper = c(2*b.fit), 
          control = nls.control(maxiter = 1000, tol = 1e-06, minFactor = 1/1024, printEval = FALSE, warnOnly = TRUE))

  bn <- coef(fit2)[[1]]

# likelihood
  nlik <- loglp.gb2(inc, an, bn, pn, qn, w)

# GB2 indicators
  indicN <- main.gb2(0.6, an, bn, pn, qn)

type=c("Emp. est", "NLS")
  results <- data.frame(type=type,
             median=c(med,indicN[1]),
             ARPR=c(indicE[1],indicN[3]),
             RMPG=c(indicE[2],indicN[4]),
             QSR=c(indicE[3],indicN[5]),
             GINI=c(indicE[4],indicN[6]),
             likelihood=c(0,nlik),
             a=c(0,an), b=c(0,bn), p=c(0,pn), q=c(0,qn))

return(list(data.frame(results), fit1, fit2))
}


