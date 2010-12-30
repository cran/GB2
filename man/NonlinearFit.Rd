\name{NonlinearFit}
\Rdversion{1.1}
\alias{NonlinearFit}
\alias{nlsfit.gb2}
\alias{nlsfit2.gb2}
\title{
Fitting the GB2 by Minimizing the Distance Between a Set of Indicators and Their GB2 Expressions
}
\description{
Fitting the parameters of the GB2 distribution by optimizing the squared weighted distance between a set of empirical indicators and the GB2 indicators using nonlinear least squares 
(function \code{nls} from package \code{stats}).
}
\usage{
nlsfit.gb2(z, w=1) 
nlsfit2.gb2(z, w=1, a.fit, b.fit, p.fit, q.fit, cva.fit) 
}
\arguments{
  \item{z}{a numeric vector; in general, the income values.}
  \item{w}{a numeric vector of the same length as \code{z}; the sampling weights. If not available, the function should be called only with its first argument (the weights are set to 1).}
  \item{a.fit, b.fit, p.fit, q.fit}{fitted parameters, e.g. my maximum likelihood estimation, of the GB2 distribution.}
  \item{cva.fit}{coefficient of variation of the fitted parameter \eqn{a.fit}.}
}
\details{
 We consider the following set of indicators: \deqn{A = (median, ARPR, RMPG, QSR, Gini)} and their corresponding GB2 expressions \eqn{A_{GB2}(a,b,p,q)}.  The nonlinear model that is passed to \code{nls} 
 in the function \code{nlsfit.gb2} is given by:
  \deqn{\sum_{i=1}^5 c_i \left(A_{empir,i}-A_{GB2,i}(a,b,p,q)\right)^2,} where the weights \eqn{c_i} take the differing scales into account.  Initial values for the parameters are taken from the Fisk distribution.  
  Estimates of the GB2 parameters are calculated by using the generic function \code{\link{coef}} on the fitted model object.
 The second function, \code{mod_nlsfit.gb2}, fits the parameters of the GB2 in two consecutive steps.  
 In the first step, we use the set of indicators, excluding the median, and their corresponding expressions in function of \code{a}, \code{ap} and \code{aq}.
 The lower and upper bounds for the algorithm "port" of \code{nls} are defined.  Starting values are the ML estimates of the GB2 parameters \code{a, p} and \code{q}.  
 The bounds for \code{a} are defined in function of the coefficient of variation of the fitted parameter \code{a.fit}.  
 \code{ap} and \code{aq} are bounded so that the constraints for the existence of the moments of the GB2 distribution and the excess for calculating the Gini coefficient are fulfilled, 
 i.e. \eqn{ap \ge 1} and \eqn{aq \ge 2}.  In the second step, only the the parameter \code{b} is estimated, optimizing the weighted square difference between the empirical median and the GB2 median in function of the already obtained NLS parameters 
 \code{a, p} and \code{q}. 
}
\value{\code{nlsfit.gb2} returns a list of two values: 1) a data frame containing the values of the fitted GB2 parameters, the values of the estimated GB2 indicators and the values of the empirical estimates of the indicators
 and 2) the fitted object. \code{nlsfit2.gb2} returns a list of three values: 1) a data frame containing the values of the fitted GB2 parameters, the values of the estimated GB2 indicators and the values of the empirical estimates of the indicators, 
 2) the first fitted object and 3) the second fitted object.
}
\seealso{
\code{\link[stats]{nls}}, \code{\link{Thomae}}, \code{\link{moment.gb2}}
}
\author{
Monique Graf and Desislava Nedyalkova
}
\examples{
# Takes long time to run, as it makes a call to the function ml.gb2
\dontrun{
library(laeken)
data(eusilc)

# Income
inc <- as.vector(eusilc$eqIncome)

# Weights
w <- eusilc$rb050

# Household size
hs <- eusilc$hsize

# Data set
d <- data.frame(inc, w, hs)
d <- d[!is.na(d$inc),]
   
# Truncate at 0
d <- d[d$inc > 0,]
inc <- d$inc
w   <- d$w

# ML fit, full log-likelihood
fitf <- ml.gb2(inc, w)$opt1

# Estimated parameters
af <- fitf$par[1]
bf <- fitf$par[2]
pf <- fitf$par[3]
qf <- fitf$par[4]
apf <- af*pf
aqf <- af*qf
gb2.par <- c(af, bf, pf, qf)

# GB2 indicators
indicm <- round(main.gb2(0.6,af,bf,pf,qf), digits=3)

# Empirical indicators
indice <- round(main.emp(inc,w), digits=3)

# NLS fit 1
n1 <- nlsfit.gb2(inc,w)
n1[[1]]

# Scores (partial derivatives of the log-likelihood with respect to the GB2 parameters)
scores <- matrix(nrow=length(inc), ncol=4)
for (i in 1:length(inc)){
scores[i,] <- dlogf.gb2(inc[i], af ,bf, pf, qf)
}

# Data on households only
dh <- unique(d)
hinc <- dh$inc
hw <- dh$w
hs <- dh$hs 

# Estimated variance-covariance matrix of af, bf, pf and qf (EVCM)
VSC <- varscore.gb2(hinc,af,bf,pf,qf,hw,hs)
VCMP <- vepar.gb2(hinc,VSC,af,bf,pf,qf,hw,hs)[[1]]

# Standard errors of af, bf, ...
se.par <- rep(0,4)
for (i in 1:4){
se.par[i] <- sqrt(VCMP[i,i])
}

# Coefficients of variation of the fitted parameters
cv.par <- se.par/gb2.par
cvaf <- cv.par[1]

# NLS fit 2
n2 <- nlsfit2.gb2(inc, w, af, bf, pf, qf, cvaf)
n2[[1]]
}
}
\keyword{distribution}

