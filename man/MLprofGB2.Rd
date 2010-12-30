\name{MLprofGB2}
\Rdversion{1.1}
\alias{MLprofGB2}
\alias{profml.gb2}
\title{
Maximum Likelihood Estimation of the GB2 Based on the Profile Log-likelihood
}
\description{
\code{profml.gb2} performs maximum likelihood estimation based on the profile log-likelihood through the general-purpose optimization function 
\code{optim} from package \code{stats}.     
}
\usage{
profml.gb2(z, w=1, method=1) 
}
\arguments{
  \item{z}{vector of data values.}
  \item{w}{corresponding weights.}
  \item{method}{the method to be used for \code{optim}. By default, the used method is BFGS. If \code{method = }2, methods BFGS and L-BFGS-B are used.}
}
\details{
Two methods are considered: BFGS and L-BFGS-B (see \code{optim} documentation for more details). Initial values of the parameters to be optimized over (\eqn{a} and \eqn{b}) 
are given from the Fisk distribution.  The function to be maximized by \code{optim} is the negative of the profile log-likelihood (\code{\link{proflogl.gb2}}) 
and the gradient is equal to the negative of the scores (\code{\link{profscores.gb2}}).
}
\value{
A list with 1 or 2 arguments: \code{opt1} output of BFGS fit and \code{opt2} output of L-BFGS fit. Further values are given by the values of \code{\link{optim}}.
}
\references{
 Graf, M. and Nedyalkova, D. (2010)
 The Generalized Beta Distribution of the Second Kind as an Income Distribution: Properties, Likelihood and Estimation.
 \emph{working paper}, SFSO.
}
\seealso{\code{\link{optim}} for the general-purpose optimization and \code{link{ml.gb2}} for the full log-likelihood.
}
\author{
Monique Graf
}
\examples{
library(laeken)
data(eusilc)

# Income
inc <- as.vector(eusilc$eqIncome)

# Weights
w <- eusilc$rb050

# Data set
d <- data.frame(inc,w)
d <- d[!is.na(d$inc),]
   
# Truncate at 0
inc <- d$inc[d$inc > 0]
w   <- d$w[d$inc > 0]

# Fit using the profile log-likelihood
fitp <- profml.gb2(inc, w)$opt1

# Fitted GB2 parameters
ap <- fitp$par[1]
bp <- fitp$par[2]
pp <- prof.gb2(inc, ap, bp, w)[3]
qp <- prof.gb2(inc, ap, bp, w)[4]

# Profile log-likelihood
proflik <- fitp$value

# If we want to compare the indicators
\dontrun{
# GB2 indicators
indicp <- round(main.gb2(0.6,ap,bp,pp,qp), digits=3)
# Empirical indicators
indice <- round(main.emp(inc,w), digits=3)
}

# Plots
\dontrun{plotsML.gb2(inc,ap,bp,pp,qp,w)}
}
\keyword{distribution}

