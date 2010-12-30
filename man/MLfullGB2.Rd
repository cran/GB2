\name{MLfullGB2}
\Rdversion{1.1}
\alias{MLfullGB2}
\alias{fisk}
\alias{fiskh}
\alias{ml.gb2}
\alias{mlh.gb2}
\arguments{
  \item{z}{vector of data values.}
  \item{w}{corresponding weights.}
  \item{hs}{vector of household sizes.}
  \item{method}{the method to be used for \code{optim}. By default, the used method is BFGS. If \code{method = }2, methods BFGS and L-BFGS-B are used.}
}
\title{
Maximum Likelihood Estimation of the GB2 Based on the Full Log-likelihood
}
\description{
Two methods of optimization are considered: BFGS and L-BFGS-B (see \code{optim} documentation for more details). Initial values of the parameters to be optimized over (\eqn{a}, \eqn{b}, 
\eqn{p} and \eqn{q}) are given from the Fisk distribution, which is a GB2 distribution with \eqn{p = q = 1}.  If \eqn{m} and \eqn{v} denote, respectively, the mean and variance of \eqn{log(z)}, 
then \eqn{\hat{a} = \pi/\sqrt{3*v}} and \eqn{\hat{b} = \exp(m)}.  
The function to be maximized by \code{optim} returns the negative of the full log-likelihood and the gradient is equal to the negative of the scores, respectively for the case of a sample of persons and a sample of households. 
}
\usage{
fisk(z, w=1)
fiskh(z, w=1, hs)
ml.gb2(z, w=1, method=1) 
mlh.gb2(z, w=1, hs, method=1)
}
\details{
Function \code{fisk} first calculates the mean and variance of \eqn{log(z)} and returns the initial values of \code{a} and \code{p} under the Fisk distribution.  
Function \code{fiskh} first calculates the mean and variance of \eqn{log(z)}, assuming a sample of households, and returns the initial values of \code{a} and \code{p} under the Fisk distribution. 
Function \code{ml.gb2} performs maximum likelihood estimation through the general-purpose optimization function \code{optim} from package \code{stats},
based on the full log-likelihood calculated in a sample of persons. Function \code{mlh.gb2} performs maximum likelihood estimation through the general-purpose optimization function \code{optim} 
from package \code{stats}, based on the full log-likelihood calculated in a sample of households.

}
\value{
A list with 1 or 2 arguments: \code{opt1} output of BFGS fit and \code{opt2} output of L-BFGS fit. Further values are given by the values of \code{optim}.
}
\references{
 Graf, M. and Nedyalkova, D. (2010)
 The Generalized Beta Distribution of the Second Kind as an Income Distribution: Properties, Likelihood and Estimation.
 \emph{working paper}, SFSO.
}
\seealso{\code{\link{optim}} for the general-purpose optimization
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
d <- data.frame(inc, w)
d <- d[!is.na(d$inc),]
   
# Truncate at 0
inc <- d$inc[d$inc > 0]
w   <- d$w[d$inc > 0]

# Fit using the full log-likelihood
fitf <- ml.gb2(inc, w)$opt1

# Fitted GB2 parameters
af <- fitf$par[1]
bf <- fitf$par[2]
pf <- fitf$par[3]
qf <- fitf$par[4]

# Likelihood
flik <- fitf$value

# If we want to compare the indicators
\dontrun{
# GB2 indicators
indicf <- round(main.gb2(0.6,af,bf,pf,qf), digits=3)
# Empirical indicators
indice <- round(main.emp(inc,w), digits=3)
}
# Plots
\dontrun{plotsML.gb2(inc,af,bf,pf,qf,w)}
}
\keyword{distribution}

