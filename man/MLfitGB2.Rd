\name{MLfitGB2}
\Rdversion{1.1}
\alias{MLfitGB2}
\alias{main.emp}
\alias{mlfit.gb2}
\title{
Fitting the GB2 by the Method of Maximum Likelihood Estimation
}
\description{
Performs maximum likelihood estimation through the general-purpose optimisation function \code{optim} from package \code{stats}.
}
\usage{
main.emp(z, w=1)
mlfit.gb2(z, w=1) 
}
\arguments{
  \item{z}{a numeric vector; in general, the income values.}
  \item{w}{a numeric vector of the same length as \code{z}; the sampling weights. If not available, the function should be called only with its first argument (the weights are set to 1).}
}
\details{
The function makes a call to \code{ml.gb2} and \code{profml.gb2}. Estimates of the GB2 parameters are calculated .
Also the primary Laeken indicators of the fitted parameters and the empirical estimates of the indicators (see package \code{laeken}) are calculated.
}
\value{
A list containing three different objects. The first is a data frame with the values of the fitted parameters for the full log-likelihood and the profile log-likelihood, the values of the two likelihoods, 
the values of the estimated indicators under the GB2 and the values of the empirical estimates of the indicators. The second and third objects are, respectively, the fit using the full log-likelihood and the fit using
the profile log-likelihhod.
}
\seealso{
\code{\link[stats]{optim}} , \code{\link{ml.gb2}}, \code{\link{profml.gb2}}
}
\author{
Monique Graf and Desislava Nedyalkova
}
\examples{
# An example of using the function mlfit.gb2
# See also the examples of ml.gb2 and mlprof.gb2

\dontrun{
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

# ML fit
m1 <- mlfit.gb2(inc,w)

# Results
m1[[1]]
# The fit using the full log-likelihood
m1[[2]]
# The fit using the profile log-likelihood
m1[[3]]

# ML fit, when no weights are avalable 
m2 <- mlfit.gb2(inc)
# Results
m2[[1]]
}
}
\keyword{distribution}

