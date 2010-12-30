\name{LogLikelihood}
\Rdversion{1.1}
\alias{LogLikelihood}
\alias{loglp.gb2}
\alias{loglh.gb2}
\alias{scoresp.gb2}
\alias{scoresh.gb2}
\alias{info.gb2}
\title{
Full Log-likelihood of the GB2 Distribution
}
\description{
Calculates the log-likelihood, the score functions of the log-likelihood and the Fisher information matrix based on all four parameters of the GB2 distribution.
}
\usage{
loglp.gb2(x, shape1, scale, shape2, shape3, w=1)
loglh.gb2(x, shape1, scale, shape2, shape3, w=1, hs)
scoresp.gb2(x, shape1, scale, shape2, shape3, w=1)
scoresh.gb2(x, shape1, scale, shape2, shape3, w=1, hs)
info.gb2(shape1, scale, shape2, shape3)
}
\arguments{
  \item{x}{vector of data values.}
  \item{shape1}{positive parameter.}
  \item{scale}{positive parameter.}
  \item{shape2, shape3}{positive parameters of the Beta distribution.}
  \item{w}{vector of weights.}
  \item{hs}{vector of household sizes.}
}
\details{
We express the log-likelihood as a weighted mean of \eqn{log(f)}, evaluated at the data points, where \eqn{f} is the GB2 density with parameters \code{shape1} \eqn{= a}, \code{scale} \eqn{= b}, 
\code{shape2} \eqn{= p} and \code{shape3} \eqn{= q}.  If the weights are not available, then we suppose that \code{w} \eqn{= 1}. \code{loglp.gb2} calculates the log-likelihood in the case where the data is a sample of persons and 
\code{loglh.gb2} is adapted for a sample of households.  Idem for the scores, which are obtained as weighted sums of the first derivatives of \eqn{log(f)} with respect to the GB2 parameters, evaluated at the data points.  
The Fisher information matrix of the GB2 was obtained by Brazauskas (2002) and is expressed in terms of the second derivatives of the log-likelihood with respect to the parameters of the GB2.
}
\references{
 Brazauskas, V. (2002)
 Fisher information matrix for the Feller-Pareto distribution.
 \emph{Statistics \& Probability Letters}, \bold{59}, 159--167.
 
 Kleiber, C. and Kotz, S. (2003) 
 \emph{Statistical Size Distributions in Economics and Actuarial Sciences}, chapter 6.
 Wiley, Ney York.
}
\author{
Monique Graf
}
\keyword{distribution}

