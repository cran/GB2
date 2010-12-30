\name{Contprof}
\Rdversion{1.1}
\alias{Contprof}
\alias{contprof.gb2}
\title{
Contour Plot of the Profile Log-likelihood of the GB2 Distribution
}
\usage{
contprof.gb2(z, w=1, resol, low=0.1, high=20)
}
\arguments{
  \item{z}{a numeric vector; in general, the income values.}
  \item{w}{a numeric vector of the same length as \code{z}; the sampling weights. If not available, the function should be called only with its first argument (the weights are set to 1).}
  \item{resol}{a scalar; number of grid points horizontally and vertically. For better graph quality, we recommend a value of 100.}
  \item{low, high}{scalar; lower and upper factors for scale.}
}
\description{Produces a contour plot of the profile log-likelihood, which is a function of two parameters only.
}

\details{The matrix containing the values to be plotted (NAs are allowed) is of size \code{resol} \eqn{\times} \code{resol}.  
The locations of the grid lines at which the values of the profile log-likelihood are measured are equally-spaced values between \code{low} and \code{high} 
multiplied by the initial parameters.
}
\value{
A contour plot of the profile log-likelihood. The initial Fisk estimate is added as point "F".
}
\seealso{\code{\link{fisk}} for the Fisk estimate, \code{\link{ProfLogLikelihood}} for the profile log-likelihood and \code{\link{contour}} (package \code{graphics}) for more details on contour plots.
}
\author{
Monique Graf
}
\keyword{distribution}

