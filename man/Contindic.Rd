\name{Contindic}
\Rdversion{1.1}
\alias{Contindic}
\alias{contindic.gb2}
\title{
Sensitivity Analysis of Laeken Indicators on GB2 Parameters
}
\usage{
contindic.gb2(resol, a, p1, p2, q1, q2, fn, tit, table=FALSE)
}
\arguments{
  \item{resol}{numeric; number of grid points horizontally and vertically.}
  \item{a}{numeric; positive parameter, first shape parameter of the GB2 distribution.} 
  \item{p1, p2, q1, q2}{numeric; limits on the positive parameters of the Beta distribution.}
  \item{fn}{string; the name of the function to be used for the calculation of the values to be plotted.}
  \item{tit}{string; title of the plot.}
  \item{table}{boolean; if \code{TRUE}, a table containing the values of the function \code{fn} at the different grid points is printed.}
}
\description{Produces a contour plot of an indicator.
}
\details{An indicator is defined as a function of three parameters. The shape parameter, \code{a}, is held fixed. The shape parameters \code{p} and \code{q} vary between \code{p1} and \code{p2}, and \code{q1} and \code{q2}, respectively.
}
\value{
A contour plot of a given indicator for a fixed value of the shape parameter \code{a}.
}
\seealso{\code{\link{contour}} (package \code{graphics}) for more details on contour plots.
}
\author{
Monique Graf
}
\examples{
par(mfrow=c(2,2))
p1 <- 0.3
q1 <- 0.36
p2 <- 1.5
q2 <- 1.5
a1 <- 2.7
a2 <- 9.2
resol <- 11
rangea <- round(seq(a1,a2,length.out=4),digits=1)
arpr <- function(a,p,q) 100*arpr.gb2(0.6,a,p,q)
fonc <- "arpr"
for (a in rangea){
contindic.gb2(resol,a,p1,p2,q1,q2,arpr,"At-risk-of-poverty rate",table=TRUE)
}
}
\keyword{distribution}

