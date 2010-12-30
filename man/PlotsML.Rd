\name{PlotsML}
\Rdversion{1.1}
\alias{PlotsML}
\alias{plotsML.gb2}
\alias{saveplot}
\title{
Cumulative Distribution Plot and Kernel Density Plot for the Fitted GB2
}
\description{
Function \code{plotsML.gb2} produces two plots. The first is a plot of the empirical cumulative distribution function versus the fitted cumulative distibution function.  
The second is a plot of the kernel density versus the fitted GB2 density.  Function \code{saveplot} saves locally the produced plot.
}
\usage{
plotsML.gb2(z, shape1, scale, shape2, shape3, w=1) 
saveplot(name, pathout)
}
\arguments{
  \item{z}{a numeric vector; in general, a vector of income values.}
  \item{shape1}{positive parameter.}
  \item{scale}{positive parameter.}
  \item{shape2, shape3}{positive parameters of the Beta distribution.}
  \item{w}{a numeric vector of the same length as \code{z}; the sampling weights. If not available, the function should be called only with its first argument (the weights are set to 1).}
  \item{name}{a character string specifying the name of the plot.}
  \item{pathout}{a character string specifying the path  of the folder or device where the plot will be saved.}
}
\details{
The used kernel is "Epanechnikov" (see \code{\link{plot.density}}).  
}
\author{
Monique Graf and Desislava Nedyalkova
}
\keyword{distribution}

