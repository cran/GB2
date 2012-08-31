\name{CompoundIndicators}
\Rdversion{1.1}
\alias{CompoundIndicators}
\alias{arpt.cgb2}
\alias{arpr.cgb2}
\alias{rmpg.cgb2}
\alias{qsr.cgb2}
\title{Indicators of Poverty and Social Exclusion under the Compound Distribution based on the GB2
}
\description{
Functions to calculate four primary social welfare indicators under the compound GB2 distribution, i.e. the at-risk-of-poverty threshold,
 the at-risk-of-poverty rate, the relative median at-risk-of-poverty gap, and the income quintile share ratio.
}
\usage{
arpt.cgb2(prop, shape1, scale, shape2, shape3, pl0, pp, decomp="r")
arpr.cgb2(prop, shape1, shape2, shape3, pl0, pp, decomp="r")
rmpg.cgb2(arpr, shape1, shape2, shape3, pl0, pp, decomp="r")
qsr.cgb2(shape1, shape2, shape3, pl0, pp, decomp="r")
}
\arguments{
  \item{prop}{numeric; proportion (in general is set to 0.6).}
  \item{arpr}{numeric; the value of the at-risk-of-poverty rate.}
  \item{shape1,scale,shape2,shape3}{numeric; positive parameters of the GB2 distribution.}
  \item{pl0}{numeric; a vector of initial proportions defining the number of components and the weight of each component density in the decomposition. Sums to one.}
  \item{pp}{numeric; a vector of fitted proportions. Sums to one. If \eqn{pp=pl0} we obtain the GB2 distribution.}
  \item{decomp}{string; specifying if the decomposition of the GB2 is done with respect to the right tail ("r") or the left tail ("l") of the distribution.
   By default, \code{decomp} = "r" - right tail decomposition.}
}
\details{
The four indicators are described in details in the case of the GB2. The difference here is that we need to give an initial vector of proportions, fitted proportions and define for which decomposition (left or right) the indicators should be calculated.
}
\value{
\code{arpt.cgb2} gives the ARPT, \code{arpr.cgb2} the ARPR, \code{rmpg.cgb2} the RMPG, and \code{qsr.cgb2} gives the QSR.
}
\author{
Monique Graf
}
\references{
 Graf, M., Nedyalkova, D., Münnich, R., Seger, J. and Zins, S. (2011)
 AMELI Deliverable 2.1: Parametric Estimation of Income Distributions and
 Indicators of Poverty and Social Exclusion.
 \emph{Technical report}, AMELI-Project.
}
\seealso{
\code{arpr.gb2} for details on the welfare indicators under the GB2.
}
\examples{
# GB2 parameters
a <- 3.9
b <- 18873
p <- 0.97
q <- 1.03

# initial proportions
p0 <- rep(1/3,3)

# fitted proportions
p1 <- c(0.39,0.26,0.35)

# for the right decomposition
arpt <- arpt.cgb2(0.6, a, b, p, q, p0, p1)
arpr <- arpr.cgb2(0.6, a, p, q, p0, p1)
\dontrun{rmpg <- rmpg.cgb2(arpr, a, p, q, p0, p1)}
\dontrun{qsr <- qsr.cgb2(a, p, q, p0, p1)}

# for the left ddecomposition
arptleft <- arpt.cgb2(0.6, a, b, p, q, p0, p1, "l")
}
\keyword{distribution}
\encoding{latin1}


