\name{Thomae}
\Rdversion{1.1}
\alias{ULg}
\alias{combiopt}
\alias{Thomae}
\alias{gb2.gini}
\title{Maximum Excess Representation of a Generalized Hypergeometric Function Using Thomae's Theorem}
\description{
Defines Thomae's arguments from the upper(\code{U}) and lower(\code{L}) parameters of a \eqn{_{3}F_{2}(U,L;1)}. Computes the optimal combination leading to the maximum excess. 
Computes the optimal combination of Thomae's arguments and calculates the optimal representation of the \eqn{_{3}F_{2}(U,L;1)} using the \code{genhypergeo_series} function from package \code{hypergeo}. 
Computes the Gini coefficient for the GB2 distribution, using Thomae's theorem.
}
\usage{
ULg(U, L)
combiopt(g)
Thomae(U, L, lB, tol, maxiter, debug)
gb2.gini(shape1, shape2, shape3, tol=1e-08, maxiter=10000, debug=FALSE)
}
\arguments{
  \item{U}{vector of length 3 giving the upper arguments of the generalized hypergeometric function \eqn{_{3}F_{2}}.}
  \item{L}{vector of length 2 giving the lower arguments of the generalized hypergeometric function \eqn{_{3}F_{2}}.}
  \item{g}{vector of Thomae's permuting arguments.}
  \item{lB}{ratio of beta functions (a common factor in the expression of the Gini coefficient under the GB2).}
  \item{shape1}{positive parameter.}
  \item{shape2, shape3}{positive parameters of the Beta distribution.}
  \item{tol}{tolerance with default 0, meaning to iterate until additional terms do not change the partial sum.}
  \item{maxiter}{maximum number of iterations to perform.}
  \item{debug}{boolean; if \code{TRUE}, returns the list of changes to the partial sum.}
}
\details{
More details can be found in Graf (2009). 
}
\value{
\code{ULg} returns a list containing Thomae's arguments and the excess, \code{combiopt} gives the optimal combination of Thomae's arguments (for internal use only), 
\code{Thomae} returns the optimal representation of the \eqn{_{3}F_{2}(U,L;1)}, \code{GB2.Gini} returns the Gini coefficient under the GB2.
}
\references{
 Graf, M. (2009)
 An Efficient Algorithm for the Computation of the Gini coefficient of the Generalized Beta Distribution of the Second Kind. 
 \emph{ASA Proceedings of the Joint Statistical Meetings}, 4835--4843.
 American Statistical Association (Alexandria, VA). 

 \emph{In Proceedings of JSM 2009}.

 McDonald, J. B. (1984)
 Some generalized functions for the size distribution of income. 
 \emph{Econometrica}, \bold{52}, 647--663.
}
\author{Monique Graf}
\seealso{
\code{\link[hypergeo]{genhypergeo_series}}
}
\keyword{distribution}

