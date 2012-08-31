\name{CompoundFitAux}
\Rdversion{1.1}
\alias{CompoundFitAux}
\alias{pkl.cavgb2}
\alias{lambda0.cavgb2}
\alias{logl.cavgb2}
\alias{scores.cavgb2}
\alias{ml.cavgb2}
\title{
Fitting the Compound Distribution based on the GB2 by the Method of Maximum Likelihood Estimation using Auxiliary Information
}
\description{
Calculates the log-likelihood, the score functions of the log-likelihood and fits the compound distribution based on the GB2 and using auxiliary information.
}
\usage{
pkl.cavgb2(z, lambda)
lambda0.cavgb2(pl0, z, w=rep(1, dim(z)[1]))
logl.cavgb2(fac, z, lambda, w=rep(1, dim(fac)[1]))
scores.cavgb2(fac, z, lambda, w=rep(1, dim(fac)[1]))
ml.cavgb2(fac, z, lambda0, w=rep(1, dim(fac)[1]), maxiter=100) 
}
\arguments{
  \item{z}{numeric; a matrix of auxiliary variables. Should not contain missing values.}
  \item{lambda}{numeric; a matrix of parameters.}
  \item{lambda0}{numeric; a matrix of initial parameters. }
  \item{pl0}{numeric; a vector of initial proportions defining the number of components and the weight of each component density in the decomposition. Sums to one.}
  \item{fac}{numeric; a matrix of Gamma factors.}
  \item{w}{numeric; vector of weights of length the number of rows of the matrix \code{fac}. By default \code{w} is a vector of 1. Should not contain missing values.}
  \item{maxiter}{numeric; maximum number of iterations to perform. By default \code{maxiter} = 100.}

}
\details{
We model the probabilities \eqn{p_\ell} with auxiliary variables. Let \eqn{z_k} denote the vector of auxiliary information for unit \eqn{k}. This auxiliary information modifies the probabilities \eqn{p_\ell} at the unit level. Let denote by \eqn{p_{k,\ell}} the weight of the density \eqn{f_\ell} for unit \eqn{k}. For \eqn{\ell=1,...,L-1}, we pose a linear model for \eqn{v_{k,\ell}}:
\deqn{\log(p_{k,\ell}/p_{k,L}) = v_{k,\ell} =\sum_{i=1}^I \lambda_{\ell i} z_{k i}= \mathbf{z}_k^{T} \boldsymbol{\lambda_{\ell}}.} 
Function \code{pkl.cavgb2} calculates the \eqn{p_{k,\ell}}. Function \code{lambda0.cavgb2} calculates the initial values \eqn{\lambda_{\ell i}}, \eqn{i= 1, ..., I}, \eqn{\ell = 1, ..., L-1} . Let \deqn{\bar{z}_{i}=\sum_k w_k z_{ki}/\sum_k w_k} be the mean value of the \eqn{i}-th explanatory variable. 
Writing \deqn{\log(\hat{p}^{(0)}_\ell / \hat{p}^{(0)}_L)=v^{(0)}_\ell = \sum_{i=1}^I \lambda^{(0)}_{\ell i} \bar{z}_{i},} we can choose \eqn{\lambda^{(0)}_{\ell i}= v^{(0)}_\ell / (I \bar{z}_{i}).} Analogically to the ordinary fit of the compound distribution based on the GB2 \code{\link{CompoundFit}}, we express the log-likelihood as a weighted mean of \eqn{log(f) = log(\sum(p_{k,\ell} f_\ell(x_k))}, evaluated at the data points, where \eqn{f} is the GB2 compound density. The scores are obtained as weighted sums of the first derivatives of the log-likelihood, with respect to the parameters \eqn{\lambda_\ell, \ \ell=1, ..., L-1}, evaluated at the data points.  Function \code{ml.cavgb2} performs maximum likelihood estimation through the general-purpose optimization function \code{optim} from package \code{stats}. The considered method of optimization is "BFGS" (\code{\link{optim}}). Once we have the fitted parameters \eqn{\hat{\lambda}} we can obtain average fitted parameters \eqn{\hat{\bar{v}}_{\ell}} and \eqn{\hat{\bar{p}_{\ell}}} in function of \eqn{\bar{z}} and \eqn{\hat{\lambda}_{\ell}}.
}
\value{
\code{pkl.cavgb2} returns a matrix of probabilities. \code{lambda0.cavgb2} returns a matrix of size \eqn{I \times L-1}. \code{logl.cavgb2} returns the value of the pseudo log-likelihood. \code{scores.cavgb2} return a vector of the scores of length \eqn{(L-1)I}. \code{ml.cavgb2} returns a list containing two objects - the vector of fitted coefficients \eqn{\hat{\lambda_\ell}} and the output of the "BFGS" fit.
}
\seealso{
\code{\link[stats]{optim}}
}
\author{
Monique Graf and Desislava Nedyalkova
}
\examples{
\dontrun{

library(simFrame)
data(eusilcP)

# Stratified cluster sampling
set.seed(12345)
srss <- SampleControl(design = "region", grouping = "hid", 
  size = c(200*3, 1095*3, 1390*3, 425*3, 820*3, 990*3, 400*3, 450*3, 230*3), k = 1)

# Draw a sample
s1 <- draw(eusilcP,srss)
#names(s1)

# Creation of auxiliary variables
ind <- order(s1[["hid"]])
ss1 <- data.frame(hid=s1[["hid"]], region=s1[["region"]], hsize=s1[["hsize"]], 
  peqInc=s1[["eqIncome"]], age=s1[["age"]], pw=s1[[".weight"]])[ind,]

ss1[["child"]] <- as.numeric((ss1[["age"]]<=14))
ss1[["adult"]] <- as.numeric((ss1[["age"]]>=20))
sa <- aggregate(ss1[,c("child","adult")],list(ss1[["hid"]]),sum)
names(sa)[1] <- "hid"
sa[["children"]] <- as.numeric((sa[["child"]]>0))
sa[["single_a"]] <- as.numeric((sa[["adult"]]==1))
# Single adult with children
sa[["sa.ch"]] <- sa[["single_a"]]*sa[["children"]]
# 2 or more adults with children
sa[["ma.ch"]] <- (1-sa[["single_a"]])*sa[["children"]]
sa[["nochild"]] <- 1-sa[["children"]]

# New data set
ns <- merge(ss1[,c("hid","region","hsize","peqInc","pw")], 
      sa[,c("hid","nochild","sa.ch","ma.ch")], by="hid")

# Ordering the data set
index <- order(ns$peqInc)
ns <- ns[index,]

# 	Truncate at 0
ns <- ns[ns$peqInc>0,]

# income
peqInc <- ns$peqInc
# weights
pw <- ns$pw             

# Adding the weight adjustment
c1 <- 0.1                                
pwa <- robwts(peqInc,pw,c1,0.001)[[1]]        
corr <- mean(pw)/mean(pwa)              
pwa <- pwa*corr  

ns <- data.frame(ns, aw=pwa) 

# Matrix of auxiliary variables
z <- ns[,c("nochild","sa.ch","ma.ch")]
z <- as.matrix(z)

# global GB2 fit, ML profile log-likelihood
gl.fit <- profml.gb2(peqInc,pwa)$opt1
agl.fit <- gl.fit$par[1]
bgl.fit <- gl.fit$par[2]
pgl.fit <- prof.gb2(peqInc,agl.fit,bgl.fit,pwa)[3]
qgl.fit <- prof.gb2(peqInc,agl.fit,bgl.fit,pwa)[4]

# Initial lambda and pl
pl0 <- c(0.2,0.6,0.2)
lambda0 <- lambda0.cavgb2(pl0, z, pwa)

# left decomposition
decomp <- "l"
facgl <- fg.cgb2(peqInc, agl.fit, bgl.fit, pgl.fit, qgl.fit, pl0 ,decomp)
fitcml <- ml.cavgb2(facgl, z, lambda0, pwa, maxiter=500) 
fitcml
lambdafitl <- fitcml[[1]]
pglfitl <-  pkl.cavgb2(diag(rep(1,3),lambdafitl)
row.names(pglfitl) <- colnames(z)
}
}
\keyword{distribution}


