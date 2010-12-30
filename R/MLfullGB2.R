


fisk <- function(z, w=1){

wmv <- function(z, w){
# weighted mean and variance of log(z)
  n <- sum(w)
  if (length(w)==1) n <- length(z)
	logz <- log(z)
	mean <- sum(w*logz)/n
	var <- sum(w*(logz-mean)^2)/n
	return(list(mean,var))
}

ab0 <- wmv(z, w)
# Initial values under Fisk
x0 <- c(pi/sqrt(3*ab0[[2]]),exp(ab0[[1]]),1,1)
return(x0)
}

fiskh <- function(z, w=1, hs){

wmv <- function(z, w, hs){
# weighted mean and variance of log(z)
  sw <- sum(w*hs)
  if (length(w)==1) sw <- length(z*hs)
	logz <- log(z)
	mean <- sum(w*hs*logz)/sw
	var <- sum(w*hs*(logz-mean)^2)/sw
	return(list(mean,var))
}

ab0 <- wmv(z, w, hs)
# Initial values under Fisk
x0 <- c(pi/sqrt(3*ab0[[2]]),exp(ab0[[1]]),1,1)
return(x0)
}

# Maximum likelihood based on the full log-likelihood (personal level)
ml.gb2 <- function(z, w = 1, method = 1){

fn <- function(x, z, w){
# function to be evaluated by optim
	a <- x[1]
	b <- x[2]
	p <- x[3]
	q <- x[4]
	return(-loglp.gb2(z, a, b, p, q, w))
}

gr <- function(x, z, w){
# gradient 
	a <- x[1]
	b <- x[2]
	p <- x[3]
	q <- x[4]
	return(-scoresp.gb2(z, a, b, p, q, w))
}

# Initial values under Fisk
x0 <- fisk(z, w)  

#print("opt1")
opt1 <- optim(x0, fn, gr, z, w, method="BFGS", control=list(parscale=x0,pgtol=1e-8), hessian=FALSE)
if (method != 2) return(list(opt1 = opt1))
if (method == 2){
#print("opt2")
opt2 <- optim(x0, fn, gr, z, w, method="L-BFGS-B", lower=0, control=list(parscale=x0,pgtol=1e-8), hessian=FALSE)
return(list(opt1 = opt1, opt2 = opt2))
	              }
}

# Maximum likelihood based on the full log-likelihood (household level)
mlh.gb2 <- function(z, w = 1, hs, method = 1){

fn <- function(x, z, w, hs){
# function to be evaluated by optim
	a <- x[1]
	b <- x[2]
	p <- x[3]
	q <- x[4]
	return(-loglh.gb2(z, a, b, p, q, w, hs))
}

gr <- function(x, z, w, hs){
# gradient 
	a <- x[1]
	b <- x[2]
	p <- x[3]
	q <- x[4]
	return(-scoresh.gb2(z, a, b, p, q, w, hs))
}

# Initial values under Fisk
x0 <- fiskh(z, w, hs)  

#print("opt1")
opt1 <- optim(x0, fn, gr, z, w, hs, method="BFGS", control=list(parscale=x0,pgtol=1e-8), hessian=TRUE)
if (method != 2) return(list(opt1 = opt1))
if (method == 2){
#print("opt2")
opt2 <- optim(x0, fn, gr, z, w, hs, method="L-BFGS-B", lower=0, control=list(parscale=x0,pgtol=1e-8), hessian=TRUE)
return(list(opt1 = opt1, opt2 = opt2))
	              }
}