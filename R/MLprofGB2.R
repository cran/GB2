
# Maximum likelihood based on the profile log-likelihood

profml.gb2 <- function(z, w=1, method=1){

fnp <- function(x, z, w){
	a <- x[1]
	b <- x[2]
return(-proflogl.gb2(z, a, b, w))
}

grp <- function(x, z, w){
	a <- x[1]
	b <- x[2]
return(-profscores.gb2(z, a, b, w))
}

# Initial values of a and b under Fisk
x0 <- fisk(z, w)[1:2]  

#print("opt1")
opt1 <- optim(x0, fnp, grp, z, w, method="BFGS", control=list(parscale=x0,pgtol=1e-16), hessian=TRUE)
if (method != 2) return(list(opt1=opt1))
if (method == 2){
#print("opt2")
opt2 <- optim(x0, fnp, grp, z, w, method="L-BFGS-B", lower=0, control=list(parscale=x0,pgtol=0), hessian=TRUE)
return(list(opt1=opt1,opt2=opt2))
	              }
}
