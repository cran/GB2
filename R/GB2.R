
dgb2 <- function(x,shape1,scale,shape2,shape3){
	y <- (x/scale)^shape1
	dy_dx <- (shape1/scale)*(x/scale)^(shape1-1)
	z <- y/(1+y)
	dz_dy <- (1+y)^(-2)
	return(dbeta(z,shape2,shape3)*dz_dy*dy_dx)
}

pgb2 <- function(x,shape1,scale,shape2,shape3){
	y <- (x/scale)^shape1
	z <- y/(1+y)
	return(pbeta(z,shape2,shape3))
}

qgb2 <- function(prob,shape1,scale,shape2,shape3){
	z <- qbeta(prob,shape2,shape3)
	y <- z/(1-z)
	return(scale*y^(1/shape1))
}

rgb2 <- function(n,shape1,scale,shape2,shape3){
	z <- rbeta(n,shape2,shape3)
	y <- z/(1-z)
	return(scale*y^(1/shape1))
}