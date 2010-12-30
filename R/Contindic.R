contindic.gb2 <- function(resol, a, p1, p2, q1, q2, fn, tit, table = FALSE){

	pp <- round(seq(p1,p2,length.out=resol),digits=2)
	qq <- round(seq(q1,q2,length.out=resol),digits=2)
	d <- pp %o% qq
	for (i in 1:resol){
		for (j in 1:resol){
		d[i,j] <- fn(a,pp[i],qq[j])
		}
	}
  dlim = range(d, finite = TRUE)
	contour(pp, qq, d, levels = pretty(seq(dlim[1], dlim[2], length.out=17)),
	xlab = "p", ylab = "q", main = paste("a =", as.character(a)), cex=1.8)
	box()
  mtext(tit,line=0.5)
	if(table){
		d <- rbind(p=pp, d) 
		d <- round(cbind(c(NA,qq), d), digits=2)	
	print(paste("a =", as.character(a)), quote=FALSE)
	print(d)
	}
}
