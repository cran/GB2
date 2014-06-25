# The indicators are always defined with shape1,scale,shape2,shape3 of the direct estimate  #4.02.2011

# At-risk-of-poverty threshold
arpt.cgb2 <- function(prop,shape1,scale,shape2,shape3,pl0,pp,decomp="r"){
     median <- qcgb2(0.5,shape1,scale,shape2,shape3,pl0,pp,decomp)   # scaled median
     return(prop*median)
}  
         
# At-risk-of-poverty rate
arpr.cgb2 <- function(prop,shape1,shape2,shape3,pl0,pp,decomp="r"){
     return(pcgb2(arpt.cgb2(prop,shape1,1,shape2,shape3,pl0,pp,decomp),shape1,1,shape2,shape3,pl0,pp,decomp))
}  

# Relative median poverty gap
rmpg.cgb2 <- function(arpr,shape1,shape2,shape3,pl0,pp,decomp="r"){
 return(1-qcgb2(arpr/2,shape1,1,shape2,shape3,pl0,pp,decomp)/qcgb2(arpr,shape1,1,shape2,shape3,pl0,pp,decomp))   
}

# Quintile share ratio
qsr.cgb2 <- function(shape1,shape2,shape3,pl0,pp,decomp="r") {
	q20 <- qcgb2(0.2,shape1,1,shape2,shape3,pl0,pp,decomp)
	q80 <- qcgb2(0.8,shape1,1,shape2,shape3,pl0,pp,decomp)
	return((1-incompl.cgb2(q80,1,shape1,1,shape2,shape3,pl0,pp,decomp))/incompl.cgb2(q20,1,shape1,1,shape2,shape3,pl0,pp,decomp))
	}