NME <- function(x,...) UseMethod("NME")

NME.default <- function(x,y,w=NULL) {
	out=structure.inputs.performNME(x,y,w)
	out$call <- match.call()
	return(out)
}

structure.inputs.performNME <- function(x,y,w,...) {
	c(x,y,w):=structure.inputs(x,y,w)
	out=setNMEclassVars(x,y,w,...)	
	return(out)
}

