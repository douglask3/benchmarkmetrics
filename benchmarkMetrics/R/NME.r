NME <- function(x,...) UseMethod("NME")

NME.default <- function(x,y,w=NULL) {
	out=structure.inputs.performNME(x,y,w)
	out$call <- match.call()
	return(out)
}

