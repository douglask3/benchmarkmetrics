NME <- function(x,...) UseMethod("NME")

NME.default <- function(x,y,w=NULL) {
	## structure inputs
	x <- as.matrix(x)
	y <- as.matrix(y)
	if (is.null(w)) w=array(1,dim(x))

	out=setNMEclassVars(x,y,w)	
	out$call <- match.call()
	
	return(out)
}


