NMSE <- function(x,...) UseMethod("NMES")

NMSE.default <- function(x,y,w=NULL) {
	## structure inputs
	x <- as.matrix(x)
	y <- as.matrix(y)
	if (is.null(w)) w=array(1,dim(x))

	out=setNMEclassVars(x,y,w,varFun=sd,metFun=NMSEForm,varFun=sVarDiv)	
	out$call <- match.call()
	
	return(out)
}


