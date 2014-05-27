NMSE <- function(x,...) UseMethod("NMES")

NMSE.default <- function(x,y,w=NULL) {
	## structure inputs
	out=structure.inputs.performNME(x,y,w,varFun=sd,metFun=NMSEForm,varFun=sVarDiv)
	out$call <- match.call()
	
	return(out)
}



