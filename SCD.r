SCD <- function(x,...) UseMethod("SCD")

SCD.default <- function(x,y,w=NULL) {
	## structure inputss
	
	out=check.and.norm.performMM(x,y,w,varFun=sVarDiv,metFun=SCDForm)
	out$call <- match.call()
	return(out)
}