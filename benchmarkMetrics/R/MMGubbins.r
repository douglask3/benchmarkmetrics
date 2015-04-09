check.and.norm.performMM <- function(x,y,w,...) {
	c(x,y,w):=structure.inputs(x,y,w)
	normalise <- function(x) sweep(x,1,rowSums(x),'/')
	
	if (dim(x)[2]<2 && dim(y)[2]<2) stop("2 or more items required")
	
	w0=w
	if (dim(w)[2]==1) for (i in 1:(dim(x)[2]-1)) w=r=cbind(w,w0)
	
	x=normalise(x)
	y=normalise(y)
	
	out=setMMclassVars(x,y,w,...)	
	return(out)
}

setMMclassVars <- function(x,y,w,varFun=absVar,metFun=MMForm,...) {
	
	out=list(score=metFun(x,y,w,...))
	out=setMetClassInfo(out,x,y,w,varFun=varFun,itemize=TRUE)
	
	class(out)="MM"
	return(out)
}