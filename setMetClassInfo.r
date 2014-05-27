setMetClassInfo <- function(out,x,y,w,varFun=absVar,itemize=FALSE) {
	out$x=x
	out$y=y
	out$w=w
	
	c(x,y):=setColNames(x,y)
	
	if (itemize) {
		meanMe 	<- function(x,...)  apply(x,2,mean)
		varMe 	<- function(x,...)  apply(x,2,absVar)
	} else {
		meanMe 	<- mean
		varMe	<- varFun
	}
	
	out$xMean=meanMe(x)
	out$yMean=meanMe(y)
	out$MeanRatio=out$yMean/out$xMean
	
	out$xVar=varMe(x)
	out$yVar=varMe(y)
	out$VarRatio=out$yVar/out$xVar
	return(out)
}

setColNames <- function (x,y) {
	if (is.null(colnames(x)) && is.null(colnames(y))) colnames(x)=letters[1:dim(x)[2]]
	setColname <- function(x,y)	{
		if (is.null(colnames(x))) colnames(x)=colnames(y)
		return(x)
	}
	
	x=setColname(x,y)
	y=setColname(y,x)
	
	return(list(x,y))
}