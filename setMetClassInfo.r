setMetClassInfo <- function(out,x,y,w,varFun=absVar) {
	out$x=x
	out$y=y
	out$w=w
	
	out$xMean=mean(x)
	out$yMean=mean(y)
	out$MeanRatio=out$yMean/out$xMean
	
	out$xVar=varFun(x)
	out$yVar=varFun(y)
	out$MeanRatio=out$xVar/out$yVar
}