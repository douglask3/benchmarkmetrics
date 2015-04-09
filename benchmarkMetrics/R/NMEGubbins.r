structure.inputs.performNME <- function(x,y,w,...) {
	c(x,y,w):=structure.inputs(x,y,w)
	out=setNMEclassVars(x,y,w,...)	
	return(out)
}

setNMEclassVars <- function(x,y,w,varFun=absVar,...) {
	out=NMEGubbins(x,y,w,...)
	out=setMetClassInfo(out,x,y,w,varFun=absVar)
	
	class(out)="NME"
	return(out)
}

NMEGubbins <- function(x,y,w,metFun=NMEForm,varFun=aVarDiv) {
	## Define functions
	
	ManVars <- function(x,y,w,FUN) list(FUN(x),FUN(y),FUN(w))
	
	NME1=metFun(x,y,w)
	
	c(x2,y2,z):=ManVars(x,y,w,MeanSub)
	NME2=metFun(x2,y2,w)
	
	c(x3,y3,z):=ManVars(x2,y2,w,varFun)
	NME3=metFun(x3,y3,w)
	
	return(list(step1=NME1,step2=NME2,step3=NME3,
                x123=cbind(x,x2,x3),y123=cbind(y,y2,y3)))
}


aVarDiv	<- function(x) x-absVar(x)
sVarDiv	<- function(x) x-sd(x)
MeanSub <- function(x) x-mean(x)
absVar  <- function(x) mean(abs(x-mean(x)))
	