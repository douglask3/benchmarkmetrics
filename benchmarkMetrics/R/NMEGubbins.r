structure.inputs.performNME <- function(x,y,w,...) {
	c(x,y,w):=structure.inputs(x,y,w)
	out=setNMEclassVars(x,y,w,...)	
	return(out)
}

setNMEclassVars <- function(x,y,w,varFun=absVar,...) {
	out=NMEGubbins(x,y,w,varFun=varFun,...)
	out=setMetClassInfo(out,x,y,w,varFun=absVar)
	
	class(out)="NME"
	return(out)
}

NMEGubbins <- function(x,y,w,metFun=NMEForm,varFun=absVar) {
	NME1 = metFun(x,y,w)
	
	x2   = MeanSub(x ,y)
	NME2 = metFun (x2,y,w)
	
    x3   = VarDiv (x2,y,varFun) 
    x3   = MeanSub(x3,y)
	NME3 = metFun (x3,y,w)
	
	return(list(step1=NME1,step2=NME2,step3=NME3,
                x123=cbind(x,x2,x3)))
}

VarDiv  <- function(x,y,FUN) mean(y)+x*FUN(y)/FUN(x)
MeanSub <- function(x,y) x-mean(x)+mean(y)
absVar  <- function(x) mean(abs(x-mean(x)))
