setNMEclassVars <- function(x,y,w,varFun=absVar,...) {
	out=NMEGubbins(x,y,w,...)
	out=setMetClassInfo(out,x,y,w,varFun=varFun)
	
	class(out)="NME"
	return(out)
}

NMEGubbins <- function(x,y,w,metFun=NMEForm,varFun=aVarDiv) {
	## Define functions
	
	ManVars <- function(x,y,w,FUN) list(FUN(x),FUN(y),FUN(w))
	
	NME1=metFun(x,y,w)
	
	c(x,y,z):=ManVars(x,y,w,MeanSub)
	NME2=metFun(x,y,w)
	
	c(x,y,z):=ManVars(x,y,w,varFun)
	NME3=metFun(x,y,w)
	
	return(list(step1=NME1,step2=NME2,step3=NME3))
}

NMEForm <- function(x,y,w) sum(w*abs(y-x))/sum(w*abs(x-mean(x)))
aVarDiv	<- function(x) x-absVar(x)

NMSEForm <- function(x,y,w) sum(w*(y-x)^2)/sum(w*(x-mean(x))^2)
sVarDiv	<- function(x) x-sd(x)

MeanSub <- function(x) x-mean(x)
absVar  <- function(x) mean(abs(x-mean(x)))
	

print.NME <- function(x, ...) {
	cat("Call:\n")
	print(x$call)

	cat("\nScores:\n")
	cat("Step1\t\tStep2\t\tStep3\n")
	
	scores=paste(standard.round(x$step1),
				 standard.round(x$step2),
				 standard.round(x$step3),sep="\t\t")
	cat(scores)
}

summary.NME <- function(x, ...) {
	summ=basic.summaryInfo(x)
	
	summ$Scores=c(x$step1,x$step2,x$step3)
	names(summ$Scores)=paste('Step',1:3)
	
	return(summ)
}

basic.summaryInfo <- function(x) {
	summ=list(metic=strsplit(as.character(x$call)[1],'\\.')[[1]][1],
		 	  xMean=x$xMean,
		 	  yMean=x$yMean,
		 	  "x:y Mean ratios"=x$MeanRatio,
		 	  xVariance=x$xVar,
		 	  yVariance=x$yVar,
		 	  "x:y Variance ratios"=x$VarRatio)
	if (length(as.character(x$call))<4) summ=c(summ,weights='non-defined')
		else summ=c(summ,weights='defined')
	class(summ)="listofMetric"
	return(summ)
}

plot.NME <- function(x,...)  plot(x$x,x$y,cex=x$w,...)
