setMPDclassVars <- function(x,y,w,...) {
	out=MPDGubbins(x,y,w,...)
	class(out)="MPD"
	return(out)
}

MPDGubbins <- function(x,y,w,metFun=NMEForm,...) {
	c(xp,xc):=seasonal_concentration_and_phase(x)
	c(yp,yc):=seasonal_concentration_and_phase(y)
	return(list(Phase=MPDonly(xp,yp,w),Concentration=NME(xc,yc,w)))
}

MPDonly <- function(x,y,w,..) {
	Phase=list(Score=MPDForm(x,y,w))
	Phase=setMetClassInfo(Phase,x,y,w,varFun=phaseAVar,DiffFun=radianDiffs)
	return(Phase)
}


MPDForm <- function(x,y,w) sum((1/pi)*w*acos(cos(y-x)))/length(x)


phaseAVar <- function(x) {
	diffs=abs(x-mean(x))
	test=diffs>(pi)
	diffs[test]=2*pi-diffs[test]
	return(mean(diffs))
}

	
print.MPD <- function(x, ...) {
	
	cat("Call:\n")
	print(x$call)
	
	cat("\nMPD Phase Score:\n")
	print(standard.round(x$Phase$Score))
	
	cat("\nConcerntration Scores:\n")
	cat.NME.scores(x$Concentration)
}


summary.MPD <- function(x, ...) {
	
	required.standard <- function(l) head(basic.summaryInfo(x[[l]])[-1],-1)
	
	summ=list()
	
	summ=determinIfWeightsUsed(x,summ)
	summ$weights=list(weights=summ$weights)
	
	summ$Concentration=c(Metric='NME - Concentration',required.standard('Concentration'))
	summ$Concentration$Scores=scores.summaryInfo(x$Concentration)
	
	summ$Phase=c(Metric='MPD - Phase',required.standard('Phase'))
	summ$Phase$Scores=standard.round(x$Phase$Score)
	
	
	class(summ) <- 'listofMetric'
	return(summ)
}
