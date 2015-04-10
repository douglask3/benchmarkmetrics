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

phaseAVar <- function(x) {
	diffs=abs(x-mean(x))
	test=diffs>(pi)
	diffs[test]=2*pi-diffs[test]
	return(mean(diffs))
}