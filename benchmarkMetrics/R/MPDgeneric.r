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
