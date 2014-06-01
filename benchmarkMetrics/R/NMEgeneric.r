print.NME <- function(x, ...) {
	cat("Call:\n")
	print(x$call)

	cat("\nScores:\n")
	cat("Step1\t\tStep2\t\tStep3\n")
	
	cat.NME.scores(x)
}


summary.NME <- function(x, ...) {
	summ=basic.summaryInfo(x)
	summ$Scores=scores.summaryInfo(x)
	return(summ)
}

plot.NME <- function(x,...)  plot(x$x,x$y,cex=x$w,...)