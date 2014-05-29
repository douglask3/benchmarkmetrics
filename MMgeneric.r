print.MM <- function(x, ...) {
	cat("Call:\n")
	print(x$call)
	
	cat("\nScore:\n")
	print(standard.round(x$score))
}

summary.MM <- function(x, ...) {
	summ=basic.summaryInfo(x)
	summ=c(summ,score=x$score)
	return(summ)
}

plot.MM <- function(x,...)  plot(x$x,x$y,cex=x$w,...)
