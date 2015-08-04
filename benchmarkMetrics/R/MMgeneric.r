print.MM <- function(x, ...) {
	cat("Call:\n")
	print(x$call)

	cat("\nScore:\n\t")
	cat(standard.round(x$score))
	cat("\n")
    invisible()
}

summary.MM <- function(x, ...) {
	summ=basic.summaryInfo(x)
	summ=c(summ, Scores = x$score)

	class(summ) = "MetricSummary"
	return(summ)
}

plot.MM <- function(x, ...)  {
    n    = ncol(x$x)
    maxw = max(x$w)
    cols = rainbow(n)

    plot(x$x, x$y, type='n', xlab='x', ylab='y', ...)
    for (i in 1:n)
        points(x$x[, i], x$y[, i], col = cols[i], pch = 19, cex = x$w[i] / maxw)
    legend(legend = names(x$xMean), x = 'topright', pch = 19, col = cols, title = 'items')
    invisible()
}

score.MM <- function(x, ...) unlist(x[1])

null.MM  <- function(x, ...) null.FUN(x, MM , items=TRUE, ...)
null.SCD <- function(x, ...) null.FUN(x, SCD, items=TRUE, ...)
