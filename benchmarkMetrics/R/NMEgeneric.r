print.NME <- function(x, ...) {
	cat("Call:\n")
	print(x$call)

	cat("\nScores:\n")
	cat.NME.scores(x)
	cat("\n")
    invisible()
}

summary.NME <- function(x, ...) {
	summ = basic.summaryInfo(x)
    summ = c(summ, Scores = scores.summaryInfo(x))

	class(summ) = "MetricSummary"
	return(summ)
}

plot.NME <- function(x, xlab='x', ylab='y', ...)  {
    cols = c('red', 'green', 'blue')
    pchs = c(19, 4, 1)
	xi   = x$x

    plot(range(xi), range(x$y123), type='n', xlab = xlab, ylab = ylab, ...)

    for (i in 1:3) {
        yi = x$y123[, i]
        points(xi, yi, pch = pchs[i], col = cols[i])
        lines(xi, predict(lm(yi ~ xi)), col = cols[i])
    }

    lines(c(-9E9, 9E9), c(-9E9, 9E9), lwd = 2)

    legend('topleft', paste('Step',1:3), col = cols, bty = 'n', pch = pchs, lty = 1)
    invisible()
}

score.NME <- function(x, ...)
    if (names(x[2]) == "x") return(x[[1]]) else return(unlist(x[1:3]))

null.NME  <- function(x, ...) null.FUN(x, NME , items = FALSE, ...)
null.NMSE <- function(x, ...) null.FUN(x, NMSE, items = FALSE, ...)
null.NMGE <- function(x, ...) null.FUN(x, NMGE, items = FALSE, maintainShape = TRUE,...)

