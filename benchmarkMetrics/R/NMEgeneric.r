print.NME <- function(x, ...) {
	cat("Call:\n")
	print(x$call)

	cat("\nScores:\n")
	cat.NME.scores(x)
}

summary.NME <- function(x, ...) {
	summ=basic.summaryInfo(x)
    summ=c(summ,Scores=scores.summaryInfo(x))
	return(summ)
}

plot.NME <- function(x,...)  {
    cols=c('red','green','blue')
    pchs=c(19,4,1)
    yi=x$y
    
    plot(range(x$x123), range(yi), type='n', xlab='x', ylab='y', ...)
    
    for (i in 1:3) {
        xi = x$x123[, i]
        points(xi, yi, pch = pchs[i], col=cols[i])
        lines(xi, predict(lm(yi ~ xi)), col = cols[i])
    }
    
    lines(c(-9E9, 9E9), c(-9E9, 9E9), lwd=2) 

    legend('topleft', paste('Step',1:3), col=cols, bty='n', pch=pchs, lty=1)
}

score.NME <- function(x, ...)
    if (names(x[2]) == "x") return(x[[1]]) else return(unlist(x[1:3]))

null.NME  <- function(x, ...) null.FUN(x, NME , items=FALSE, step1only=TRUE, ...) 

null.NMSE <- function(x, ...) null.FUN(x, NMSE, items=FALSE, step1only=TRUE, ...) 