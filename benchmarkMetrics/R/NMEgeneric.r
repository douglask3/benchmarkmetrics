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
    
    plot(range(x$x123),range(x$y123),type='n')
    lines(c(-9E9,9E9),c(-9E9,9E9),lty=2)
    for (i in 1:3)
        points(x$x123[,i],x$y123[,i],pch=19,
               col=cols[i])
               
    legend('topleft',paste('Step',1:3),pch=19,col=cols)
}