setMMclassVars <- function(x,y,w,varFun=absVar,metFun=MMForm,...) {
	
	out=list(score=metFun(x,y,w,...))
	
	out=setMetClassInfo(out,x,y,w,varFun=varFun,itemize=TRUE)
	
	class(out)="MM"
	return(out)
}

MMForm 		<- function(x,y,w) sum(abs(w*(x-y)))/sum(w*dim(x)[1])
SCDForm 	<- function(x,y,w) sum(w*(sqrt(x)-sqrt(y))^2)/sum(w*dim(x)[1])

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

print.listofMetric <- function(x, ...) {
	nn <- names(x)
    ll <- length(x)
    if (length(nn) != ll) 
        nn <- paste("Component", seq.int(ll))
    for (i in seq_len(ll)) {
        cat(nn[i], ":\n")
        
        if (is.numeric(x[[i]])) {
        	
        	if (length(x[[i]])==1) {
        		cat("\t")
        		cat(standard.round (x[[i]]), ...)
        	} else print(standard.round (x[[i]]), ...)
        }	else cat("\t",x[[i]])
        cat("\n\n")
    }
    invisible(x)

}

plot.MM <- function(x,...)  plot(x$x,x$y,cex=x$w,...)
