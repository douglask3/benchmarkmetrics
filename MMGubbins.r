setMMclassVars <- function(x,y,w,varFun=absVar,metFun=MMForm,...) {
	
	out=list(score=metFun(x,y,w,...))
	
	out=setMetClassInfo(out,x,y,w,varFun=varFun)
	
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
	cat("Call:\n")
	print(x$call)

	cat("\nScores:\n")
	print(x$score)
}

plot.MM <- function(x,...)  plot(x$x,x$y,cex=x$w,...)
