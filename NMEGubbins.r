setNMEclassVars <- function(x,y,w,varFun=absVar,...) {
	out=NMEGubbins(x,y,w,...)
	
	out=setMetClassInfo(out,x,y,w,varFun=absVar)
	
	class(out)="NME"
	return(out)
}

NMEGubbins <- function(x,y,w,metFun=NMEForm,varFun=aVarDiv) {
	## Define functions
	
	ManVars <- function(x,y,w,FUN) list(FUN(x),FUN(y),FUN(w))
	
	NME1=metFun(x,y,w)
	
	c(x,y,z):=ManVars(x,y,w,MeanSub)
	NME2=metFun(x,y,w)
	
	c(x,y,z):=ManVars(x,y,w,varFun)
	NME3=metFun(x,y,w)
	
	return(list(step1=NME1,step2=NME2,step3=NME3))
}

NMEForm <- function(x,y,w) sum(w*abs(y-x))/sum(w*abs(x-mean(x)))
aVarDiv	<- function(x) x-absVar(x)

NMSEForm <- function(x,y,w) sum(w*(y-x)^2)/sum(w*(x-mean(x))^2)
sVarDiv	<- function(x) x-sd(x)

MeanSub <- function(x) x-mean(x)
absVar  <- function(x) mean(abs(x-mean(x)))
	

print.NME <- function(x, ...) {
	cat("Call:\n")
	print(x$call)

	cat("\nScores:\n")
	cat("Step1\t\tStep2\t\tStep3\n")
	
	cat.NME.scores(x)
}

cat.NME.scores <- function(x) {
	cat("Step1\t\tStep2\t\tStep3\n")
	
	scores=paste(standard.round(x$step1),
				 standard.round(x$step2),
				 standard.round(x$step3),sep="\t\t")
	cat(scores)
}


summary.NME <- function(x, ...) {
	summ=basic.summaryInfo(x)
	summ$Scores=scores.summaryInfo(x)
	return(summ)
}

basic.summaryInfo <- function(x) {
	summ=list(metic=strsplit(as.character(x$call)[1],'\\.')[[1]][1],
		 	  xMean=x$xMean,
		 	  yMean=x$yMean,
		 	  "x:y Mean ratios"=x$MeanRatio,
		 	  xVariance=x$xVar,
		 	  yVariance=x$yVar,
		 	  "x:y Variance ratios"=x$VarRatio)
	
	summ=determinIfWeightsUsed(x,summ)
	class(summ)="listofMetric"
	return(summ)
}

determinIfWeightsUsed <- function(x,summ) {
	if (length(as.character(x$call))<4) return(c(summ,weights='non-defined'))
		else return(c(summ,weights='defined'))
}

print.listofMetric  <- function(x,...) {
	printSingle.listofMetric <- function(x, ...) {
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

	lapply(x,printSingle.listofMetric)
	invisible(x)
}


scores.summaryInfo <- function(x) {
	Scores=c(x$step1,x$step2,x$step3)
	names(Scores)=paste('Step',1:3)
	return(Scores)
}

plot.NME <- function(x,...)  plot(x$x,x$y,cex=x$w,...)
