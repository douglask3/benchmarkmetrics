score <- function(x, ...) UseMethod("score")

cat.NME.scores <- function(x) {
	cat("Step1\t\tStep2\t\tStep3\n")
	
	scores=paste(standard.round(x$step1),
				 standard.round(x$step2),
				 standard.round(x$step3), sep="\t\t")
	cat(scores)
}

basic.summaryInfo <- function(x) {
	summ = list(metic = strsplit(as.character(x$call)[1], '\\.')[[1]][1],
		 	  xMean = x$xMean,
		 	  yMean = x$yMean,
		 	  "x:y Mean ratios" = x$MeanRatio,
		 	  xVariance = x$xVar,
		 	  yVariance = x$yVar,
		 	  "x:y Variance ratios" = x$VarRatio)
	
	summ = determinIfWeightsUsed(x,summ)
	class(summ) = "listofMetric"
	return(summ)
}

determinIfWeightsUsed <- function(x,summ) {
	if (length(as.character(x$call)) < 4) return(c(summ, weights = 'non-defined'))
		else return(c(summ, weights = 'defined'))
}

scores.summaryInfo <- function(x) {
	Scores=c(x$step1, x$step2, x$step3)
	names(Scores) = paste('Step', 1:3)
	return(list(Scores))
}