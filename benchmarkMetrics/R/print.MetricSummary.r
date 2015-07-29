print.MetricSummaryList  <- function(x, ...) {
	printMetricCat(x[[1]]); printMetricCat(x[2], '\t')
	lapply(x[-1:-2], print.MetricSummary)
	invisible(x)
}

print.MetricSummary <- function(x, ...) {
	printMetricSummaryHead(x)
	printMeanVarSummaryTable(x)
	cat("\n")
	printMetricSummaryScores(x)
	cat("\n")
	invisible(x)
}

printMetricCat <- function(x, strt = "")
	cat(strt, names(x),': ', x[[1]], '\n', sep =" ")

printMetricSummaryHead <- function(x) {
	cat("\n==============================\n")
	printMetricCat(x[1])
	cat(  "==============================\n")
	weightID = names(x) == 'weights'
	if (any(weightID)) printMetricCat(x[weightID])
}

printMeanVarSummaryTable <- function(x) {
	cat(' Basic Variable Information:\n')
	MeanVars = unlist(x[c('xMean',     'yMean',     'x:y Mean ratios',
	                      'xVariance', 'yVariance', 'x:y Variance ratios')])
	MeanVars = t(matrix(standard.round(MeanVars), nrow = 3))
	colnames(MeanVars) = c('x', 'y', 'x:y')
	rownames(MeanVars) = c(' Mean',' Variance')

	print(MeanVars)
}

printMetricSummaryScores <- function(x) {
	x[['Scores']] = standard.round(x[['Scores']])
	printMetricHead(x['Scores'])
}
