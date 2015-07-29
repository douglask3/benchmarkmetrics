print.MetricSummaryList  <- function(x, ...) {
	printMetricHead(x[[1]]); printMetricHead(x[2], '\t')
	lapply(x[-1:-2], print.MetricSummary)
	invisible(x)
}

print.MetricSummary <- function(x, ...) {
	cat("\n==============================\n")
	printMetricHead(x[1])
	cat(  "==============================\n")
	weightID = names(x) == 'weights'
	if (any(weightID)) printMetricHead(x[weightID])

	cat(' Basic Variable Information:\n')
	MeanVars = unlist(x[c('xMean',     'yMean',     'x:y Mean ratios',
	                      'xVariance', 'yVariance', 'x:y Variance ratios')])
	MeanVars = t(matrix(standard.round(MeanVars), nrow = 3))
	colnames(MeanVars) = c('x', 'y', 'x:y')
	rownames(MeanVars) = c(' Mean',' Variance')

	print(MeanVars)

	cat("\n")
	x[['Scores']] = standard.round(x[['Scores']])
	printMetricHead(x['Scores'])
	cat("\n")
	invisible(x)
}

printMetricHead <- function(x, strt = "")
	cat(strt, names(x),': ', x[[1]], '\n', sep =" ")
