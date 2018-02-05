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

print.DescreteMetricSummary <- function(x, ...) {
	
	printMetricSummaryHead(x)
	printMeanVarSummaryTable.Descrete(x)
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
	if (x$Metric == "MM" || x$Metric == "SCD") printMeanVarSummaryTableMM(x)
		else printMeanVarSummaryTableStnd(x)
	MeanVars = unlist(x[c('xMean',     'yMean',     'x:y Mean ratios',
	                      'xVariance', 'yVariance', 'x:y Variance ratios')])
}

printMeanVarSummaryTable.Descrete <- function(x) {
	cat(' Basic Variable Information:\n')	
	MeanVars = unlist(x[c('xMean',     'yMean')])
	MeanVars = colnames(x$scoreMat)[MeanVars]
	
	cat('Most common type\n')
	names(MeanVars) = c( 'x',     'y')

	print(MeanVars)
}

printMeanVarSummaryTableMM <- function(x) {
	MeanVarsi = x[c('xMean',     'yMean',     'x:y Mean ratios',
	               'xVariance', 'yVariance', 'x:y Variance ratios')]

	MeanVars = matrix(NaN, ncol = 3, nrow = 0)
	for (i in 1:length(MeanVarsi[[1]])) {
		j = sapply(MeanVarsi,function(j) standard.round(j[i]))
		j = t(matrix(j, nrow = 3))
		MeanVars = rbind(MeanVars,j)
	}

	colnames(MeanVars) = c('x', 'y', 'x:y')
	rnames = c(' Mean',' Variance')
	nrows = nrow(MeanVars)

	MeanVars = cbind(rep(rnames, floor(nrows)/2), MeanVars)
	rnames = rep('', nrow(MeanVars))

	rn = names(x[[2]])

	rnames[seq(1, nrow(MeanVars), by = 2)] = rn
	rownames(MeanVars) = rnames

	print(MeanVars, quote = FALSE)
}

printMeanVarSummaryTableStnd <- function(x) {
	MeanVars = unlist(x[c('xMean',     'yMean',     'x:y Mean ratios',
	                      'xVariance', 'yVariance', 'x:y Variance ratios')])

	MeanVars = t(matrix(standard.round(MeanVars), nrow = 3))
	#MeanVars = matrix(standard.round(MeanVars), ncol = 3)
	colnames(MeanVars) = c('x', 'y', 'x:y')
	rownames(MeanVars) = c(' Mean',' Variance')

	print(MeanVars)
}

printMetricSummaryScores <- function(x) {
	x[['Scores']] = standard.round(x[['Scores']])

	printMetricCat(x['Scores'])
}
