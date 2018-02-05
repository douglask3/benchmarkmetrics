print.DMM <- function(x, ...) 
	print.MM(x,...)


summary.DMM <- function(x, ...) {
	
	summ = basic.summaryInfo(x)
	
	summ = c(summ, Scores = x$score, scoreMat = list(x$scoreMat))

	class(summ) = "DescreteMetricSummary"
	return(summ)
}

plot.DMM <- function(x, ...)  {
	
	index = unique(x$x)	
	xsplit = lapply(index, function(i) x$diff[x$x == i])
	names = colnames(x$scoreMat)[index]
	
	multiLine <- function(name)  {
		nc = nchar(name)
		if (nc < 10) return(name)
		
		name = strsplit(name, ' ')[[1]]
		nw = length(name)
		
		pasteS <- function(index, collapse = ' ') paste(name[index], collapse = ' ')
		
		if (nc > 20) {
			mp = ceiling(nw * c(1/3, 2/3))
			name = c(pasteS(1:mp[1]),pasteS((mp[1] + 1):mp[2]), pasteS((mp[2] + 1):nw))
		} else if (nc > 10) {
			mp =  ceiling(nw/2)
			name = c(pasteS(1:mp), pasteS((mp+1):nw))
		}
		name = paste(name, collapse = '\n')
		
		return(name)
	}
	names = sapply(names, multiLine)
	
	boxplot(xsplit, axes = FALSE)
	axis(2)
	
	ncats = length(index)
	axis(1, at = 1:ncats, labels = names, padj = 1, line = -1, lty = 0)
	axis(1, at = 0:(ncats+1), labels = rep('', ncats + 2))
	
	invisible()
}

score.DMM <- function(x, ...) unlist(x[1])

#null.DMM  <- function(x, ...) null.FUN(x, DMM , items=TRUE, ...)
#null.DSCD <- function(x, ...) null.FUN(x, DSCD, items=TRUE, ...)
