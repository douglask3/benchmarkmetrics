print.DMM <- function(x, ...) 
	print.MM(x,...)


summary.DMM <- function(x, ...) {
	
	summ = basic.summaryInfo(x)
	
	summ = c(summ, Scores = x$score,
			scoreMat = list(x$scoreMat), 
			similarityTable = similarityTable.DMM(x)[1])
	
	class(summ) = "DescreteMetricSummary"
	return(summ)
}

similarityTable.DMM <- function(x) {
	xi = x$x
	yi = x$y
	
	ix = unique(xi); nx = length(ix)
	iy = unique(yi); ny = length(iy)
	
	z = matrix(0, ncol = length(ix), nrow = length(iy))
	
	for (i in 1:nx) for (j in 1:ny) {
		ii = ix[i]; jj = iy[j]
		z[j, i] = sum(yi == jj & xi == ii)
	}
	
	colnames(z) = colnames(x$scoreMat)[ix]
	rownames(z) = colnames(x$scoreMat)[iy]
	return(list(z, ix, iy))
}

plot.DMM <- function(x, ...) {
	c(countTab, ix, iy) := similarityTable.DMM(x)
	scoreTab = countTab
	
	is = rep(ix, each = length(iy))
	js = rep(iy, length(ix))
	
	scoreTab[] =  mapply(function(i,j) x$scoreMat[i,j], is, js)
	
	yrange = c(0,  max(scoreTab[countTab > 0]), ...)
	
	plot(c(0.5, length(ix) + 0.5), yrange, type = 'n', 
		 axes = FALSE, xlab = '', ylab = 'score')
	
	names = colnames(x$scoreMat)[ix]
	names = sapply(names, convertStr2multiLine)
	
	axis(1, at = 1:length(ix), labels = names)
	axis(2)
	
	findPoints <- function(i, j) {
		x = j
		y   = scoreTab[i,j]
		cex = countTab[i,j]
		
		name = rownames(countTab)[i]
		name = convertStr2multiLine(name)
		
		return(c(x, y, cex, name, -0.15 ))
	}
	
	pnts = c()
	for (i in 1:nrow(countTab)) for (j in 1:ncol(countTab)) pnts = rbind(pnts, findPoints(i,j))
	
	for (i in 1:(nrow(pnts)-1)) for (j in (i+1):nrow(pnts)) {
		if (pnts[i, 1] == pnts[j, 1] && 
			abs(as.numeric(pnts[i,2]) - as.numeric(pnts[j, 2])) < 0.1 &&
			as.numeric(pnts[i,3]) > 0.0 && as.numeric(pnts[j,3]) > 0.0) {
			 pnts[i,1] = as.numeric(pnts[i,1]) - 0.1
			 pnts[j,1] = as.numeric(pnts[j,1]) + 0.1
			 pnts[j,5] = 0.1
		}
	}
	
	addPoints <-  function(pnt) {
		x = as.numeric(pnt[1]); y = as.numeric(pnt[2]); cex = 2 * as.numeric(pnt[3]) / max(as.numeric(pnts[,3]))
		if (cex == 0) return()
		points(x, y, cex = cex, pch = 19)
		text(pnt[4], x = x + as.numeric(pnt[5]), y = y)
	}
	
	apply(pnts, 1, addPoints )
	
	invisible()
}

boxplot.DMM <- function(x, ...)  {
	
	index = unique(x$x)	
	xsplit = lapply(index, function(i) x$diff[x$x == i])
	
	names = colnames(x$scoreMat)[index]
	names = sapply(names, convertStr2multiLine)
	
	boxplot(xsplit, axes = FALSE, ylab = 'score', ...)
	axis(2)
	
	ncats = length(index)
	axis(1, at = 1:ncats, labels = names, padj = 1, line = -1, lty = 0)
	axis(1, at = 0:(ncats+1), labels = rep('', ncats + 2))
	
	invisible()
}

convertStr2multiLine <- function(name)  {
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

score.DMM <- function(x, ...) unlist(x[1])

meanMod.DMM <- function(x) 
	rep(x, each = length(x))#ncol = length(x), nrow = length(x))


medianMod.DMM <- function(x, ...)
	return(list('Median', x))

null.DMM  <- function(mat, x, ..., itemNames = NULL) {
	x = convert2numeric4DMM(x, itemNames, mat)
	null.FUN(x, DMM , items=TRUE, medianFun = medianMod.DMM, meanFun = meanMod.DMM, mat = mat, itemNames = NULL, ...)
	
}


null.DSCD <- function(mat, x, ...) null.FUN(x, DSCD, items=TRUE, medianFun = medianMod.DMM, meanFun = meanMod.DMM, mat = mat, ...)
