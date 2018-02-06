check.and.norm.performDMM <- function(mat, x, y, w, 
								      matAsFile = is.character(mat), 
									  row1AsNames = is.character(mat[1,1]), 
									  traitID = TRUE, allowRegridding = TRUE, ...) {
	
	if (matAsFile) mat = read.csv(mat, stringsAsFactors = FALSE)
	
	index = 1:(dim(mat)[1])
	
	if (row1AsNames) {
		names = mat[,1]
		mat = mat[,-1]
	} else names = index
	
	normaliseTrait <- function(index) 
		mat[index] / apply(mat[index], 1, sum)
	
	if (is.null(traitID))  
		nTraits = ncol(mat)
	else if (is.logical(traitID)) {		
		if (traitID) nTraits = ncol(mat)
			else nTraits = 1
		
		mat = normaliseTrait(1:nTraits)
	} else if (!is.logical(traitID)) {
		IDs = unique(traitID)
		for (i in unique(traitID)) {
			index = traitID == i
			mat[index] = normaliseTrait(index)
		}
		nTraits = length(IDs)
	}
	
	if (is.list(y) && (y[[1]] == "Median" || y[[1]] == "median")) {
		mat = rbind(mat, apply(mat, 2, function(i) mean(i[y[[2]]])))
		names = c(names, 'Median')
		y = nrow(mat)
	}
	
	c(x, y, w) := structure.inputs(x, y, w, allowRegridding = allowRegridding)
	
	out = setDMMclassVars(mat, names, nTraits, x, y, w, ...)
	return(out)
}
	

setDMMclassVars <- function(mat, names, nTraits, x, y, w, varFun = absVar, metFun = MMForm,
	                       step1only = NULL, ...) {
						   				  
	DMM.point <- function(b1, b2) 
		metFun(mat[b1,], mat[b2,], 1) / nTraits
	index = 1:(dim(mat)[1])
	
	
	scores = sapply(index, function(...) sapply(index, function(b1, b2) DMM.point(b1, b2), ...))
	#	scores2 = sapply(index, function(...) sapply(index, function(b1, b2) DMM.point2(b1, b2), ...))
	
	colnames(scores) = names
	rownames(scores) = names
	
	diff = mapply(function(i, j) scores[i,j], x, y)

	out = list(score = sum(diff * w) / sum(w))
	out = setMetClassInfo(out, x, y, w, varFun = varFun, itemize = NULL, scoreMat = scores)
	
	out$diff = diff
	out$scoreMat = scores
	
	class(out) = "DMM"
	return(out)
}
