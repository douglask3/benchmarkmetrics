check.and.norm.performDMM <- function(mat, x, y, w, 
								      matAsFile = is.character(matFile), 
									  row1AsNames = is.character(mat[1,1]), 
									  traitID = TRUE, ...) {
	
	c(x, y, w) := structure.inputs(x, y, w)

	
	if (matAsFile) mat = read.csv(mat, stringsAsFactors = FALSE)
	
	index = 1:(dim(mat)[1])
	
	if (row1AsNames) {
		names = mat[,1]
		mat = mat[,-1]
	} else names = index
	
	normaliseTrait <- function(index) 
		mat[index] / apply(mat[index], 1, sum)
	
	if (!is.null(traitID)) 
	if (is.logical(traitID) && traitID) {
		nTraits = dim(mat)[2]
		mat = normaliseTrait(1:nTraits)
	} else if (!is.logical(traitID)) {
		IDs = unique(traitID)
		for (i in unique(traitID)) {
			index = traitID == i
			mat[index] = normaliseTrait(index)
		}
		nTraits = length(IDs)
	}

	
	out = setDMMclassVars(mat, names, nTraits, x, y, w, ...)
	return(out)
}

#DMMForm <- function(x, y) {
#	sum(abs(mat[b1,] - mat[b2,])) / sum(mat[b1,] + mat[b2,])
	
#DSCDForm <- function(x, y) {
	

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
