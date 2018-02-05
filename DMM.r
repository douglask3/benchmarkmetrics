library(gitBasedProjects)
sourceAllLibs('benchmarkMetrics/R/')

mat = 'benchmarkMetrics/affinityMat.csv'


DMM.point <- function(mat, b1, b2, matAsFile = is.character(matFile), row1AsNames = is.character(mat[1,1])) {
	if (matAsFile) mat = read.csv(mat, stringsAsFactors = FALSE)
	
	if (row1AsNames) {
		names = mat[,1]
		mat = mat[,-1]
	} else names = 1:(dim(mat)[1])
	
	return(MM(mat[b1, ], mat[b2, ]))
}


DMM <- function(mat, x, y, w, matAsFile = is.character(matFile), row1AsNames = is.character(mat[1,1])) {

	if (matAsFile) mat = read.csv(mat, stringsAsFactors = FALSE)
	
	index = 1:(dim(mat)[1])
	
	if (row1AsNames) {
		names = mat[,1]
		mat = mat[,-1]
	} else names = index
	
	scores = sapply(index, function(...) sapply(index, function(b1, b2) score(DMM.point(mat, b1, b2, FALSE, FALSE)), ...))	
	colnames(scores) = names
	rownames(scores) = names
	
	diff = mapply(function(i, j) scores[i,j], x, y)
	return(sum(diff) / length(x))

}

obs = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4)
sim = c(1, 2, 3, 3, 4, 2, 1, 2, 5, 2, 4, 5)


score = DMM(mat, obs, sim)