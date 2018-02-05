DMM <- function(mat, x, ...) UseMethod("DMM")

DMM.default <- function(mat, x, y, w = NULL, ...) {
	out = check.and.norm.performDMM(mat, x, y, w, ...)

	out$call <- match.call()

	return(out)
}

