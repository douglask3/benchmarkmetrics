MM <- function(x, ...) UseMethod("MM")

MM.default <- function(x, y, w = NULL, ...) {
	out = check.and.norm.performMM(x, y, w, ...)

	out$call <- match.call()

	return(out)
}
