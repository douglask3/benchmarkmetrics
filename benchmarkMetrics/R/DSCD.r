DSCD <- function(mat, x,...) UseMethod("DSCD")

DSCD.default <- function(mat, x, y, w = NULL, ...) {
	out = check.and.norm.performDMM(mat, x, y, w, varFun = sVarDiv, metFun = SCDForm, ...)
	out$call <- match.call()
	return(out)
}