NMDE <- function(x,...) UseMethod("NMDE")

NMDE.default <- function(x, y, w = NULL, ...) {
	out = structure.inputs.performNME(x, y, w,
                                      metFun = NMDEForm, varFun =mean, maintainShape = TRUE, ...)
	out$call <- match.call()
	return(out)
}

