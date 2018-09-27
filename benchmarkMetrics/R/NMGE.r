NMGE <- function(x,...) UseMethod("NMGE")

NMGE.default <- function(x, y, w = NULL, ...) {
	out = structure.inputs.performNME(x, y, w,
                                      metFun = NMGEForm, varFun =sd, maintainShape = TRUE, ...)
	out$call <- match.call()
	return(out)
}

