NMSE <- function(x,...) UseMethod("NMSE")

NMSE.default <- function(x, y, w = NULL, ...) {
	out = structure.inputs.performNME(x, y, w,
                                      metFun = NMSEForm, varFun =sd, ...)
	out$call <- match.call()
	return(out)
}

