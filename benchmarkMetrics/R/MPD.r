MPD <- function(x, ...) UseMethod("MPD")

MPD.default <- function(x, y, w = NULL, ...) {
	
	c(x, y, w) := structure.inputs(x, y, w, itemize = TRUE)
	out = setMPDclassVars(x, y, w, ...)	
	out$call <- match.call()
	return(out)
}

