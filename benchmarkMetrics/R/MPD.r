MPD <- function(x, ...) UseMethod("MPD")

MPD.default <- function(x, y, w = NULL, allowRegridding = TRUE, ...) {
	
	c(x, y, w) := structure.inputs(x, y, w, , allowRegridding = allowRegridding, itemize = TRUE)
	out = setMPDclassVars(x, y, w, ...)	
	out$call <- match.call()
	return(out)
}

