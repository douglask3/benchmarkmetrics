check.and.norm.performMM <- function(x, y, w, ...) {
	c(x, y, w) := structure.inputs(x, y, w , itemize = TRUE)
	
	if (dim(x)[2]<2 && dim(y)[2]<2)
	    stop("2 or more items required")

	w0 = w
	if (dim(w)[2] == 1) for (i in 1:(dim(x)[2] - 1)) w = r = cbind(w, w0)

	
	x = normalise(x)
	y = normalise(y)
	
    c(x, y, w) := removeNaNs(x, y, w)

	out = setMMclassVars(x, y, w, ...)
	return(out)
}

setMMclassVars <- function(x, y, w, varFun = absVar, metFun = MMForm,
	                       step1only = NULL, ...) {

	out = list(score = metFun(x, y, w, ...))
	out = setMetClassInfo(out, x, y, w, varFun = varFun, itemize = TRUE)
	class(out) = "MM"
	return(out)
}

normalise <- function(x) sweep(x, 1, rowSums(x), '/')

removeNaNs <- function(x, y, w) {
    test = apply(is.na(x + y + w), 1, sum) == 0
	if (length(test) > 1) {
		removeNaN <- function(x) x[test, ]
		c(x, y, w) := lapply(list(x, y, w), removeNaN)
	}
    return(list(x, y, w))
}
