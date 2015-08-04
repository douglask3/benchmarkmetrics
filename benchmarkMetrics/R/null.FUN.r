null.FUN <- function(x, ...) UseMethod("null.FUN")

null.FUN.default <- function(x, FUN, w = NULL, n = 1000, items = FALSE,
	                         step1only = c(TRUE, TRUE),  ...) {

	c(x, w) := structure.inputs.nulls(x, w, items)

    mscore = meanMod.FUN(x, w, FUN,    step1only = step1only[1], ...)
    rscore = randMod.FUN(x, w, FUN, n, step1only = step1only[2], ...)

    out = list(meanModel = mscore, randModel = rscore)
    class(out) = "nullModel"
    out$call <- match.call()

    return(out)
}
