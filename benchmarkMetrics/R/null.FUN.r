null.FUN <- function(x, ...) UseMethod("null.FUN")

null.FUN.default <- function(x, FUN, n = 1000, items = FALSE,
	                         step1only = c(TRUE, TRUE),  ...) {

	x <- structure.inputs.nulls(x, items)

    mscore = meanMod.FUN(x, FUN,    step1only = step1only[1], ...)
    rscore = randMod.FUN(x, FUN, n, step1only = step1only[2], ...)

    out = list(meanModel = mscore, randModel = rscore)
    class(out) = "nullModel"
    out$call <- match.call()

    return(out)
}
