null.FUN <- function(x, ...) UseMethod("null.FUN")

null.FUN.default <- function(x, FUN, w = NULL, n = 1000, items = FALSE,
	                         step1only = c(TRUE, TRUE),  ...) {

	c(x, w) := structure.inputs.nulls(x, w, items)

	mediScore = medianMod.FUN(x, w, FUN,    step1only = step1only[1], ...)
    meanScore =   meanMod.FUN(x, w, FUN,    step1only = step1only[1], ...)
    randScore =   randMod.FUN(x, w, FUN, n, step1only = step1only[2], ...)

    out = list( medianModel = mediScore,
	              meanModel = meanScore, 
				  randModel = randScore)
    class(out) = "nullModel"
    out$call <- match.call()

    return(out)
}
