null.FUN <- function(x,...) UseMethod("null.FUN")

null.FUN.default <- function(x, FUN, n = 1000, items = FALSE, ...) {
	x <- structure.inputs.nulls(x, items)
    
    mscore = meanMod.FUN(x, FUN)
    rscore = randMod.FUN(x, FUN, n, ...)
    
    out = list(meanModel = mscore, randModel = rscore)
    class(out) = "nullModel"
    out$call <- match.call()
    
    return(out)
}