null.FUN <- function(x,...) UseMethod("null.FUN")

null.FUN.default <- function(x, FUN, n = 1000, items = FALSE, ...) {
	x <- structure.inputs.nulls(x, items)
    
    mscore = meanMod.FUN(x, FUN,    ...)
    rscore = randMod.FUN(x, FUN, n, ...)
    
    out = list(meanModel = mscore, randModel = rscore)
    class(out) = "nullModel"
    out$call <- match.call()
    
    return(out)
}

meanMod.FUN <- function(x, FUN, ...) {
    meanX = apply(x, 2, mean)
    return(score(FUN(x, meanX, ...)))
}

randMod.FUN <- function(x, FUN, nrs=1000, ...) {
    randComp <- function(i) {
        randX = x[sample(1:dim(x)[1]),]
        return(score(FUN (x, randX, ...)))
    }
    return(sapply(1:nrs,randComp))
}

structure.inputs.nulls <- function(x, items = FALSE) {
    if (items) x = as.matrix(x) else x = as.matrix(as.vector(x))
    return(x)
}