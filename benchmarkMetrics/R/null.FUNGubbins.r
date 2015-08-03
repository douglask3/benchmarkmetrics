meanMod.FUN <- function(x, FUN, ...) {
    meanX = apply(x, 2, mean)

    return(score(FUN(x, meanX, ...)))
}

randMod.FUN <- function(x, FUN, nrs=1000, ...) {
    randComp <- function(i) {
        randX = x[sample(1:dim(x)[1]),]
        return(score(FUN (x, randX, ...)))
    }
    return(sapply(1:nrs, randComp))
}

structure.inputs.nulls <- function(x, items = FALSE) {
    x = setAsMatrix(x)
    if (!items) x = as.matrix(as.vector(x))
    x = as.matrix(x[!is.na(apply(x,1,sum)),])
    return(x)
}
