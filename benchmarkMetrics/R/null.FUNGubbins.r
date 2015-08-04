meanMod.FUN <- function(x, w = NULL, FUN, ...) {
    meanX = apply(x, 2, mean)

    return(score(FUN(x, meanX, w, ...)))
}

randMod.FUN <- function(x, w = NULL, FUN, nrs=1000, ...) {
    randComp <- function(i) {

        randX = x[sample(1:dim(x)[1]),]
        return(score(FUN (x, randX, w, ...)))
    }
    return(sapply(1:nrs, randComp))
}

structure.inputs.nulls <- function(x, w, items = FALSE, allowRegridding = TRUE) {
    wNtNull = !is.null(w)
    if (allowRegridding && wNtNull &&
        is.raster(x) && is.raster(w))
        c(x, w) := cropInputs(x, w)
    x = setAsMatrix(x)
    
    if (wNtNull) w = setAsMatrix(w)

    if (!items) {
        x = as.matrix(as.vector(x))
        if (wNtNull) w = as.matrix(as.vector(w))
    }

    mask = !is.na(apply(x,1,sum))
    if (wNtNull) mask = mask & !is.na(apply(w,1,sum))

    x = as.matrix(x[mask,])
    if (wNtNull) w = as.matrix(w[mask,])
    return(list(x, w))
}
