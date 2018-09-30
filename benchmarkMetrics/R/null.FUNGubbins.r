nullModFun <- function(x, fun, maintainShape = FALSE) {
	if (ncol(x) == 1 || maintainShape) X = fun(x, na.rm = TRUE)
	else X = apply(x, 2, fun, na.rm = TRUE)
	return(X)
}

medianMod.FUN <- function(x, w = NULL, FUN, medianFun = median, maintainShape = FALSE, ...) {
	medianX = nullModFun(x, medianFun)
	if (maintainShape) medianX = NaN
	return(score(FUN(x, medianX, w, ...)))
}

meanMod.FUN <- function(x, w = NULL, FUN, meanFun = mean, maintainShape = FALSE, ...) {
	meanX = nullModFun(x, meanFun, maintainShape)
    return(score(FUN(x, meanX, w, ...)))
}

randMod.FUN <- function(x, w = NULL, FUN, nrs=1000, ...) {
    randComp <- function(i) {

        randX = x[sample(1:dim(x)[1]),]
        return(score(FUN (x, randX, w, ...)))
    }
    return(sapply(1:nrs, randComp))
}

structure.inputs.nulls <- function(x, w, items = FALSE, allowRegridding = TRUE, maintainShape = FALSE) {
    wNtNull = !is.null(w)
    if (allowRegridding && wNtNull &&
        is.raster(x) && is.raster(w))
        c(x, w) := cropInputs(x, w)
    x = setAsMatrix(x, maintainShape = maintainShape)

    if (wNtNull) w = setAsMatrix(w, maintainShape = maintainShape)

    if (!items) {
		if (!maintainShape) {
			x = as.vector(x)
			if (wNtNull) w = as.vector(w)
		}
        x = as.matrix(x)
        if (wNtNull) w = as.matrix(w)
    }

	Mask <- function() {
		mask = !is.na(apply(x,1,sum))
		if (wNtNull) mask = mask & !is.na(apply(w,1,sum))

		x = as.matrix(x[mask,])
		if (wNtNull && length(w) > 1) w = as.matrix(w[mask,])
		return(list(x, w))
	}
	
	if (!maintainShape) {
		MaskOut = try(Mask(), silent = TRUE)
		if (class(MaskOut) != "try-error") c(x, w) := MaskOut
	}
	
    return(list(x, w))
}
