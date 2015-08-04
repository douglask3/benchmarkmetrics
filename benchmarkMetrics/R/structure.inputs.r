structure.inputs <- function(x, y, w, itemize = FALSE, na.rm = TRUE,
                             allowRegridding = TRUE) {

    if (allowRegridding &&is.raster(x) && is.raster(y))
        c(x, y, w) := cropInputs(x,y,w)

    x = setAsMatrix(x)
	y = setAsMatrix(y)

    if (is.null(w))
        if (itemize) w = array(1, dim(x)[1]) else w = array(1,dim(x))

    w = setAsMatrix(w)

    if (!all(dim(x) == dim(y))) y = matchDimensions(x, y)
    if (length(w) == 1) w = array(w,dim(x))

    if (!itemize) {
        nc = ncol(x)
        c(x, y, w) := lapply(list(x, y, w), function(i)
                                                matrix(i, ncol = 1))
        if (nc*length(w) == length(x)) w = as.matrix(rep(w, nc))
    }

	if (na.rm) {
        test = !is.na(apply(x, 1, sum) + apply(y, 1, sum)+ apply(w, 1, sum))

        x = as.matrix(x[test, ])
        y = as.matrix(y[test, ])
        w = as.matrix(w[test, ])
    }

    return(list(x, y, w))
}

cropInputs <- function(x, y, w) {
    resampleIns <- function(r1, r2) {
        if (all(res(r1) == res(r2))) return(list(r1,r2))
        if (sum(res(r1)) > sum(res(r2))) r2 = resample(r2, r1)
            else r1 = resample(r1, r2)
        return(list(r1, r2))
    }

    cropIns <- function(r1, r2) {
        c(r1, r2) := resampleIns(r1, r2)
        if (extent(r1) == extent(r2)) return(list(r1, r2))

        r1 = crop(r1, r2)
        r2 = crop(r2, r1)
        return(list(r1, r2))
    }
    
    c(x, y) := cropIns(x, y)
    if (!is.null(w) && is.raster(w)) {
        c(x, w) := cropIns(x, w)
        c(y, w) := cropIns(y, w)
    }

    return(list(x, y, w))
}



setAsMatrix <- function(x) {
    if (class(x) == "ts") return(ts2matrix(x)) else {
        if (is.raster(x)) x = values(x)
        return(as.matrix(x))
    }
}

matchDimensions <- function(x, y, y0 = y, tryC = 0) {
    if (tryC == max(sapply(list(x, y), function(i) length(dim(i)))))
        return(y0)

    tryC = tryC + 1
    if (!any(dim(x) == dim(y))) {
        y = aperm(y)
        matchDimensions(x, y, y0, tryC)
    }

    y = t(matrix(rep(y, length.out = length(x)), ncol = nrow(x)))
    return(y)
}
