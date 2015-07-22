structure.inputs <- function(x, y, w, itemize = FALSE, na.rm = TRUE) {	    
    x <- setAsMatrix(x)
	y <- setAsMatrix(y)
    
    if (!all(dim(x) == dim(y))) y = matchDimensions(x, y)
    
	if (is.null(w))
        if (itemize) w = array(1, dim(x)[1]) else w = array(1,dim(x))
	w = as.matrix(w)
    
    if (!itemize) c(x, y, w) := lapply(list(x, y, w), function(i)
                                                matrix(i, ncol = 1))
	if (na.rm) {
        test = !is.na(apply(x, 1, sum) + apply(y, 1, sum))
        
        x = as.matrix(x[test, ])
        y = as.matrix(y[test, ])
        w = as.matrix(w[test, ])
    }
    return(list(x, y, w))
}

setAsMatrix <- function(x) 
    if (class(x) == "ts") return(ts2matrix(x)) else return(as.matrix(x))

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
