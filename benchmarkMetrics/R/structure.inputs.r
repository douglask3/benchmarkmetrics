structure.inputs <- function(x,y,w,itemize=FALSE) {	    
    x <- as.matrix(x)
	y <- as.matrix(y)
    
    if (!all(dim(x)==dim(y))) y=matchDimensions(x,y)
    
	if (is.null(w))  if (itemize) w=array(1,dim(x)[1]) else w=array(1,dim(x))
	w=as.matrix(w)
    
    if (!itemize) c(x,y,w):=lapply(list(x,y,w),function(i)
                                                matrix(i,ncol=1))
	return(list(x,y,w))
}

matchDimensions <- function(x,y,y0=y,tryC=0) {
    if (tryC==max(sapply(list(x,y), function(i) length(dim(i)))))
        return(y0)
     
    tryC=tryC+1
    if (!any(dim(x)==dim(y))) {
        y=aperm(y)
        matchDimensions(x,y,y0,tryC)
    }
    
    y=t(matrix(rep(y,length.out=length(x)),ncol=nrow(x)))
    return(y)      
}
