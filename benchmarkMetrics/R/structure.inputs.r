structure.inputs <- function(x,y,w,itemize=FALSE) {	
    x <- as.matrix(x)
	y <- as.matrix(y)
    
	if (is.null(w))  if (itemize) w=array(1,dim(x)[1]) else w=array(1,dim(x))
	w=as.matrix(w)
    
    if (!itemize) c(x,y,w):=lapply(list(x,y,w),function(i) matrix(i,ncol=1))
    
	return(list(x,y,w))
}

