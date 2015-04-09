structure.inputs <- function(x,y,w,itemize=FALSE) {	
	x <- as.matrix(x)
	y <- as.matrix(y)
    
	if (is.null(w))  if (itemize) w=array(1,dim(x)[1]) else w=array(1,dim(x))
	
	w=as.matrix(w)
	
	return(list(x,y,w))
}