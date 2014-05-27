structure.inputsPerform <- function(x,y,w) {	
	x <- as.matrix(x)
	y <- as.matrix(y)
	if (is.null(w)) w=array(1,dim(x)) else w=as.matrix(w)
	
	return(list(x,y,w))
}

structure.inputs <- function(x,y,w) {	
	x <- as.matrix(x)
	y <- as.matrix(y)
	if (is.null(w)) w=array(1,dim(x)) else w=as.matrix(w)
	
	return(list(x,y,w))
}