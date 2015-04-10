structure.inputs.performNME <- function(x,y,w,...) {
	c(x,y,w):=structure.inputs(x,y,w,asVector=TRUE)
	out=setNMEclassVars(x,y,w,...)	
	return(out)
}
