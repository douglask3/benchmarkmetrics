structure.inputs.performNME <- function(x,y,w,...) {
	c(x,y,w):=structure.inputs(x,y,w)
	out=setNMEclassVars(x,y,w,...)	
	return(out)
}
