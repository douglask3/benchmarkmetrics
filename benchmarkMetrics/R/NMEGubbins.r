structure.inputs.performNME <- function(x, y, w, 
							            allowRegridding = TRUE, maintainShape = FALSE, ...) {
	c(x, y, w) := structure.inputs(x, y, w, 
								   allowRegridding = allowRegridding, 
								   maintainShape = maintainShape)
	out = setNMEclassVars(x, y, w, ...)	
	return(out)
}

setNMEclassVars <- function(x, y, w, varFun = absVar, ...) {
	out=NMEGubbins(x, y, w, varFun = varFun, ...)
    out=setMetClassInfo(out, x, y, w, varFun = absVar)
    
	class(out) = "NME"
	return(out)
}

NMEGubbins <- function(x, y, w,
                       metFun = NMEForm, varFun = absVar,
                       step1only = FALSE) {
	
	NME1 = metFun(x, y, w)
    
    if (step1only) return(list(score = NME1))
    
	y2   = MeanSub(y, x)
	NME2 = metFun (x, y2, w)
	
    y3   = VarDiv (y2, x, varFun, na.rm = TRUE) 
    y3   = MeanSub(y3, x)
	NME3 = metFun (x, y3, w)
	
	return(list(step1 = NME1, step2 = NME2, step3 = NME3,
                y123 = cbind(y, y2, y3)))
}

VarDiv  <- function(x, y, FUN, ...) mean(y, na.rm = TRUE) + x * FUN(y, ...) / FUN(x, ...)
MeanSub <- function(x, y) x - mean(x, na.rm = TRUE) + mean(y, na.rm = TRUE)
absVar  <- function(x, na.rm = TRUE) mean(abs(x - mean(x, na.rm = na.rm)), na.rm = na.rm)
