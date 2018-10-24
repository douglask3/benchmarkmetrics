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
	w = as.matrix(as.vector(w))
    out=setMetClassInfo(out, x, y, w, varFun = absVar)
    
	class(out) = "NME"
	return(out)
}

NMEGubbins <- function(x, y, w,
                       metFun = NMEForm, varFun = absVar,
                       step1only = FALSE, ...) {
	
	run_with_diff <- function(yi) {
		NME     = metFun(x, yi, w, ...)
		if (class(NME) == "list" &&  length(NME) ==2) {
			NME_pnt = NME[[2]]
			NME = NME[[1]]
		} else NME_pnt = metFun(x, y, w, FALSE, ...)
		
		return(list(NME, NME_pnt))
	}
	
	c(NME1, NME1_pnt) := run_with_diff(y)
    if (step1only) return(list(score = NME1))
    
	y2   = MeanSub(y, x)
	c(NME2, NME2_pnt) := run_with_diff(y2)
	
    y3   = VarDiv (y2, x, varFun, na.rm = TRUE) 
    y3   = MeanSub(y3, x)
	c(NME3, NME3_pnt) := run_with_diff(y3)
	
	return(list(step1 = NME1, step2 = NME2, step3 = NME3,
                y123 = sapply(list(y, y2, y3), as.vector), diff = cbind(NME1_pnt, NME2_pnt, NME3_pnt), x = x))
}

VarDiv  <- function(x, y, FUN, ...) mean(y, na.rm = TRUE) + x * FUN(y, ...) / FUN(x, ...)
MeanSub <- function(x, y) x - mean(x, na.rm = TRUE) + mean(y, na.rm = TRUE)
absVar  <- function(x, na.rm = TRUE) mean(abs(x - mean(x, na.rm = na.rm)), na.rm = na.rm)
