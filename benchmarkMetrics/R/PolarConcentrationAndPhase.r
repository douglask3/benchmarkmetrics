PolarConcentrationAndPhase <- function(x, ...)
    UseMethod("PolarConcentrationAndPhase")

PolarConcentrationAndPhase.default <-
    function(cdata, phase_units = "radians",  ncycle = NULL) {
    if (class(cdata) == "numeric") cdata =  t(matrix(cdata))
	if (is.null(ncycle)) ncycle = dim(cdata)[2]
    xdata = ydata = matrix(0, dim(cdata)[1], 1)
	
    for (k in 1:ncycle) {
        angle = 2 * pi * (ncycle - k + 1) / ncycle
        xdata = xdata + cdata[, k] * cos(angle)
        ydata = ydata + cdata[, k] * sin(angle)
    }
	
    adata = apply(cdata, 1, sum)

    phase = atans(-ydata, xdata, phase_units)
    conc  = sqrt (xdata^2 + ydata^2) / adata
    return(list(phase, conc))
}
