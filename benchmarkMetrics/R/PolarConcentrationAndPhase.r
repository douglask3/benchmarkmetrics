PolarConcentrationAndPhase <- function(cdata,phase_units="radians",n=dim(cdata)[2]) {
    if (class(cdata)=="numeric") cdata =  t(matrix(cdata))
    a = dim(cdata)[2]
    
    xdata = ydata = matrix(0, dim(cdata)[1], 1)
    for (k in 1:a) {
        angle = 2 * pi * (n - k + 1) / n
        xdata = xdata + cdata[, k] * cos(angle)
        ydata = ydata + cdata[, k] * sin(angle)
    }
    
    adata = apply(cdata,1,sum)
    
    phase = atans(-ydata, xdata, phase_units)
    conc  = sqrt (xdata^2 + ydata^2) / adata
    return(list(phase, conc))
}