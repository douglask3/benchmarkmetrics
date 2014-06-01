seasonal_concentration_and_phase <- function(cdata,phase_units="radians") {

    a=dim(cdata)[2]
    
    xdata=ydata=matrix(0,dim(cdata)[1],1)
    for (k in 1:a) {
        angle=2*pi*(12-k+1)/12
        xdata=xdata+cdata[,k]*cos(angle)
        ydata=ydata+cdata[,k]*sin(angle)
    }
    adata=sum(cdata)
    
    pdata=atans(0-ydata,0-xdata,phase_units)
    cdata=sqrt(xdata^2+ydata^2)/adata
    list(pdata,cdata)
}