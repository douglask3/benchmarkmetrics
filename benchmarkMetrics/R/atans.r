atans <- function(x,y,units='months',revs=FALSE) {
    if (class(x)=="RasterLayer") phase_out=x[[1]]
    
    if (class(x)=="RasterLayer") x=as.matrix(x)
    if (class(y)=="RasterLayer") y=as.matrix(y)
    
    phase=atan2(x,y)
    
    if (units=='months') {
      phase=6*(phase/pi)+6;
    } else if (units=='degrees') { 
      phase=phase
      phase=phase*360/(2*pi)
    } else if (units=='radians') phase=phase
            
    if (class(x)=="RasterLayer") {
    	values(phase_out)=phase
    	return(phase_out)
    } else {
    	return(phase)
    }
}
