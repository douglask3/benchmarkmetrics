atans <- function(x,y,units='months') {
    if (class(x)=="RasterLayer") phase_out=x[[1]]
    
    if (class(x)=="RasterLayer") x=as.matrix(x)
    if (class(y)=="RasterLayer") y=as.matrix(y)
    
    phase=atan(x/y)
    
    test=(x>0)+(y<0);
    phase[which(test==2)]=pi+phase[which(test==2)];

    test=(x==0)+(y<0);
    phase[which(test==2)]=pi;

    test=(x<0)+(y<0);
    phase[which(test==2)]=-pi+phase[which(test==2)];

    test=(x<0)+(y==0);
    phase[which(test==2)]=-pi/2;

    if (units=='months') {
      phase=6*(phase/pi)+6;
    } else if (units=='degrees') { 
      phase=phase+pi
      phase=phase*360/(2*pi)
    } else if (units=='radians') phase=phase+pi
            
    if (class(x)=="RasterLayer") {
    	values(phase_out)=phase
    	return(phase_out)
    } else {
    	return(phase)
    }
}
