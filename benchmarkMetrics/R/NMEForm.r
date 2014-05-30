NMEForm <- function(x,y,w) sum(w*abs(y-x))/sum(w*abs(x-mean(x)))
NMSEForm <- function(x,y,w) sum(w*(y-x)^2)/sum(w*(x-mean(x))^2)