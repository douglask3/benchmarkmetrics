print.listofMetric  <- function(x,...) {
	printSingle.listofMetric <- function(x, ...) {
	nn <- names(x)
    ll <- length(x)
    if (length(nn) != ll) 
        nn <- paste("Component", seq.int(ll))
    for (i in seq_len(ll)) {
        cat(nn[i], ":\n")
        
        if (is.numeric(x[[i]])) {
        	
        	if (length(x[[i]])==1) {
        		cat("\t")
        		cat(standard.round (x[[i]]), ...)
        	} else print(standard.round (x[[i]]), ...)
        }	else cat("\t",x[[i]])
        cat("\n\n")
    }
    invisible(x)
	
	}

	lapply(x,printSingle.listofMetric)
	invisible(x)
}