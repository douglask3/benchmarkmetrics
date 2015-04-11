print.listofMetric  <- function(x,...) {
	printSingle.listofMetric <- function(x, ...) {
        nn <- names(x)
        ll <- length(x)
        if (length(nn) != ll) 
            nn <- paste("Component", seq.int(ll))
        for (i in seq_len(ll)) {
            if (i==1) strt="" else strt="\t\t"
            cat(strt,nn[i], ":\n")
            xi=x[[i]]
            if (is.list(xi) && length(xi)==1) xi=xi[[1]]
            if (is.numeric(xi)) {
        	
                if (length(xi)==1) {
                    cat(strt,"\t")
                    cat(strt,standard.round (xi), ...)
                } else print(standard.round (xi), ...)
            }	else cat(strt,"\t",xi)
            cat(strt,"\n\n")
        }
        invisible(x)
	}
	lapply(x,printSingle.listofMetric)
	invisible(x)
}