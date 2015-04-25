ts2matrix <- function(dat, calendar=TRUE,...) {
    tdat = time(dat)
    fdat = frequency(dat)
    
    if     (calendar && fdat == 4 ) cnames = c("Q1", "Q2", "Q3", "Q4")
    else if(calendar && fdat == 12) cnames = month.abb
    else                            cnames = tdat[1:fdat] - floor(tdat[1])

    rnames = tdat[seq(1, length(dat), by = fdat)]
    
    dat=matrix(unclass(dat), ncol = fdat,...)
    rownames(dat) = rnames
    colnames(dat) = cnames
    
    return(dat)
}