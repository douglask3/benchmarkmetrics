ts2matrix <- function(x) {
    f = frequency(x)
    t = tsp(x)
    y = t(matrix(x, nrow = f))

    if      (f == 12) colnames(y) = month.abb
    else if (f ==  4) colnames(y) = c('Qtr1', 'Qtr2', 'Qtr3', 'Qtr4')
    else              colnames(y) = 1:ncol(y)
    
    rownames(y) = seq(t[1], t[2] + 1/f - 1, length.out = nrow(y))

    return(y)
}