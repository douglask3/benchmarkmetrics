standard.round <- function(x, ...) sapply(x, standard.round.ind, ...)

standard.round.ind <- function(x, n = 2) {
	if (is.na(x) || is.infinite(x) || x == 0) 	return(x)
	ndig = find.ndig(x)
	
	if (ndig == 0)  return(signif(x,n))
	if (ndig == 1)  return(round(x,n))
	
	return(round(x, nround))
}


find.ndig <- function(x) {
	i = 0
	while (x > 1) {x = x / 10; i = i + 1}
	return(i)
}
