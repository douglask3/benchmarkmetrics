sum_numerator <- function(score, w, sum_numerator = TRUE) {
	if (sum_numerator) score = mean(score * w) else score = score * mean(w)
	return(score)
}

NMEForm  <- function(x, y, w, sum_numerator = TRUE) {
	score = abs(y - x)/mean(w * abs(x - mean(x)))
	score = sum_numerator(score, w, sum_numerator)
	return(score)
}

NMSEForm <- function(x, y, w, sum_numerator = TRUE) {
	score = ((y - x)^ 2) / mean(w * (x - mean(x))^ 2)
	score = sum_numerator(score, w, sum_numerator)
	return(score)
}

NMGEForm <- function(x, y, w, sum_numerator = TRUE) {
	getXY <- function(r) {	
		nc = ncol(r); nr = nrow(r)
		
		rx = r[c(2:nr,1), ] - r
		ry = r[, c(2:nc,1)] - r
		return(list(rx, ry))
	}
	
	x_xy = getXY(x)
	if (is.null(y))
		y_xy = lapply(x_xy, function(i) {i[,] = median(i, na.rm = TRUE); i})
	else
		y_xy = getXY(y)
	
	sqrdDist <- function(i, x = x_xy, y = y_xy) 
		(x[[i]] - y[[i]])^2
		
	
	num = sqrt(sqrdDist(1) + sqrdDist(2))
	
	y_xy = lapply(y_xy, function(i) {i[,] = 0; i})
	dom = sqrt(sqrdDist(1) + sqrdDist(2))
	
	score = num / mean(w * dom, na.rm = TRUE)
	
	score = sum_numerator(score, w, sum_numerator)
	return(score)
}