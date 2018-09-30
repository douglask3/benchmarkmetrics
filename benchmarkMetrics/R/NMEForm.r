NMEForm  <- function(x, y, w) sum(w * abs(y - x)) / sum(w * abs(x - mean(x)))

NMSEForm <- function(x, y, w) sum(w * (y - x)^ 2) / sum(w * (x - mean(x))^ 2)

NMGEForm <- function(x, y, w) {
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
	
	res = sum(w * num, na.rm = TRUE) / sum(w * dom, na.rm = TRUE)
}