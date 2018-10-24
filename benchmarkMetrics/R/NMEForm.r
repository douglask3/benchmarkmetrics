sum_numerator <- function(score, w, sum_numerator = TRUE, ...) {
	if (sum_numerator) score = mean(score * w, ...) else score = score * mean(w, ...)
	return(score)
}

NMEForm  <- function(x, y, w, sum_numerator = TRUE) {
	score = abs(y - x)/mean(w * abs(x - mean(x)))
	score = sum_numerator(score, w, sum_numerator)
	return(score)
}

NMDEForm  <- function(x, y, w, scale_fact = 0.002, max_scale_fact = NULL, sum_numerator = TRUE) {
	
	moveXY <- function(r, dx = 1, dy = 1) {	
		nc = ncol(r); nr = nrow(r)
		if (dx == 0) xindex = 1:nr else xindex = c((dx+1):nr,1:dx)
		if (dy == 0) yindex = 1:nc else yindex = c((dy+1):nc,1:dy)
		rx = r[xindex, yindex]
		return(rx)
	}
	
	measure_shift <- function(dx = 1, dy = 1) {
		ds = scale_fact * sqrt(dx^2 + dy^2)
		
		if (ds > max_scale_fact) return(list(diff_d, diff_n))
		x_xy = moveXY(x, dx, dy)
		
		new_distance <- function(yi, diff) {
			dv = x_xy - yi
			dv = sqrt(dv^2 + ds^2)
			test = dv < diff
			test[is.na(test)] = FALSE
			diff[test] = dv[test]
			return(diff)
		}
		
		diff_d = new_distance(y, diff_d)
		diff_n = new_distance(xmean, diff_n)
		return(list(diff_d, diff_n))
	}
	
	diff_d = diff0_d = abs(x - y)
	
	xmean = sum(w * x, na.rm = TRUE)/sum(w)
	diff_n = diff0_n = abs(x - xmean)
	
	if (is.null(max_scale_fact)) max_scale_fact = max(c(diff_d, diff_n), na.rm = TRUE)
	index = 0:(max_scale_fact/scale_fact)
	
	for (dx in index) for (dy in index) c(diff_d, diff_n) := measure_shift(dx, dy)	
	
	score = diff_d/mean(w * diff_n, na.rm = TRUE)
	score1 = sum_numerator(score, w, TRUE, na.rm = TRUE)
	score2 = as.vector(sum_numerator(score, w, FALSE, na.rm = TRUE))
	return(list(score1, score2))
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
	
	score = sum_numerator(score, w, sum_numerator, na.rm = TRUE)
	if (!sum_numerator)  score = as.vector(score)
	return(score)
}