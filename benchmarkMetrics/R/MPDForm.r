MPDForm <- function(x, y, w, sum_numerator = TRUE) {
	score = ((1 / pi) * w * acos(cos(y-x))) / mean(w)
	score = ((1 / pi) * acos(cos(y-x)))
	if (sum_numerator) score = sum(w * score) / sum(w)
	return(score)
}
