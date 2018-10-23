MMForm 		<- function(x, y, w, sum_numerator = TRUE) {
	#score = sum(abs(w * (x - y))) / MMnormFact(x, w) 
	score = abs((x - y)) / MMnormFact(x, w)
	score = sum_numerator(score, w, sum_numerator)
	return(score)
}
	

SCDForm 	<- function(x, y, w, sum_numerator = TRUE) {
	#score = sum(w * (sqrt(x) - sqrt(y)) ^ 2) / MMnormFact(x, w)
	score = abs((x - y)) / MMnormFact(x, w)
	score = sum_numerator(score, w, sum_numerator)
	return(score)
}

MMnormFact  <- function(x, w) (mean(w) / dim(x)[2])


NMEForm  <- function(x, y, w, sum_numerator = TRUE) {
	score = abs(y - x)/mean(w * abs(x - mean(x)))
	score = sum_numerator(score, w, sum_numerator)
	return(score)
}