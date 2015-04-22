MMForm 		<- function(x, y, w) sum(abs(w * (x - y))) / MMnormFact(x, w)
SCDForm 	<- function(x, y, w) sum(w * (sqrt(x) - sqrt(y)) ^ 2) / MMnormFact(x, w)

MMnormFact  <- function(x, w) (sum(w) / dim(x)[2])