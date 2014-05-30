MMForm 		<- function(x,y,w) sum(abs(w*(x-y)))/sum(w*dim(x)[1])
SCDForm 	<- function(x,y,w) sum(w*(sqrt(x)-sqrt(y))^2)/sum(w*dim(x)[1])