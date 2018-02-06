\name{DMM}
\alias{DMM}
\alias{DMM.default}
\alias{DSCD}
\alias{DSCD.default}
\alias{print.DMM}
\alias{summary.DMM}
\alias{plot.DMM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Discrete Manhattan Metric/ Discrete Square Chord Distance}
\description{Performs the Discrete Manhattan Metric (DMM) and Discrete Square Chord Distance (DSCD) comparison between modelled and observed data on Discrete, trait based items.}
\usage{
  DMM (mat, x, y, w = NULL,
       matAsFile = is.character(mat), row1AsNames = is.character(mat[1,1]), 
	   traitID = TRUE, allowRegridding = TRUE)
  DSCD(mat, x, y, w = NULL,
       matAsFile = is.character(mat), row1AsNames = is.character(mat[1,1]), 
	   traitID = TRUE, allowRegridding = TRUE)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat}{filename or matrix describing items in terms of affinity to traits. 
		     If filename, \code{mat} should point to a csv file (e.g. 
			 \link{https://raw.githubusercontent.com/douglask3/benchmarkmetrics/master/online-resource/biomeAffinityMatrix.csv}). 
			 First column can 
			 be name of item when \code{row1AsNames} is set to \code{TRUE}}
  \item{x, y}{matrix, data.frame, raster or raster stack with either 
			  names matching items in \code{mat} or numbers 
			  corrisoponding to item in equivlent row in \code{mat} 
			  r where y is the approximation to x.}
  \item{w}{vector or raster of weights. If NULL, all items are considered equally.
           If a vector, must by the same length as first dimension of x and represents the
           weight of each point. If matrix or raster, than weight for each point and item.}
  \item{matAsFile}{logical, which should be set to \code{TRUE} if mat is points to a csv file. 
				   Default of \code{TRUE} if \code{mat} is a character}
  \item{row1AsNames}{logical, set to \code{TRUE} when first column in \code{mat} is the name of the item}
  \item{traitID}{NULL, logical and numeric vector of length of number of traits. Describes trait
				 grouping. If numeric vector, describes which traits are in the same ``trait group'' 
				 (i.e, hot trait cold trait are in temperature group). Each trait group is normalised for each item
				 (i.e. for a given item, affinity to hot and cold traits will add up to 1). 
				 If \code{TRUE}, each trait is considered to be in different groups, but normalising is still conducted over all traits at once.
				 If \code{FALSE}, each all traits are considered as the same group, and normalising is conducted over all traits.
				 If \code{NULL}, each trait is considered to be in its own group, and no normalising occurs.}
  \item{allowRegridding}{Logical arguement used if x and y are both raster.
           If TRUE and x and y are on different grids, then x and y are cropped
           to the smallest shared extent and lowest reslution of either x and y.
           If w is raster as well, this will also be regridded to smallest extent and lowest
           resolution of x or y.}
}
\details{DMM measure the absolute distamce between simulated and observed items when items are described by affinity to traits.

The “closeness” of two items (a and b) is the total difference in affinity scores for all traits, normalised by the total affinity of the items in question, equilvalent to twice the number of trait groups (T):
\deqn{d(a, b) = \Sigma |x_{a, j} - x{b, j}| / T}
When items are exactly the same (i.e when a=b), d = 0. If biomes are completely different, d = 2. The performance of a model  \deqn{mod = b_1, b_2, ..., b_p} simulated items corresponding to \beqn{obs = a_1, a_2, ..., a_p} observations is the mean of all data point comparisons, i.e:
\deqn{DMM(obs, mod) = \Sigma d(a_i,b_i) / p}
For a perfect model, BD=0. For a completely dissimilar model, BD = 2. As the metric is a linear combination of the difference in affinity scores, improvements in scores are directly proportional to improvement in model skill.

For Discrete Square Chord Distance (\code{DSCD}), \dequn{d} is the squared version similar to \code{\link{SCD}}:
\deqn{d(a, b) = \Sigma (x_{a,j}^0.5 - y_{a,j}^0.5)^2 / T}

}
\value{
DMM and DSCD returns an object of class "DMM".

The  \code{\link{print}} print function returns the call information and score

The function \code{\link{summary}} returns the most common item type for x and y,  a "comparison table" with where x items (colnames) are found in y (rows), a "score table" with the score each of the biome comparisons have, and a "score contribution" table, which determines which biome assigmnets from x to y corrispond to the overall score (i.e, score table * score contribution table).

The \code{\link{plot}} function plots the observed items (x axis), and the scores (y-axis) against the labelled items represented by points. The size of the points indicates the number of times item x is assigned to item y.

\code{\link{boxplot}} plots a box and whisker plot showing median and (IQ) ranges of scores for each veg type found in x.

\code{\link{score}} returns the metric score.

}

\references{
To follow
}

\author{Douglas Kelley \email{douglas.i.kelley@gmail.com}}

\seealso{
\code{\link{print.DMM}}, \code{\link{summary.DMM}}, \code{\link{plot.DMM}},
\code{\link{MM}}, \code{\link{SCD}}
}
\examples{
  
	###################
	## Set up inputs ##
	###################
	## biomeAffinityMatrix is an example dataset provided as part of the package
	print(biomeAffinityMatrix)

	## bit of a random example of items numbers
	obs = c(1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 4, 4, 4)
	sim = c(1, 2, 3, 3, 4, 2, 1, 2, 3, 2, 1, 1, 2, 3, 3, 5, 2, 4, 5)

	####################
	## run and output ##
	####################
	out = DMM(biomeAffinityMatrix, obs, sim)
	
	print(out)
	print(score(out))
	print(summary(out))	
	
	## can feed in biome names instead
	obs = biomeAffinityMatrix[obs,1]
	sim = biomeAffinityMatrix[sim,1]	
	print(rbind(obs = obs, sim = sim))
	
	## to get the same result
	out = DMM(biomeAffinityMatrix, obs, sim)
	print(out)
	
	##################
	## plot results ##
	##################
	par(mfrow = c(2,1), mar = c(2, 4, 0, 1))
	boxplot(out)
	plot(out)
	
	#########################
	## Run the null models ##
	#########################
	null = null.DMM(biomeAffinityMatrix, obs, n = 100) ## iteractions set to 100 to speed up example
	print(summary(null))
	dev.new()
	plot(null, legend = TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~benchmarking }
\keyword{ ~metric }% __ONLY ONE__ keyword per line
\keyword{ ~MM}
