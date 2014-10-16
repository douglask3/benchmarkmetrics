\name{MM}
\alias{MM}
\alias{MM.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Manhattan Metric}
\description{Performs the Manhattan Metric comparison between modelled and observed data on fractional-type items.}
\usage{MM(x,y,w=NULL)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{matrix,array, data.frame, raster or raster stack,representing a list of observations. 
           The last dimension represents the fractional items, whilst all other dimenstions 
           represent different realisations of these items}
  \item{y}{matrix,array, data.frame, raster or raster stack of same size as x representing modelled}
  \item{w}{vector, matrix or raster of weights. If NULL, all items are considered equally. 
           If a vector, must by the same length as first dimension of x and represents the
           weight of each point. If matrix or raster, than weight for each point and item.}
}
\details{if x and y are 2-dimensional, rows represent 'points', columns represent items. Items for each
         'point' are normalised before metric comparison is made.}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, ...) 
UseMethod("MM")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
