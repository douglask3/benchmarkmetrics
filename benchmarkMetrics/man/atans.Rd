\name{atans}
\alias{atans}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Performing a full circle arc-tangent}
\description{The arc-tangent of two vectors, x and y, returns the angle between the x-axis and the vector from the origin to (x, y), i.e., for positive arguments atan2(y, x) == atan(y/x). Equivalent to atan2, but allows conversion into different angle measures.}
\usage{
atans(x, y, units = "months")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a scalar,vector or array with each value representing a the x position of the coridiantes (x,y)}
  \item{y}{sclar, vector or array with same dimeneions as x, reprenting the y poistion in (x,y)}
  \item{units}{How the resultant angle should be measured. See value for more information}

\value{The angle, or phase,  between the origin and points (xi,yi) for each element in x and y. Dimensions are the same as x and y. Expressed in units as described by 'units'. units='months' is the default, and returns fractional months between 0 and 12 (0 representing the x-axis). 'radians' returns radians. 'degrees' return degrees.}
\author{Douglas Kelley \email{douglaskelley3@gmail.com}}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
	x <- array(1:24,dim=c(2,3,4))
	y <- array(rev(1:24),dim=c(2,3,4))
	phase <- atans(x,y)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~math }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
