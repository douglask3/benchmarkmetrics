\name{atans}
\alias{atans}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Performing a full circle arc-tangent}
\description{ Performs to atan2, but allows conversion into different angle measures and allows raster inputs. The arc-tangent of two vectors, x and y, returns the angle between the x-axis and the vector from the origin to (x, y).}
\usage{
atans(x, y, units = "months")
}
\arguments{
  \item{x, y}{numeric or complex vectors or rasters}
  \item{units}{How the resultant angle should be measured. See value for more information}

\value{The angle between the origin and points  for each element in x and y. Expressed in units as described by 'units'. units='months' is the default, and returns fractional months between 0 and 12 (0 representing the x-axis). 'radians' returns radians. 'degrees' returns degrees.}
\author{Douglas Kelley \email{douglas.i.kelley@gmail.com}}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{atan2}}
  \code{\link{raster}}
}
\examples{
	x <- array(1:24,dim=c(2,3,4))
	y <- array(rev(1:24),dim=c(2,3,4))
	phase <- atans(x,y)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~math }% __ONLY ONE__ keyword per line
