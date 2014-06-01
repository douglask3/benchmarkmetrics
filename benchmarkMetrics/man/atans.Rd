\name{atans}
\alias{atans}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
atans(x, y, units = "months")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{units}{
%%     ~~Describe \code{units} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
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
function (x, y, units = "months") 
{
    if (class(x) == "RasterLayer") 
        phase_out = x[[1]]
    if (class(x) == "RasterLayer") 
        x = as.matrix(x)
    if (class(y) == "RasterLayer") 
        y = as.matrix(y)
    phase = atan(x/y)
    test = (x > 0) + (y < 0)
    phase[which(test == 2)] = pi + phase[which(test == 2)]
    test = (x == 0) + (y < 0)
    phase[which(test == 2)] = pi
    test = (x < 0) + (y < 0)
    phase[which(test == 2)] = -pi + phase[which(test == 2)]
    test = (x < 0) + (y == 0)
    phase[which(test == 2)] = -pi/2
    if (units == "months") {
        phase = 6 * (phase/pi) + 6
    }
    else if (units == "degrees") {
        phase = phase + pi
        phase = phase * 360/(2 * pi)
    }
    else if (units == "radians") 
        phase = phase + pi
    if (class(x) == "RasterLayer") {
        values(phase_out) = phase
        return(phase_out)
    }
    else {
        return(phase)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
