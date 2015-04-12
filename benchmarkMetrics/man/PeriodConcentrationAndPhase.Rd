\name{PeriodConcentrationAndPhase}
\alias{PeriodConcentrationAndPhase}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
seasonal_concentration_and_phase(cdata, phase_units = "radians")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cdata}{
%%     ~~Describe \code{cdata} here~~
}
  \item{phase_units}{
%%     ~~Describe \code{phase_units} here~~
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
\author{Douglas Kelley \email{douglaskelley3@gmail.com}}
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
function (cdata, phase_units = "radians") 
{
    a = dim(cdata)[2]
    xdata = ydata = matrix(0, dim(cdata)[1], 1)
    for (k in 1:a) {
        angle = 2 * pi * (12 - k + 1)/12
        xdata = xdata + cdata[, k] * cos(angle)
        ydata = ydata + cdata[, k] * sin(angle)
    }
    adata = sum(cdata)
    pdata = atans(0 - ydata, 0 - xdata, phase_units)
    cdata = sqrt(xdata^2 + ydata^2)/adata
    list(pdata, cdata)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
