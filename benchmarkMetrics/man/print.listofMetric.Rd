\name{print.listofMetric}
\alias{print.listofMetric}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
print.listofMetric(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (x, ...) 
{
    printSingle.listofMetric <- function(x, ...) {
        nn <- names(x)
        ll <- length(x)
        if (length(nn) != ll) 
            nn <- paste("Component", seq.int(ll))
        for (i in seq_len(ll)) {
            cat(nn[i], ":\n")
            if (is.numeric(x[[i]])) {
                if (length(x[[i]]) == 1) {
                  cat("\t")
                  cat(standard.round(x[[i]]), ...)
                }
                else print(standard.round(x[[i]]), ...)
            }
            else cat("\t", x[[i]])
            cat("\n\n")
        }
        invisible(x)
    }
    lapply(x, printSingle.listofMetric)
    invisible(x)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
