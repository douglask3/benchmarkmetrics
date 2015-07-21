\name{null.FUN }
\alias{null.NME }
\alias{null.NMSE}
\alias{null.MM  }
\alias{null.SCD }
\alias{null.MPD }

\title{Null Benchmark Models}
\description{Performs mean and random-resampling null-models for given metric.}
\usage{
  null.FUN (x, FUN, n = 1000, ...)
  null.NME (x, ...)
  null.NMSE(x, ...)
  null.MM  (x, ...)
  null.SCD (x, ...)
  null.MPD (x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{matrix, data.frame, raster or raster stacks, matching the format descibed by the corrisonponding metric}
  \item{FUN}{A function to perform null models on. Calling e.g. \code{null.FUN(x, NME)} is equivalent to calling \code{null.NME(x)}}
  \item{n}{ number of resampled bootstraps to be performed for the randomly-resampled null model.}
  \item{...}{Additional arguements used by the corrisponding metric and \code{null.FUN}}
}

\details{
To facilitate interpretation of the scores, \code{null.FUN} compares a benchmark dataset \code{x} to a dataset of the same size, filled with: 1) the mean of the observations; and 2) a ``randomized'' datasets. Ramdomizing of datasets is performed using a bootstrapping procedure (Efron, 1979; Efron and Tibshirani, 1994), whereby a constructed dataset of the same dimensions as \code{x} is filled by randomly resampling the data in \code{x} with replacement 100 times to estimate a probability density function of ``random-resampled'' scores.
}

\value{
null.FUN returns an object of class ``nullModel''.

The \code{\link{print}} print function returns the score for all runs.

The function \code{\link{summary}} returns the mean model score(s) and the mean and \code{\link{sd}} of the radomly-resampled model.

The \code{\link{plot}} function plots a histagram of the probability density function (pdf) of radomly-reampled models, and indicated on this pdf the \code{\link{summary}} information.
}

\references{
Efron, B. (1979). Bootstrap methods: another look at the jackknife. The annals of Statistics, 1-26.

Efron, B., & Tibshirani, R. J. (1994). An Introduction to the Bootstrap (Chapman & Hall/CRC Monographs on Statistics & Applied Probability).

Kelley, D. I., Prentice, I. C., Harrison, S. P., Wang, H., Simard, M., Fisher, J. B., & Willis, K. O. (2013). A comprehensive benchmarking system for evaluating global vegetation models. Biogeosciences, 10(5), 3313-3340. doi:10.5194/bg-10-3313-2013
}


\author{Douglas Kelley \email{douglas.i.kelley@gmail.com}}

\seealso{
  \code{\link{print.NME}}, \code{\link{summary.NME}}, \code{\link{plot.NME}}
}

\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~benchmark }
\keyword{ ~metric }
\keyword{ ~null model}
