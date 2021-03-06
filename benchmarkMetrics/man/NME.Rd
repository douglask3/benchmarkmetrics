\name{NME}
\alias{NME}
\alias{NME.default}
\alias{NMSE}
\alias{NMSE.default}
\alias{print.NME}
\alias{summary.NME}
\alias{plot.NME}

\title{Normalised Mean (Squared) Error}
\description{Performs the Normalised Mean Error (NME) and Normalised Mean Squared Error (NMSE) comparison between model and observed.}
\usage{
  NME (x, y, w = NULL, allowRegridding = TRUE)
  NMSE(x, y, w = NULL, allowRegridding = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y}{matrix, data.frame, raster or raster stacks. Where y is the approximation to x}
  \item{w}{vector or raster of weights or same size as x. If NULL, all items are considered equally.}
  \item{allowRegridding}{Logical arguement used if x and y are both raster.
           If TRUE and x and y are on different grids, then x and y are cropped
           to the smallest shared extent and lowest reslution of either x and y.
           If w is raster as well, this will also be regridded to smallest extent and lowest
           resolution of x or y.}
}

\details{
  NME/NMSE measures the distance (or error) between model and observations.

\deqn{NME = \Sigma | y_{i} - x_{i} | / \Sigma| x_{i} - mean(x) |}
\deqn{NMSE = \Sigma ( y_{i} - x_{i} )^2 / \Sigma  ( x_{i} - mean(x) )^2}

where \eqn{y_{i}} is the modelled value of variable \eqn{x} in grid cell (or at site)
\eqn{i}, \eqn{x_{i}} the corresponding observed value, and \eqn{mean(x)} the mean
observed value across all grid cells or sites.

They both take the value of 0 for perfect agreement. A value of 1 is equivalent to a comparison between observations and the observtations mean value. Large numbers donote larger errors and therefore worse model performance.

The functions provide 3 measures: 1) straight NME/NMSE comparisons; 2) comparisons with the influcance of the mean removed (i.e testing pattern and variablity); 3) comparisons with the influance of the mean and variance removed (i.e testing the pattern only).
}

\value{
NME and NMSE returns an object of class "NME".

The \code{\link{print}} print function returns the call information and scores for each step.

The function \code{\link{summary}} can be used to obtain information  means and variances for x and y, as well as the ratio of x and ys mean and variance.

The \code{\link{plot}} function plots the steps in a scatter plot.
}

\references{
Kelley, D. I., Prentice, I. C., Harrison, S. P., Wang, H., Simard, M., Fisher, J. B., & Willis, K. O. (2013). A comprehensive benchmarking system for evaluating global vegetation models. Biogeosciences, 10(5), 3313-3340. doi:10.5194/bg-10-3313-2013

Nash, Je., & Sutcliffe, J. V. (1970). River flow forecasting through conceptual models part I-A discussion of principles. Journal of hydrology, 10(3), 282-290.
}


\author{Douglas Kelley \email{douglas.i.kelley@gmail.com}}

\seealso{
  \code{\link{print.NME}}, \code{\link{summary.NME}}, \code{\link{plot.NME}}
}

\examples{
  ## C. I. Bliss (1952) The Statistics of Bioassay. Academic Press.
  ## In McNeil, D. R. (1977) Interactive Data Analysis. New York: Wiley.
  ## see ?ToothGrowth

  ## Load data
  Obs   = ToothGrowth[,'len' ]

  ## Construct Model
  supp  = ToothGrowth[,'supp']
  dose  = ToothGrowth[,'dose']
  toothFairy = sample(c(TRUE,FALSE), length(Obs),replace=TRUE)
  Mod   = predict(lm(Obs ~  supp + dose + toothFairy))/2 + 10

  ## Compare model errors using metrics
  nme = NME (Obs, Mod)
  nmse= NMSE(Obs, Mod)

  ## View metric info
  nme
  nmse
  summary(nme)

  ## Plot metric info
  par(mfrow=c(2,1))
  plot(nme)
  mtext("NME")
  plot(nmse)
  mtext("NMSE")


  ## Compare Models with and without toothfariy
  with    = predict(lm(Obs ~  supp + dose + toothFairy))
  without = predict(lm(Obs ~  supp + dose))

  ## Perform metrics
  with    = NME (Obs,with)
  without = NMSE(Obs,without)

  ## Plot with and without
  par(mfrow=c(2,1))
  plot(with)
  mtext("with")
  plot(without)
  mtext("without")


  ## Example taken from ?brick
  require(raster)
  b = brick(system.file("external/rlogo.grd", package="raster"))

  m2 = NME(b[[1]],b[[2]])
  m3 = NME(b[[1]],b[[3]])

  par(mfrow=c(2,1))
  plot (m2)
  mtext("2")
  plot (m3)
  mtext("3")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~benchmark }
\keyword{ ~metric }% __ONLY ONE__ keyword per line
