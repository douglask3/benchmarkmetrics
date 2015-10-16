\name{PolarConcentrationAndPhase}
\alias{PolarConcentrationAndPhase}
\alias{PolarConcentrationAndPhase.RasterBrick}
\alias{PolarConcentrationAndPhase.RasterStack}
\title{
  Polar Concentration And Phase
}
\description{
  Calculates the concentration and phase of a polar data
}
\usage{
  PolarConcentrationAndPhase(cdata, phase_units = "radians", n = 12,
                             disag = FALSE, justPhase = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cdata}{
      Either vector or matrix. If vector, each element reprents equally spaced
      points along a polar coordinate system. If matrix, each column represnets
      equally spaced position along a polar coordinate system and each row a new
      series of measurments.
  }
  \item{phase_units}{
      Units of the phase outputs. Default is \code{"radians"} (\eqn{- \pi} to
      \eqn{\pi}), but there is also a choice of \code{"degrees"} (-180 to 180)
      and \code{"months"} (-6 months to 6 months)
  }
  \item{n}{
      length of cycle phase and concentration is calculated over. Default number
      of columns for matrix or 12 (i.e, 12 in a year) for raster stack or brick.
      If dat is longer (i.e, if \cide{ncol(dat)} for matrix or number of layers
      for raster objects is > n), a a 'climatology' is calculated at base n.
  }
  \item{disagFact}{
      Only used if dat is a raster brick or stack. Disaggregatation factor used
      by \code{\link{disaggregate}} to smooth raster outputs. Useful fi neater
      plotting. Default is NaN which does not perform disaggregatation.
  }
  \item{justPhase}{
    Logical only used if dat is a raster brick or stack. . If \code{TRUE}, just
    returns the phase metric.
  }
}
\details{
    Each simulated or observed timestep (e.g month) with in the period
    (i.e year) is represented by a vector in the complex plane, the length of
    the vector corresponding to the magnitude of the variable for each period
    and the directions of the vector corresponding to the timeing within the
    period. It is assumed each timestep is uniformly distributed:
    \deqn{\theta_{t} = 2 . \pi ( t - 1 ) / n}

    where \eqn{n} is the number of timesteps \eqn{t} in the period.

    A mean vector L is calculated by averaging the real and imaginary parts of
    the n vectors, \eqn{x}.
    \deqn{Lx = \Sigma x cos ( \theta )}
    and
    \deqn{Ly = \Sigma x sin ( \theta )}

    The length of the mean vector divided by the annual value stands for
    seasonal concentration, \eqn{C}; its direction stands for phase, \eqn{P}:
    \deqn{C = ( Lx^2 + Ly^2 ) / \Sigma x}

    \deqn{P = atan (Lx / Ly)}
    Thus, if the variable is concentrated all in one point within the polar
    coordinates, seasonal concentration is equal to 1 and the phase corresponds
    to that month. If the variable is evenly spread over all coordinates, then
    concentration is equal to zero and phase is undefined.
}
\value{
  Two compoments are returned, each of the length of the first dimension of
  cdata input, or length 1 if cdata is a vector
  \item{phase }{the phase timing of each row of the inputs (see details above)}
  \item{conc  }{the concentration around the phase of each row of the inputs
  (see details above)}
}
\references{
    Kelley, D. I., Prentice, I. C., Harrison, S. P., Wang, H., Simard, M.,
    Fisher, J. B., & Willis, K. O. (2013). A comprehensive benchmarking system
    for evaluating global vegetation models. Biogeosciences, 10(5), 3313-3340.
    doi:10.5194/bg-10-3313-2013
}
\author{Douglas Kelley \email{douglas.i.kelley@gmail.com}}

\seealso{
    \code{\link{MPD}}
}
\examples{
  require(plotrix)
  ##############################################################################
  ## matrix                                                                   ##
  ##############################################################################
  ## Average Monthly Temperatures at Nottingham, 1920-1939
  ## Anderson, O. D. (1976) Time Series Analysis and Forecasting: The
  ## Box-Jenkins approach. Butterworths. Series R.
  ## see ?nottem

  ## Load T
  T       = t(matrix(nottem,nrow=12))

  ## Calculate seasonal climatology and angle of each month in degrees
  climT      = apply(T, 2, mean)
  climT[1:6] = climT[1:6]
  periods    = head(seq(0, 360, length.out = 13), -1)

  ## Plot climatology
  polar.plot(climT, periods,
             labels = c('J','F','M','A','M','J','J','A','S','O','N','D'),
             label.pos = periods, radial.labels = '', radial.lim = c(25,62),
             rp.type = 'p', poly.col = '#FF0000AA')

  scaleConc <- function(i) min(climT) + i * diff(range(climT))

  ## Calculate phase and concentraion.
  pc      = PolarConcentrationAndPhase(climT, phase_units = "degrees")
  phase   = pc[[1]][1]

  ## Covert concentration to point on tempurature plot
  conc    = scaleConc(pc[[2]][1])

  ## Plot climatology phase on concentration on plot
  polar.plot(conc, phase, point.symbol = 4,radial.lim = c(25,62),
             rp.type = 'rs', cex = 2, lwd = 2, add = TRUE)

  ## same calculation and plot or each year.
  pc      = PolarConcentrationAndPhase(T, phase_units = "degrees")
  phase   = pc[[1]]
  conc    = scaleConc(pc[[2]])

  polar.plot(conc, phase, point.symbol = 16, radial.lim = c(25,62),
             rp.type = 'rs', cex = 1, add = TRUE, line.col = 'black')

  ##############################################################################
  ## Raster                                                                   ##
  ##############################################################################
  b = brick(system.file("external/rlogo.grd", package = "raster"))
  b = PolarConcentrationAndPhase(b)
  dev.new()
  plot(b)

  b  = b[[2]]
  b0 = b*2*pi
  for (i in 1:12) {
      bi = cos(pi *i/12 + b0)
      b  = addLayer(b, bi)
  }

  b    = dropLayer(b, 1)
  maxb = which.max(b)
  phsb = PolarConcentrationAndPhase(b, phase_units = 'months', justPhase = TRUE)

  dev.new()
  par(mfrow = c(3, 1))
  plot(maxb, main = 'max layer')
  plot(phsb, main = 'phase')
  plot(values(maxb), values(phsb), pch = 4)

}
\keyword{ ~polar }
\keyword{ ~phase }
\keyword{ ~concetration }
