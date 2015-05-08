\name{PolarConcentrationAndPhase}
\alias{PolarConcentrationAndPhase}
\title{
  Polar Concentration And Phase
}
\description{
  Calculates the concentration and phase of a polar data
}
\usage{
  PolarConcentrationAndPhase(cdata, phase_units = "radians")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cdata}{
      Either vector or matrix. If vector, each element reprents equally spaced points along a polar coordinate system. If matrix, each column represnets equally spaced position along a polar coordinate system and each row a new series of measurments.
  }
  \item{phase_units}{
      Units of the phase outputs. Default is radians (\eqn{- \pi} to \eqn{\pi}), but there is also a choice of degrees (-180 to 180) and months (-6 months to 6 months)
  }
}
\details{
Each simulated or observed timestep (e.g month) with in the period (i.e year) is represented by a vector in the complex plane, the length of the vector corresponding to the magnitude of the variable for each period and the directions of the vector corresponding to the timeing within the period. It is assumed each timestep is uniformly distributed:
\deqn{
  \theta_{t} =2\pi \left( {t-1} \right)/n,
}

where \equ{n} is the number of timesteps \equ{t} in the period.

A mean vector L is calculated by averaging the real and imaginary parts of the n vectors, \eqn{x_{t}$}.
\deqn{
L_{x} =\sum\nolimits_t {x_{t} \cos \left( {\theta_{t} }
\right)}~~\text{and}~~L_{y} =\sum\nolimits_t {x_{t} \sin \left( {\theta_{t} }
\right)}
}
The length of the mean vector divided by the annual value stands for seasonal concentration, \equ{C}; its direction stands for phase, \eqn{P}:
\deqn{
  C=\frac{\sqrt {L_{x}^{2} +L_{y}^{2} } }{\sum\nolimits_t {x_{t} }};
}
\deqn{
  P=\arctan \left( {L_{x} /L_{y} } \right).
}
Thus, if the variable is concentrated all in one point within the polar coordinates, seasonal concentration is equal to 1 and the phase corresponds to that month. If the variable is evenly spread over all coordinates, then concentration is equal to zero and phase is undefined.
}
\value{
  Two compoments are returned, each of the length of the first dimension of cdata input, or length 1 if cdata is a vector
  \item{phase }{the phase timing of each row of the inputs (see details above)}
  \item{conc  }{the concentration around the phase of each row of the inputs (see details above)}
}
\references{
Kelley, D. I., Prentice, I. C., Harrison, S. P., Wang, H., Simard, M., Fisher, J. B., & Willis, K. O. (2013). A comprehensive benchmarking system for evaluating global vegetation models. Biogeosciences, 10(5), 3313-3340. doi:10.5194/bg-10-3313-2013
}
\author{Douglas Kelley \email{douglaskelley3@gmail.com}}

\seealso{
\link{\code{MPD}}
}
\examples{
  require(plotrix)
  ## Average Monthly Temperatures at Nottingham, 1920-1939
  ## Anderson, O. D. (1976) Time Series Analysis and Forecasting: The Box-Jenkins approach. Butterworths. Series R.
  ## see ?nottem
  
  ## Load T
  T       = t(matrix(nottem,nrow=12))
  
  ## Calculate seasonal climatology and angle of each month in degrees
  climT   = apply(annualT,2,mean)
  climT[1:6]=climT[1:6]
  periods = head(seq(0,360,length.out=13),-1)
  
  ## Plot climatology
  polar.plot(climT,periods,labels=c('J','F','M','A','M','J','J','A','S','O','N','D'),label.pos=periods,radial.labels='',radial.lim=c(25,62),rp.type='p',poly.col='#FF0000AA')
  
  scaleConc <- function(i) min(climT)+i*diff(range(climT))
  
  ## Calculate phase and concentraion.
  pc      = PolarConcentrationAndPhase(climT,phase_units="degrees")
  phase   = pc[[1]][1]
  
  ## Covert concentration to point on tempurature plot
  conc    = scaleConc(pc[[2]][1])
  
  ## Plot climatology phase on concentration on plot
  polar.plot(conc,phase,point.symbol=4,radial.lim=c(25,62),rp.type='rs',cex=2,lwd=2,add=TRUE)
  
  ## same calculation and plot or each year.
  pc      = PolarConcentrationAndPhase(annualT,phase_units="degrees")
  phase   = pc[[1]]
  conc    = scaleConc(pc[[2]])
  
  polar.plot(conc,phase,point.symbol=16,radial.lim=c(25,62),rp.type='rs',cex=1,add=TRUE,line.col='black')
  
}
\keyword{ ~polar }
\keyword{ ~phase }
\keyword{ ~concetration }
