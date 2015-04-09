\name{MM}
\alias{MM}
\alias{MM.default}
\alias{MPD}
\alias{MPD.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Manhattan Metric}
\description{Performs the Manhattan Metric comparison between modelled and observed data on fractional-type items.}
\usage{MM(x,y,w=NULL)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y}{matrix, data.frame, raster or raster stack,representing a list of observations. 
           The last dimension represents the fractional items, whilst all other dimenstions 
           represent different realisations of these items}
  \item{w}{vector or raster of weights. If NULL, all items are considered equally. 
           If a vector, must by the same length as first dimension of x and represents the
           weight of each point. If matrix or raster, than weight for each point and item.}
}
\details{if x and y are 2-dimensional, rows represent 'points', columns represent items. Items for each
         'point' are normalised before metric comparison is made.}
\value{
MM returns an object of class "MM".

The  \code{\link{print}} print function (i.e \code{\link{print.MM}}) returns the call information and score

The function \code{\link{summary}} (i.e., \code{\link{summary.MM}}) can be used to obtain information on individual item means and variances for both x and y, as well as the ratio of x and ys mean and variance.

The \code{\link{plot}} function (i.e \code{\link{plot.MM}}) plots the items in a scatter plot.

}
\references{
Kelley, D. I., Prentice, I. C., Harrison, S. P., Wang, H., Simard, M., Fisher, J. B., & Willis, K. O. (2013). A comprehensive benchmarking system for evaluating global vegetation models. Biogeosciences, 10(5), 3313-3340. doi:10.5194/bg-10-3313-2013
}
\author{Douglas Kelley \email{douglas.i.kelley@gmail.com}}

\seealso{
\code{\link{print.MM}}, \code{\link{summary.MM}}, \code{\link{plot.MM}}
}
\examples{
  ## NPP allocation observations from Schuur & Matson (2001)
  Obs =cbind(canopy = c(4.25 ,5.0  ,4.25 ,4.5  ,3.0  ,2.0 ),
             stem   = c(0.37 ,0.12 ,0.62 ,0.12 ,0.75 ,0.05),
             wood   = c(0.62 ,0.21 ,1.03 ,0.21 ,1.24 ,0.08))
  
  ## Allocacion fractions from GDAY fixed allocation (Mcmurtrie & Comins 1996)
  Mod = cbind(canopy = rep(0.2,6),
              stem   = rep(0.6,6),
              wood   = rep(0.2,6))
  
  ## Weight are total NPP of each measure
  weights = apply(Obs,1,sum)
  
  m = MM(Obs,Mod,weights)
  
  m
  
  summary(m)
  
  plot(m)
  
  ## Schuur E. A. G., Matson P. A. 2001 Net primary productivity and nutrient cycling across a mesic to wet precipitation gradient in Hawaiian montane forest. Oecologia 128, 431-442. doi:10.1007/s004420100671
  ## Mcmurtrie, R. E., & Comins, H. N. (1996). The temporal response of forest ecosystems to doubled atmospheric CO2 concentration. Global Change Biology, 2(1), 49-57.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~benchmarking }
\keyword{ ~metric }% __ONLY ONE__ keyword per line
