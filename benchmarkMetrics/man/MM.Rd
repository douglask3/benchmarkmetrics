\name{MM}
\alias{MM}
\alias{MM.default}
\alias{SCD}
\alias{SCD.default}
\alias{print.MM}
\alias{summary.MM}
\alias{plot.MM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Manhattan Metric/ Square Chord Distance}
\description{Performs the Manhattan Metric (MM) and Square Chord Distance (SCD) comparison between modelled and observed data on fractional-type items.}
\usage{
  MM (x,y,w=NULL)
  SCD(x,y,w=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y}{matrix, data.frame, raster or raster stack where y is the approximation to x.
           The last dimension represents the fractional items, whilst all other dimenstions 
           represent different realisations of these items}
  \item{w}{vector or raster of weights. If NULL, all items are considered equally. 
           If a vector, must by the same length as first dimension of x and represents the
           weight of each point. If matrix or raster, than weight for each point and item.}
}
\details{MM measure the absolute distamce between items of simulated and observed items.

\deqn{
MM=\sum\nolimits_{ij} {\left| {x_{ij} -y_{ij} } \right|} /n;
}
where \equ{x_{ij}} and \equ{y_{ij}} is the fraction of item \equ{j} in measurement \equ{i}

SCD is similar and is often employed as an enhanced way of measuring  ``signal-to-noise''. 

\deqn{
SCD=\sum\nolimits_{ij} {\left( {\sqrt {q_{ij} } -\sqrt {p_{ij} } },
\right)^{2}} /n
}

They both take the value of 0 for perfect agreement, and 2 for complete disagreement.

If x and y are 2-dimensional, rows represent 'points', columns represent items. Items for each
         'point' are normalised before metric comparison is made.
}
\value{
MM and SCD returns an object of class "MM".

The  \code{\link{print}} print function returns the call information and score

The function \code{\link{summary}} can be used to obtain information on individual item means and variances for both x and y, as well as the ratio of x and ys mean and variance.

The \code{\link{plot}} function plots the items in a scatter plot.

}

\references{
Cha, S.-H. (2007). Comprehensive survey on distance/similarity measures between probability density functions. City, 1(2), 1.

Gavin, D. G., Oswald, W. W., Wahl, E. R., & Williams, J. W. (2003). A statistical approach to evaluating distance metrics and analog assignments for pollen records. Quaternary Research, 60(3), 356-367.

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
  
  m = SCD(Obs,Mod,weights)
  summary(m)
  
  ## Schuur E. A. G., Matson P. A. 2001 Net primary productivity and nutrient cycling across a mesic to wet precipitation gradient in Hawaiian montane forest. Oecologia 128, 431-442. doi:10.1007/s004420100671
  ## Mcmurtrie, R. E., & Comins, H. N. (1996). The temporal response of forest ecosystems to doubled atmospheric CO2 concentration. Global Change Biology, 2(1), 49-57.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~benchmarking }
\keyword{ ~metric }% __ONLY ONE__ keyword per line
