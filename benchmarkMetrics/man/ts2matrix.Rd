\name{ts2matrix}
\alias{ts2matrix}
\title{
  Time Series to Matrix
}
\description{
  Comverts Time Series Class to Matrix
}
\usage{
    ts2matrix(ts)
}
\arguments{
  \item{ts}{
      An object of class ts
  }
}
value{
    The resultant matrix appears in the same shape with same values as the vector or matrix when using print(ts) with the same colnames and rownames as displayed by print(x). However, the resultant matrix is usable (not just displayable) as a matrix
}
\author{Douglas Kelley \email{douglas.i.kelley@gmail.com}}

\seealso{
\code{\link{ts}}
}
\examples{
  ## Measurements of the annual flow of the river Nile at Ashwan 1871-1970.
  ## Durbin, J. and Koopman, S. J. (2001) _Time Series Analysis by State Space
  ## Methods._ Oxford University Press.  http://www.ssfpack.com/DKbook.html

  print(Nile)
  Nile  = ts2matrix(Nile)
  print(Nile)

  ## Quarterly TS of UK gas consumption from 1960Q1 to 1986Q4, in millions of
  ## therms.
  ## Durbin, J. and Koopman, S. J. (2001) _Time Series Analysis by State Space
  ## Methods._ Oxford University Press. http://www.ssfpack.com/dkbook/

  print(UKgas)
  UKgas = ts2matrix(UKgas)
  print(UKgas)

  ## Monthly TS of Atmospheric concentrations of CO2 are expressed in parts per
  ## million (ppm) and reported in the preliminary 1997 SIO manometric mole
  ## fraction scale.
  ## Keeling, C. D. and Whorf, T. P., Scripps Institution of Oceanography (SIO),
  ## University of California, La Jolla, California USA 92093-0220.
  ## ftp://cdiac.esd.ornl.gov/pub/maunaloa-co2/maunaloa.co2

  co2
  co2   = ts2matrix(co2)
  print(co2)

}
\keyword{ ~ts }
\keyword{ ~matrix }
