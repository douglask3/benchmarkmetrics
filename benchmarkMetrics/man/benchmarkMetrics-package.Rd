\name{benchmarkMetrics-package}
\alias{benchmarkMetrics-package}
\alias{benchmarkMetrics}
\docType{package}
\title{Benchmark Metrics}
\description{
Specifically designed metrics quantify model performance against observations via ``metric scores'' and compare to scores to "null" model-benchmarks based on the temporal or spatial mean value of the
observations and a ``random'' model produced by bootstrap resampling of the
observations.
}
\details{
\tabular{ll}{
Package: \tab benchmarkMetrics\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-05-30\cr
License: \tab GPL 2\cr
Website: https://bitbucket.org/teambcd/benchmarkmetrics
}

}
\author{Douglas Kelley \email{douglas.i.kelley@gmail.com}}
\references{
Kelley, D. I., Prentice, I. C., Harrison, S. P., Wang, H., Simard, M., Fisher, J. B., & Willis, K. O. (2013). A comprehensive benchmarking system for evaluating global vegetation models. Biogeosciences, 10(5), 3313-3340. doi:10.5194/bg-10-3313-2013
}

\keyword{ package      }
\keyword{ benchmarks   }
\keyword{ benchmarking }
\keyword{ metrics }
\examples{

  mnthN         = rep(1:12, length.out = dim(Seatbelts)[1])
  yr            = seq(start(Seatbelts)[1], end(Seatbelts)[1]+11/12,by=1/12)
  law           = Seatbelts[, "law"]
  test          = law == 0

  addCol2Ts <- function(dat,ndat) {
    cnames        = c(colnames(dat),colnames(ndat))
    dat           = cbind(dat,ndat)
    colnames(dat) = cnames
    return(dat)
  }

  SeatbeltsLaw   = addCol2Ts(Seatbelts,cbind(month=mnthN,year=yr))
  SeatbeltsNoLaw = SeatbeltsLaw[test,]

  form  = "drivers ~ kms * (a  + b*year + d*sin(e + month * 2*pi/12))"
  start = c(a=0, b=-1, d=1, e=0)

  performModel <- function(dat, dform = "", dstart =  c()) {
    start = c(start, dstart)
    form  = formula(paste(form, dform))
    mod   = nls(form, start = start, data=as.data.frame(dat))

    return(predict(mod,as.data.frame(SeatbeltsLaw)))
  }

  modNoLaw = performModel(SeatbeltsNoLaw)
  modLaw   = performModel(SeatbeltsLaw, "+ f*law", c(f = 0))

  SeatbeltsLaw = addCol2Ts(SeatbeltsLaw, cbind(modNoLaw,modLaw))


  ## Test interannual variability
  ma <- function(x, n = 12) filter(SeatbeltsLaw[, x], rep(1 / n, n), sides = 2)

  Deaths   = ma('drivers')
  modNoLaw = ma('modNoLaw')
  modLaw   = ma('modLaw')

  graphics.off()
  par(mfcol=c(3,1),mar=c(4,5,1,1))

  plot (Deaths)
  lines(yr, modNoLaw, col = 'red' )
  lines(yr, modLaw  , col = 'blue')
  legend(x = 'bottomleft', lty=1, col = c('black', 'red', 'blue'), legend = c('observed', 'Without Law', 'With Law'))

  nme.NoLaw = NME(Deaths,modNoLaw)
  nme.Law   = NME(Deaths,modLaw)

  plot (nme.NoLaw, xlab = 'Observed Deaths', ylab = 'Predicted Deaths')
  mtext("Model without Law", side = 1, line = -1, adj = 0.99)
  plot (nme.Law  , xlab = 'Observed Deaths', ylab = 'Predicted Deaths')
  mtext("Model with Law"   , side = 1, line = -1, adj = 0.99)


  ## Test interannual variability in seasonality
  plot(c(1,12),range(SeatbeltsLaw[,'drivers']))

  addSeasonal <- function(nm,col) apply(ts2matrix(SeatbeltsLaw[,nm]),1,function(y) polygon(c(1:12,12:1),c(y+100,rev(y)-100),col=col,border=NA))

  addSeasonal('drivers' ,'#00000055')
  addSeasonal('modNoLaw','#FF000022')
  addSeasonal('modLaw'  ,'#0000FF11')

  mpd.NoLaw = MPD(SeatbeltsLaw[,'drivers'],SeatbeltsLaw[,'modNoLaw'])
  mpd.Law   = MPD(SeatbeltsLaw[,'drivers'],SeatbeltsLaw[,'modLaw'  ])

  plot(mpd.NoLaw,rlab="",labels=c('0','0.8'),radial.labels='')
  plot(mpd.Law)
}
