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
  ma <- function(x, n = 12) filter(x, rep(1 / n, n), sides = 2)
  
  ma.Seatbelt <- function(x, ...) ma(SeatbeltsLaw[, x], ...)
  
  Deaths   = ma.Seatbelt('drivers')
  modNoLaw = ma.Seatbelt('modNoLaw')
  modLaw   = ma.Seatbelt('modLaw')
  
  graphics.off()
  par(mfcol=c(3,3),mar=c(4,3,1,0),oma=c(0,4,0,0))
  
  plot (Deaths,ylab='Deaths',xpd=NA)
  lines(yr, modNoLaw, col = 'red' )
  lines(yr, modLaw  , col = 'blue')
  legend(x = 'bottomleft', lty=1, col = c('black', 'red', 'blue'), legend = c('observed', 'Without Law', 'With Law'))
  
  nme.NoLaw = NME(Deaths,modNoLaw)
  nme.Law   = NME(Deaths,modLaw)
  
  nmePlot <- function(nme,txt) {
    plot (nme, xlab = 'Observed Deaths', ylab = 'Predicted Deaths', xpd = NA)
    mtext(txt, side = 2, line = 4, adj = 0.99)
  }
  
  nmePlot(nme.NoLaw,"Model without Law")
  nmePlot(nme.Law,"Model with Law")
  
  
  ## Test interannual variability in seasonality
  plot(c(1,12),range(SeatbeltsLaw[,'drivers']),xaxt='n',xlab='Month',ylab='', type='n')
  axis(1, at=1:12, c('J','F','M','A','M','J','J','A','S','O','N','D'))
  
  addSeasonal <- function(nm,col) apply(ts2matrix(SeatbeltsLaw[,nm]),1,function(y) polygon(c(1:12,12:1),c(y+100,rev(y)-100),col=col,border=NA))
  
  addSeasonal('drivers' ,'#00000055')
  addSeasonal('modNoLaw','#FF000022')
  addSeasonal('modLaw'  ,'#0000FF11')
  
  mpd.NoLaw = MPD(SeatbeltsLaw[,'drivers'],SeatbeltsLaw[,'modNoLaw'])
  mpd.Law   = MPD(SeatbeltsLaw[,'drivers'],SeatbeltsLaw[,'modLaw'  ])
  
  mpdPlot <- function(mpd) { 
    plot(mpd.NoLaw,labels=c('Jan','Jul'),
         radial.lim=c(0,0.08),radial.labels='')
    axis(1,pos=0,at=c(0,0.08))
  }
  mpdPlot(mpd.NoLaw)
  mpdPlot(mpd.Law)
  
  
  ## Test law model explonation of front:rear ratio killed
  tot = SeatbeltsLaw[, 'front'] + SeatbeltsLaw[, 'rear']
  SeatbeltsLaw[, 'front'] = SeatbeltsLaw[, 'front'] / tot
  SeatbeltsLaw[, 'rear' ] = SeatbeltsLaw[, 'rear' ] / tot
  SeatbeltsNoLaw          = SeatbeltsLaw[test,]
  
  form  = "front ~ kms * (a  + b*year + d*sin(e + month * 2*pi/12))"
  modFrontNoLaw = performModel(SeatbeltsNoLaw)
  modFrontLaw   = performModel(SeatbeltsLaw, "+ f*law", c(f = 0))
  
  SeatbeltsLaw = addCol2Ts(SeatbeltsLaw,
        cbind(modFrontNoLaw, modRearNoLaw = 1 - modFrontNoLaw,
              modFrontLaw  , modRearLaw   = 1 - modFrontLaw  ))
   
  returnDensity <- function(x) {
    out=hist(SeatbeltsLaw[, x], 1000, plot = FALSE)[c('mids','density')]
    out[[2]]=ma(out[[2]],100)
    return(out)
  }
  
  Obs   = returnDensity('front'        )
  NoLaw = returnDensity('modFrontNoLaw')
  Law   = returnDensity('modFrontLaw'  )
  
  plot(range(Obs[[1]]),range(c(Obs[[2]],NoLaw[[2]],Law[[2]]),na.rm=TRUE),type='n',xlab='',ylab='Count')
  
  addDensityLines <- function(x,col) lines(x[[1]],x[[2]],col=col)
  
  addDensityLines(Obs,'black')
  addDensityLines(NoLaw,'red')
  addDensityLines(Law,'blue')
  
  
  mm.NoLaw = MM(SeatbeltsLaw[,c('front'        , 'rear'        )],
                SeatbeltsLaw[,c('modFrontNoLaw', 'modRearNoLaw')])
  mm.Law   = MM(SeatbeltsLaw[,c('front'        , 'rear'        )],
                SeatbeltsLaw[,c('modFrontLaw'  , 'modRearLaw'  )])
                
  plot(mm.NoLaw)
  plot(mm.Law  )
  
  ## Find nulls models
  nmeNull = null.NME(SeatbeltsLaw[,'drivers'])
  mpdNull = null.MPD(SeatbeltsLaw[,'drivers'])
  mmNull  = null.MM (SeatbeltsLaw[,'drivers'])
  
  ## Compare metrics to null models
  
  par(mfcol=c(2,2))
  cols = cbind(Law =   c("#0000FF","#0000DD","#0000DD"),
               NoLaw = c("#DDDD00","#AAAA00","#550000"))

  
  addRsltLine <- function(score  ,col ) lines(c(score,score),c(0,9E9),col=col)
  
  addStepLine <- function(scores ,cols)
    mapply(addRsltLine,score(scores),cols)
  
  plot(nmeNull, main = "NME Model Comparison", xlim = c(0.4 , 1.5), legend = FALSE)
  addStepLine(nme.Law  , cols[, 1])
  addStepLine(nme.NoLaw, cols[, 2])
  
  plot(mmNull,  main = " MM Model Comparison ", xlim = c(0.08, 0.2), legend = FALSE)
  addRsltLine(score(mm.Law)  , cols[1, 1])
  addRsltLine(score(mm.NoLaw), cols[1, 2])
  
  plot(mpdNull, main = "MPD Model Comparison" , legend = FALSE)
}
