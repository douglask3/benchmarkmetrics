\name{MPD}
\alias{MPD}
\alias{summary.MPD}
\alias{print.MPD}
\alias{plot.MPD}

\title{
  Mean Phase and Concentration Difference
}
\description{
  MPD measures model to observed error for pedictable periodic patterns (ie weasonal variablity) characterised in terms of differences in concentration (i.e inverse of season length) and phase (i.e timing of season).
}
\usage{
MPD(x, y, w=NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y}{matrix, data.frame, raster or raster stacks. Where y is the approximation to x}
  \item{w}{vector or raster of weights or same size as x. If NULL, all items are considered equally.}
  \item{\dots}{arguments passed to \code{\link{NME}}}
}
\details{
Each simulated or observed timestep (e.g month) with in the period (i.e year) is represented by a vector in the complex plane, which is split into phase and concontration components as descibed in \code{\link{PolarConcentrationAndPhase}}. If the variable is concentrated all in one point within the polar coordinates, seasonal concentration is equal to 1 and the phase corresponds to that month. If the variable is evenly spread over all coordinates, then concentration is equal to zero and phase is undefined. If either modelled or observed values have zero
values for all months in a given cell or site, then that cell/site is not included in the comparisons. Concentration comparisons are performed using \code{\link{NME}}. Modelled and observed phase are compared using mean phase difference:

\deqn{mpd = (1/\pi) . acos [ cos(\omega - \phi )/n]}
where \eqn{\omega} is the phase from input x, and \eqn{\phi} is from input y.
}


\value{
Length comparisons return values as outlines in \code{\link{NME}}. Phase comparisons represent a fration of maximum possible timing error (i.e 6 months for a year).
}
\references{
Kelley, D. I., Prentice, I. C., Harrison, S. P., Wang, H., Simard, M., Fisher, J. B., & Willis, K. O. (2013). A comprehensive benchmarking system for evaluating global vegetation models. Biogeosciences, 10(5), 3313-3340. doi:10.5194/bg-10-3313-2013
}
\author{Douglas Kelley \email{douglas.i.kelley@gmail.com}}


\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
    ## World Data Center-C1 For Sunspot Index Royal Observatory of Belgium, Av. Circulaire, 3, B-1180 BRUSSELS http://www.oma.be/KSB-ORB/SIDC/sidc_txt.html
    ## see ?sunspot.month

    ## Seasonal cycle of sunspots
    full  = t(matrix(sunspot.month,nrow=12))
    clim  = apply(full,2,mean)

    testAndPlot <- function(mod) {
        m     = MPD(full,mod) # Full model test
        print(summary(m))     # Prints scores and obs/mod information

        # Perform and plot example for first 10 only
        m10   = MPD(full[1:10,],mod)
        plot(m10,lwd=2,labels=c('Jan','Jul'),radial.labels='',cex=2)
    }
    testAndPlot(clim)


    ## test seasonal climatology model
    mnthN= rep(1:12,length.out=length(full))*pi/6
    fullTS=as.vector(t(full))

    climMod = predict(nls(fullTS ~ a*sin(b+mnthN) + c ,
                          start=c(a=1,b=1,c=1)))[1:12]

    testAndPlot(climMod)


    ## test inter-annual sunspot cycle
    fullTS=fullTS[1:2904] #2904 = 22 11-year cycles

    #11-year cycle phase position
    yearP = seq(0,11,by=1/12)*2*pi/11
    yearP = rep(yearP,length.out=length(fullTS))

    IAVMod= nls(fullTS ~ a*cos(b+yearP) + c, start=c(a=50,b=1,c=1))
    IAVMod= predict(IAVMod)[1:(11*12)]  ## Repreated cycle so only consider fiirst 11 years

    Yr11Cycle <- function(d) t(matrix(d,nrow=(11*12)))
    full11=Yr11Cycle(fullTS) # Transform observations into matri of 11-year cycles

    m     = MPD(full11,IAVMod)
    print(summary(m))
    plot(m,lwd=2,labels=c('0','6.5'),radial.labels='',cex=2)

    ## Trying to find an even long cycle
    longCycle <- function(n) { # n is number of months in longer cycle
        IAVMod= nls(fullTS ~ a*cos(b+yearP) +
                    c*rep(1:n,length.out=length(fullTS)) + e ,
                    start=c(a=50,b=1,c=1,e=1))
        IAVMod=predict(IAVMod)
        return(Yr11Cycle(IAVMod))
    }

    # Test length of cycle in stages of 1 yar between 11 and 100 years
    cycleLength=seq(11*12,1200,by=12)
    mods=lapply(cycleLength,longCycle)

    # Find scores and transform into range of 0 to 1
    scores=sapply(mods,function(i) score(MPD(full11,i)))
    range01 <- function(x){(x-min(x))/(max(x)-min(x))}
    scores = apply(scores,1,range01)


    # Plot Phase and concentration scores
    par(mfcol=c(2,1))
    plot(range(cycleLength),c(0,1),type='n',yaxt='n',
         xlab='Cycle Length',ylab='score range')
    axis(labels=c('min','max'),at=c(0,1),side=2)

    for (i in 1:2) {
        y=scores[,i]
        col=rainbow(2)[i]
        lines(cycleLength,y,col=col)
        j=which.min(y)

        lines(rep(cycleLength[j],2),c(-1,2),cex=3,xpd=FALSE)
        lines(rep(cycleLength[j],2),c(-1,2),col=col,cex=3,lty=2,xpd=FALSE)
    }
    plot.new()
    legend('top',colnames(scores)[1:2],col=rainbow(2),bty='n',ncol=2,lty=1)

    legend('bottom',colnames(scores)[1:2],col='black',lty=1,bty='n',ncol=2)
    legend('bottom',paste("min. score",colnames(scores)[1:2]),col=rainbow(2),lty=2,bty='n',ncol=2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~Phase }
\keyword{ ~Concentration }
\keyword{ ~Period }
\keyword{ ~Radial }
\keyword{ ~Polar }
\keyword{ ~Metric }
