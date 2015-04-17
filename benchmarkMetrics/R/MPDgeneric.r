print.MPD <- function(x, ...) {
	cat("Call:\n")
	print(x$call)
	
	cat("\nMPD Phase Score:\n")
	print(standard.round(x$Phase$Score))
	
	cat("\nConcerntration Scores:\n")
	cat.NME.scores(x$Concentration)
}


summary.MPD <- function(x, ...) {
	required.standard <- function(l) head(basic.summaryInfo(x[[l]])[-1],-1)
	
	summ=list()
	
	summ=determinIfWeightsUsed(x,summ)
	summ$weights=list(weights=summ$weights)
	
	summ$Concentration=c(Metric='NME - Concentration',required.standard('Concentration'))
	summ$Concentration$Scores=scores.summaryInfo(x$Concentration)
	
	summ$Phase=c(Metric='MPD - Phase',required.standard('Phase'))
	summ$Phase$Scores=standard.round(x$Phase$Score)
	
	
	class(summ) <- 'listofMetric'
	return(summ)
}

plot.MPD <- function(x,...) {
    xc=x[[1]]$x
    yc=x[[1]]$y
    
    xl=x[[2]]$x
    yl=x[[2]]$y
    
    cols=colorRampPalette(c("#EEEEEE","#777777"))(180)
    
    dl = abs(xl-yl)
    dc  = abs(xc-yc)
    
    dc=round(180*dc/pi); dc[dc>360]=dc[dc>360]-360
    
    cols=colorRampPalette(c("#000099","#990000","#000099"))(360)
    cols=makeTransparent(cols[dc],0.9*(1-dl/max(dl)))
    
    graphics.off()
    l=c(xl,yl); cn=c(xc,yc); symb=c(rep(c(4,16),each=length(xl)))
    radial.plot(l,cn,line.col=cols,point.symbols=symb,
                point.col=cols,rp.type="rs",...)
}

score.MPD <- function(x, ..) {
    if (names(x[[2]][2]) == "x") index = 1 else index = 1:3
    return(c(phase = x[[1]][[1]], concentration = unlist(x[[2]][index])))
}


null.MPD <- function(x, ...) null.FUN(x, MPD, items=TRUE, step1only=TRUE, ...)

makeTransparent <- function(col, transparency) {
     ## Semitransparent colours
     tmp <- col2rgb(col)/255
     rgb(tmp[1,], tmp[2,], tmp[3,], alpha=1-transparency)
}