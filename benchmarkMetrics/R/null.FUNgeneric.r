print.nullModel <- function(x,...) {
    cat("Mean mode score:\n\t\t")
    cat(standard.round(x[[1]]))
    
    cat("Rand-Resampling scores:\n\t\t:")
    cat(standard.round(x[[2]]))
}

summary.nullModel <- function(x,...) {
    modSum=c("Mean Model"             =        x[[1]],
             "Mean Random-Resampling" =   mean(x[[2]]),
             "sd Randon Resampling"   =   sd  (x[[2]]))
    
    class(modSum)="NullModelSummary"
    return(modSum)    
}

print.NullModelSummary <- function(x) {
    x=standard.round(x)
    
    print(x[1])
    
    cat ("Random Model\n\t\t(Mean +/- sd)\n\t\t ")
    cat (x[2])
    cat (" +/- ")
    cat (x[3])
}

plot.nullModel <- function(x,xlab='',ylab='',...) {
    hist(x[[2]],ceiling(length(x[[2]])/10),yaxt='n',xlab=xlab,ylab=ylab)
    
    x=summary(x)
    randRange =  x[2]+x[3]*c(-1,1)
    
    polygon(rep(randRange,each=2),c(0,1,1,-1)*9E9,
            border=NA,col=makeTransparent('blue',0.8))
    
    verLine <- function(xi,col='blue',...) lines(rep(xi,2),c(0,9E9),col=col,...)
    
    verLine(x[1],'red')
    verLine(x[2])
    verLine(x[2]-x[3],lty=2)
    verLine(x[2]+x[3],lty=2)
    
    
    legendStandard <- function(...)  legend('topleft',bty='n',legtxt,...)
    
    legtxt=c('Mean Model','Randon Model',paste('    ',c('frequancy','mean','standard deviation')))
    
    lty=c(1,0,0,1,2)
    pch=c(NA,0,0,NA,NA)
    col=c('red','transparent','black','blue','blue')
    legendStandard(lty=lty,pch=pch,col=col)
    
    pch=15
    col = c('transparent','transparent','transparent','blue','blue')
    col = makeTransparent(col,0.8)
    legtxt[length(legtxt)]=""
    lty[length(lty)]=0
   
    legendStandard(lty=lty,pch=15,col=col,y.intersp=c(rep(1,4),0.97))
    
}