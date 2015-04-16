print.nullModel <- function(x,...) {    
    cat("Mean mode score:\n\t\t")
    cat(standard.round(x[[1]]),"\n\n")
   
    cat("Rand-Resampling scores:\n\t\t")
    
    if (class(x[[2]]) == "matrix") {
        pr = apply(x[[2]], 1, standard.round)
        printStep <- function(i)
            cat(colnames(pr)[i],"\n\t",pr[,i],"\n\n")
        
        lapply(1:ncol(pr),printStep)
    } else cat(standard.round(x[[2]]))  
}

summary.nullModel <- function(x,...) {
    if (length(x[[1]])>1) comb=list else comb=c
    
    if (class(x[[2]])!='matrix') x[[2]]=t(as.matrix(x[[2]]))
   
    modSum=comb("Mean Model"             =        x[[1]],
                "Mean Random-Resampling" =  apply(x[[2]],1,mean),
                "sd Randon Resampling"   =  apply(x[[2]],1,sd  ))
    
    class (modSum)="NullModelSummary"
    return(modSum)
}

print.NullModelSummary <- function(x) {
   
    if (class(x[1])=="list") fun=lapply else fun=sapply
    x=fun(x,standard.round)
    
    if (class(x[1])=="list") {cat("Mean Model\n\t"); print(x[[1]])} else print(x[1])
    
    printRand <- function(a,b,c) {
        if (!is.null(a)) cat(a,"\n")
        cat("\t\t",b," +/- ",c,"\n")
    }
    
    cat ("Random Model\n\t\t(Mean +/- sd)\n")
    
    if (class(x)=="list") mapply(printRand,names(x[[2]]),x[[2]],x[[3]])
        else  printRand(NULL,x[2],x[3])
}

plot.nullModel <- function(x,xlab='',ylab='',main='Null Model Results',...) {
    # Plot histergram of random model
    hist(x[[2]],ceiling(length(x[[2]])/10),yaxt='n',xlab=xlab,ylab=ylab,main=main)
    
    # Calculate summary of null scores
    x=summary(x)
    randRange =  x[2]+x[3]*c(-1,1)
    
    # Plot std range as oligon
    polygon(rep(randRange,each=2),c(0,1,1,-1)*9E9,
            border=NA,col=makeTransparent('blue',0.8))
    
    # Plot Mean mode in read; mean random-resampled socre in blue, and 
    #sd range of randon resampled score in dashed blue
    verLine <- function(xi,col='blue',...) lines(rep(xi,2),c(0,9E9),col=col,...)
    
    verLine(x[1],'red')
    verLine(x[2])
    verLine(x[2]-x[3],lty=2)
    verLine(x[2]+x[3],lty=2)
    
    nullModelLegend()
}

nullModelLegend <- function() {
    ## Define legend arrangeent and labels
     legendStandard <- function(...)  legend('topleft',bty='n',legtxt,...)
    
    legtxt=c('Mean Model','Randon Model',paste('    ',c('frequancy','mean','standard deviation')))
    
    ## Add lines and symbols
    lty=c(1,0,0,1,2)
    pch=c(NA,0,0,NA,NA)
    col=c('red','transparent','black','blue','blue')
    legendStandard(lty=lty,pch=pch,col=col)
    
    ## Add polygn shades
    pch=15
    col = c('transparent','transparent','transparent','blue','blue')
    col = makeTransparent(col,0.8)
    legtxt[length(legtxt)]=""
    lty[length(lty)]=0
   
    legendStandard(lty=lty,pch=15,col=col,y.intersp=c(rep(1,4),0.97))
}