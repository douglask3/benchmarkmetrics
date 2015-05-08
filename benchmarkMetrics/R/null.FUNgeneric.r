print.nullModel <- function(x, ...) {    
    
    printMultiModelMean(x[[1]])
   
    cat("\n\n","Random-Resampling model scores:\n\t\t")
    
    if (class(x[[2]]) == "matrix") {
        pr = apply(x[[2]], 1, standard.round)
        printStep <- function(i)
            cat(colnames(pr)[i], "\n\t", pr[, i], "\n\n")
        
        lapply(1:ncol(pr), printStep)
    } else cat(standard.round(x[[2]]))  
}

printMultiModelMean <- function(x) {
    cat("Mean model score:\n\t\t")
    if (length(x) > 1) {
        if (names(x[1]) == "phase") {
            names(x) = c(" ", paste("Step", 1:3))
            cat("Phase\t\tConcentration\n\t")
         }
        print(standard.round(x))
    } else cat(standard.round(x), "\n")
}

summary.nullModel <- function(x, ...) {
    if (length(x[[1]]) > 1) comb = list else comb = c
    
    if (class(x[[2]]) != 'matrix') x[[2]] = t(as.matrix(x[[2]]))
   
    modSum=comb("Mean Model"             =        x[[1]],
                "Mean Random-Resampling" =  apply(x[[2]],1,mean),
                "sd Randon Resampling"   =  apply(x[[2]],1,sd  ))
    
    class (modSum) = "NullModelSummary"
    return(modSum)
}

print.NullModelSummary <- function(x) {
    printMultiModelMean(x[[1]])
    
    if (class(x[2]) == "list") fun = lapply else fun = sapply
    x = fun(x[2:3], standard.round)
    
    printRand <- function(a, b, c) {
        if (!is.null(a)) cat(a, "\n")
        cat("\t\t", b, " +/- ", c, "\n")
    }
    
    cat ("Random Model\n\t\t(Mean +/- sd)\n")
    if (class(x) == "list") mapply(printRand, names(x[[1]]), x[[1]], x[[2]])
        else  printRand(NULL, x[1], x[2])
}

plot.nullModel <- function(x, main='Null Model Results', ...) {
    if (class(x[[2]]) != "matrix") {
        plot.nullModelInd(x, ...)
    } else {
        par(mfrow = c(2, 1))
        plotInd <- function(i1, i2, ttl) {
            xi = list(x[[1]][i1], x[[2]][i2,], x[3])
            class(xi) = class(x)
            plot.nullModelInd(xi, main=paste(main, ': ', ttl, ep=""), ...)
        }
        mapply(plotInd, list(1, 2:4), 1:2, c('phase', 'concentration'))
    }
    invisible()
}
    

plot.nullModelInd <- function(x, xlab='', ylab='',
                              main = 'Null Model Results', ...) {
    
    # Plot histergram of random model
    max(hist(x[[2]], ceiling(length(x[[2]]) / 10),
             yaxt = 'n', xlab = xlab, ylab = ylab, main = main)$density)
    
    # Calculate summary of null scores
    x = summary(x)
    
    randRange =  x[[2]] + x[[3]] * c(-1, 1)
    
    # Plot std range as polygon
    polygon(rep(randRange, each = 2), c(0, 1, 1, -1) * 9E9,
            border = NA, col = makeTransparent('blue', 0.8))
    
    # Plot Mean mode in read; mean random-resampled socre in blue, and 
    #sd range of randon resampled score in dashed blue
    verLine <- function(xi, col = 'blue', ...)
        lines(rep(xi, 2),c(0, 9E9), col = col, ...)
    
    x[[1]]   = x[[1]][!is.na(x[[1]])]
    x[[1]]   = unlist(lapply(unique(x[[1]]),
                             function(i) x[[1]][x[[1]] == i][1]))
    meanCols = c('#FF0000', '#BBBB00', '#00FF00')[1:length(x[[1]])]
    mapply(verLine, x[[1]], meanCols)
    verLine(x[[2]])
    verLine(x[[2]]-x[[3]], lty=2)
    verLine(x[[2]]+x[[3]], lty=2)
    
    if (length(x[[1]])>1) names(x[[1]]) = paste('Step', 1:length(x[[1]]))
    
    nullModelLegend(meanCols, names(x[[1]]), cex = 0.7)
    invisible()
}

nullModelLegend <- function(meanCols, meanNames, ...) {
    ## Define legend arrangeent and labels
    xl = par("xaxp")[1]
    yl = par("yaxp")[2]
    
    legendStandard <- function(...)
        legend(x=xl, y=yl * 1.2, bty='n', legtxt, xpd=TRUE, ...)
    
    legtxt = 'Mean Model'
    if (length(meanNames) > 1) {
        legtxt   = c(legtxt, paste('    ', meanNames))
        meanCols = c('transparent', meanCols)
    }
         
    legtxt = c(legtxt, 'Randon Model',
               paste('    ', c('frequancy', 'mean', 'standard deviation')))
    
    ## Add lines and symbols
    lty = c(rep(1, length(meanCols)), 0, 0, 1,  2 )
    pch = c(rep(1, length(meanCols)), 0, 0, NA, NA)
    
    col = c(meanCols, 'transparent', 'black', 'blue', 'blue')
    legendStandard(lty = lty, pch = pch, col = col, ...)
    
    ## Add polygn shades
    pch = 15
    col[1:(length(col) - 2)] = 'transparent'
    col = makeTransparent(col, 0.8)
    legtxt[length(legtxt)] = ""
    lty[length(lty)] = 0
   
    legendStandard(lty = lty, pch = 15, col = col,
                   y.intersp = c(rep(1, length(col) - 1), 0.97), ...)
    invisible()
}