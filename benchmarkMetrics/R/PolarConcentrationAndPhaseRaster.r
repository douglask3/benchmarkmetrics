PolarConcentrationAndPhase.RasterStack <-
        function(dat, phase_units = "radians", n = min(12, nlayers(dat)),
                 disagFact = NaN, justPhase = FALSE) {
    if (nlayers(dat) < n) {
        warning(paste('number of layers in dat is less than n. n set to',
                      nlayers(dat)))
    }
    if (nlayers(dat) > n) {
        dat0 = dat
        dat  = dat0[[1:n]]

        for (i in 1:n) {
            index = seq(i, nlayers(dat0), by = n)
            dat[[i]] = mean(dat0[[index]])
        }
    }
    if (!is.na(disagFact))
     dat = layer.apply(dat, disaggregate, disagFact, method = "bilinear")

    out        = dat[[1:2]]
    names(out) = c('Phase', 'Concentration')

    vout = PolarConcentrationAndPhase(values(dat), phase_units)
    test = sum(dat)==0

    if (justPhase) index = 1 else index = 1:2
    for (i in index) {
        out[[i]] = vout[[i]]
        out[[i]][test] = NaN
    }
    if (justPhase) return(out[[1]])
    return(out)
}

PolarConcentrationAndPhase.RasterBrick <- function(...)
    PolarConcentrationAndPhase.RasterStack(...)
