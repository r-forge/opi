
simH.opiClose <- function() { }

simH.opiInitialize <- function() { }

simH.opiSetBackground <- function() { }

simH.opiQueryDevice <- function() { 
    return (list(type="SimHenson"))
}


simH.opiPresent <- function(stim, ...) { UseMethod("simH.opiPresent") }
setGeneric("simH.opiPresent")

    # stim in dB
simH.opiPresent.opiStaticStimulus <- function(stim, fpr=0.03, fnr=0.01, tt=30) {
        # variability of patient, henson formula for normals 
    db <- cdTodb(stim$level)
    pxVar <- min(6, exp(-0.066*db + 2.81))

    prSeeing <- fpr + (1-fpr-fnr)*(1-pnorm(db, mean=tt, sd=pxVar))    

    return ( list(
        err = NULL,
        seen= runif(1) < prSeeing,
        time= 0
    ))
}

simH.opiPresent.opiTemporalStimulus <- function(stim) {
    stop("ERROR: haven't written simH temporal persenter yet")
}

simH.opiPresent.opiKineticStimulus <- function(stim) {
    stop("ERROR: haven't written simH kinetic persenter yet")
}
