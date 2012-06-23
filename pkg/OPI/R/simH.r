#
# An iimplementation of the OPI that simulates responses using 
# Henson et al (2000) variability.
#
# Author: Andrew Turpin    (aturpin@unimelb.edu.au)
# Date: June 2012
#
# Copyright 2012 Andrew Turpin and Jonathan Denniss
# This program is part of the OPI (http://perimetry.org/OPI).
# OPI is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

simH.opiClose         <- function() { }
simH.opiInitialize    <- function() { }
simH.opiSetBackground <- function() { }
simH.opiQueryDevice   <- function() { return (list(type="SimHenson")) }

simH.type <- NA
simH.cap  <- NA

simH.opiPresent <- function(stim, nextStim=NULL, fpr=0.03, fnr=0.01, tt=30) { UseMethod("simH.opiPresent") }
setGeneric("simH.opiPresent")

#
# Helper function that allows different coefficients from Table 1 of Henson 2000.
#
simH.present <- function(db, cap=6, fpr=0.03, fnr=0.01, tt=30, A, B) {
    pxVar <- min(cap, exp(A*db + B)) # variability of patient, henson formula 

    prSeeing <- fpr + (1-fpr-fnr)*(1-pnorm(db, mean=tt, sd=pxVar))    

    return ( list(
        err = NULL,
        seen= runif(1) < prSeeing,
        time= 0
    ))
}#

#
# stim is list of type opiStaticStimulus
#
simH.opiPresent.opiStaticStimulus <- function(stim, nextStim=NULL, fpr=0.03, fnr=0.01, tt=30) {
    if (.GlobalEnv$simH.type == "N") {
        return(simH.present(cdTodb(stim$level), .GlobalEnv$simH.cap, fpr, fnr, tt, -0.066, 2.81))
    } else if (.GlobalEnv$simH.type == "G") {
        return(simH.present(cdTodb(stim$level), .GlobalEnv$simH.cap, fpr, fnr, tt, -0.098, 3.62))
    } else if (.GlobalEnv$simH.type == "C") {
        return(simH.present(cdTodb(stim$level), .GlobalEnv$simH.cap, fpr, fnr, tt, -0.081, 3.27))
    } else {
        stop(paste("Invalid type parameter passed to simH.opiPresent():",.GlobalEnv$simH.type))
    }
}

##########################################
simH.opiPresent.opiTemporalStimulus <- function(stim, nextStim=NULL, ...) {
    stop("ERROR: haven't written simH temporal persenter yet")
}

simH.opiPresent.opiKineticStimulus <- function(stim, nextStim=NULL, ...) {
    stop("ERROR: haven't written simH kinetic persenter yet")
}
