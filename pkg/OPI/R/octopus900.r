#
# OPI for Octopus 900 
# 
# This would all have been nicer in an OO style, with each implementation
# being a subclass of an opi class, but I don't think it can be in R.
# The OPI standard doesn't want users employing exactly the same function 
# no matter what the underlying implementation, and so there cannot be 
# extra parameters to create method signatures for different classes.
# Similarly, some implementations use exactly the same method signatures,
# again which will confuse R, I think. Anyway, if I am wrong, sorry about that.
# What I've done (use a list of implementations and then use a global
# integer to index them) works and makes sense to the non-OO person.
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
# Author: Andrew Turpin
#
# July 2012
#

# -----------------------------------------------------------------
# Fire up Java

require(rJava)
assign("opi.global.octopusObject", NULL, envir = .GlobalEnv)

# -----------------------------------------------------------------
# Goldmann target sizes in degrees

GOLDMAN <- c(6.5, 13, 26, 52, 104) / 60

# -----------------------------------------------------------------
# INPUT: 
#   eyeSuiteJarLocation      = dir name containing EyeSuite Jar files
#   eyeSuiteSettingsLocation = dir name containing EyeSuite settings
#   eye                 = "right" or "left"
#
#   Both input dirs should INCLUDE THE TRAILING SLASH.
#
# @return NULL if succeed
# @return 1 if already initialised
# @return 2 if failed to make ready
#
octo900.opiInitialize <- function(eyeSuiteJarLocation=NA, eyeSuiteSettingsLocation=NA, eye=NA) {
    if (is.na(eyeSuiteJarLocation))
        stop("You must specify the EyeSuite jar file folder in your call to opiInitialize")
    if (is.na(eyeSuiteSettingsLocation))
        stop("You must specify the EyeSuite settings folder in your call to opiInitialize")
    if (is.na(eye))
        stop("You must specify which eye ('left' or 'right') in your call to opiInitialize")
    if (eye != "left" && eye != "right")
        stop("The eye argument of opiInitialize must be 'left' or 'right'")

    .jinit(c(
        paste(eyeSuiteJarLocation, "jgoodies-binding-2.5.0.jar", sep=""),
        paste(eyeSuiteJarLocation, "jgoodies-common-1.2.1.jar", sep=""),
	    paste(eyeSuiteJarLocation, "HSEyeSuiteBasic.jar", sep=""),
	    paste(eyeSuiteJarLocation, "HSEyeSuiteExtPerimetryViewer.jar", sep=""),
	    paste(eyeSuiteJarLocation, "HSEyeSuiteExtPerimetry.jar", sep=""),
        paste(.Library,"OPIOctopus900","java", sep="/")
    ))

    print(.jclassPath())    # just for debugging, not really needed

    setupBackgroundConstants()

        # the controling object
    assign("opi.global.octopusObject", .jnew("opi.Opi", eyeSuiteSettingsLocation, eye), envir = .GlobalEnv)

	err <- .jcall(.GlobalEnv$opi.global.octopusObject, "I", "opiInitialize")
	if (err == 0)
		return(NULL)
	else
		return(err)
}

###########################################################################
# Get values for fixation, color and bg intensity constants
# from EyeSuite classes, and set globals
#       opi.O900.* 
# to the values of those constants.
# INPUT: None.
# OUTPUT: None.
# SIDE EFFECTS: sets opi.O900.* if possible.
###########################################################################
setupBackgroundConstants <- function() {
    f <- .jfields("com.hs.eyesuite.ext.extperimetryviewer.peristatic.data.exam.Const")

        #
        # check if cName exists as a field in f. If so, set 
        # opi.O900.cName <- class value for cName
        #
    getC <- function(cName) {
        if (length(grep(cName, f)) > 0) 
            assign(paste("opi.O900.",cName,sep=""),
                   .jfield("com.hs.eyesuite.ext.extperimetryviewer.peristatic.data.exam.Const",NULL,cName), 
                   envir = .GlobalEnv)
    }

    getC("FIX_CENTRE")
    getC("FIX_CROSS")
    getC("FIX_RING")
    getC("BG_OFF")
    getC("BG_1")
    getC("BG_10")
    getC("BG_100")
    getC("BG_125")
    getC("BG_150")
    getC("COL_WHITE")
    getC("COL_BLUE")
    getC("COL_YELLOW")
    getC("COL_RED")
    getC("COL_GREEN")
    getC("COL_USER")
}

###########################################################################
# INPUT: 
#   as per OPI spec
#
# Return a list of 
#	err  = string message
#	seen = 1 if seen, 0 otherwise
#	time = reaction time
###########################################################################
octo900.opiPresent <- function(stim, nextStim) { 
	if (is.null(stim)) {
		opiPresent.opiStaticStimulus(NULL)
	} else {
		UseMethod("opiPresent") 
	}
}

octo900.opiPresent.opiStaticStimulus <- function(stim, nextStim) {
    if (is.null(stim)) {
        stimObj <- .jnull("opi/OpiStaticStimulus")
        nextObj <- .jnull("opi/OpiStaticStimulus")
    } else {
        stimObj <- .jnew("opi/OpiStaticStimulus", stim$x*10.0, stim$y*10.0, cdTodb(stim$level)*10.0)
        nextObj <- .jnew("opi/OpiStaticStimulus", nextStim$x*10.0, nextStim$y*10.0, nextStim$level) # level no matter
	    .jcall(stimObj, "V", "setSize", as.double(which.min(abs(GOLDMAN - stim$size))))
	    .jcall(stimObj, "V", "setDuration", as.double(stim$duration))
	    .jcall(stimObj, "V", "setResponseWindow", as.double(stim$responseWindow))
	    .jcall(nextObj, "V", "setSize", as.double(which.min(abs(GOLDMAN - nextStim$size))))
	    .jcall(nextObj, "V", "setDuration", as.double(nextStim$duration))
	    .jcall(nextObj, "V", "setResponseWindow", as.double(nextStim$responseWindow))
    }

    # ret <- .jcall(opi.global.octopusObject, "Lopi/OpiPresentReturn;", "opiPresent", stimObj, nextObj)

    done <- FALSE
    while (!done) {
	done <- TRUE
    	tryCatch(ret <- .jcall(opi.global.octopusObject, "Lopi/OpiPresentReturn;", "opiPresent", stimObj, nextObj), 
	             java.util.ConcurrentModificationException = function(e) { done = FALSE })
    }

    return(list(
	    err =.jcall(ret, "S", "getErr"), 
	    seen=.jcall(ret, "I", "getSeen"), 
	    time=.jcall(ret, "I", "getTime")
	))
}

###########################################################################
#
# XXX What the hell are these params?
#
# @return NULL is succeed.
# @return -1 if opiInitialize has not been successfully called
# @return -2 trouble setting backgound color
# @return -3 trouble setting fixation
###########################################################################
octo900.opiSetBackground <- function(lum=NA, color=NA, fixation=NA, fixIntensity) {
    if (is.null(lum)) {
    }

    ret <- .jcall(opi.global.octopusObject, "I", "opiSetBackground", lum, color, fixation, fixIntensity)
    if (ret == 0)
        return(NULL)
    else
        return(ret)
}

###########################################################################
# return NULL on success (in fact, always!)
###########################################################################
octo900.opiClose <- function() {
    ret <- .jcall(opi.global.octopusObject, "I", "opiClose")
    return(NULL)
}
