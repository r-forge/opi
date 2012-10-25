#
# The standard OPI functions that simply distributes calls to 
#   opi.implementations[[opi.global.chooser]]
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

opiDistributor <- function(method, ...) {
    if (!exists("opi.global.chooser") || (is.na(opi.global.chooser))) {
        msg <- "You have not chosen a valid OPI implementation. Use chooseOpi()"
        warning(msg)
        return(msg)
    }
    toCall <- opi.implementations[[opi.global.chooser]][[method]]
    allowedArgs <- names(formals(toCall))
    haveArgs    <- names(list(...))
#print(paste("Allowed args: ", allowedArgs))
#print(paste("Have args: ", haveArgs))
    argsToPass  <- intersect(allowedArgs, haveArgs)
#print(paste("Passing args: ", argsToPass))
    do.call(toCall, list(...)[argsToPass])
}

opiPresent        <- function(stim,nextStim=NULL,...) { opiDistributor("opiPresent", stim=stim, nextStim=nextStim, ...) }

opiInitialize     <- function(...) { opiDistributor("opiInitialize", ...) }

opiSetBackground  <- function(...) { opiDistributor("opiSetBackground", ...) }

opiQueryDevice    <- function(...) { opiDistributor("opiQueryDevice", ...) }

opiClose          <- function(...) { opiDistributor("opiClose", ...) }
