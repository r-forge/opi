#
# Choose which OPI implementation to run
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

################################################################################
# Input parameters
#   opi      Either "Octopus900", "HEP", "SimHenson"
#   type     For SimHenson type=N|G|C for Normal, Glaucoma, Combined
# Returns
#   TRUE     If successful
#   FALSE    Otherwise
################################################################################
chooseOpi <- function(opi="SimHenson", type="N") {
    if (opi == "Octopus900") {
        stop("Have not implemented chooseOPI(Octopus900)")
    } else if (opi == "HEP") {
        stop("Have not implemented chooseOPI(HEP)")
    } else if (opi == "SimHenson") {
        .GlobalEnv$opiInitialize    <- simH.opiInitialize
        .GlobalEnv$opiClose         <- simH.opiClose
        .GlobalEnv$opiSetBackground <- simH.opiSetBackground
        .GlobalEnv$opiQueryDevice   <- simH.opiQueryDevice
        if (type == "N") {
            .GlobalEnv$opiPresent <- simH.opiPresent.N
        } else if (type == "G") {
            .GlobalEnv$opiPresent <- simH.opiPresent.G
        } else if (type == "C") {
            .GlobalEnv$opiPresent <- simH.opiPresent.C
        } else {
            stop("Bad type specified for SimHenson in chooseOpi()")
        }
    }
}
