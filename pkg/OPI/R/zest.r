#
# ZEST algorithm for a single location.
# Based on Andrew Turpin's implementation in his Barramundi Simulator.
#
# Author: Andrew Turpin    (aturpin@unimelb.edu.au)
#         Jonathan Denniss (jdenniss@unimelb.edu.au)
# Date: July 2012
#
# Copyright 2012 Andrew Turpin 
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
#   domain        List of dB values over which pdf is kept
#   prior         Probability distribution over domain.
#   likelihood    matrix where likelihood[s,t] is likelihood of seeing s given t is true thresh (Pr(s|t)
#                 where s and t are indexs into domain
#   stopType      N | S | H
#   stopValue     Value for num prs (N), stdev (S) of Entropy (H)
#   minNotSeenLimit Will terminate if lowest domain value not seen this many times
#   maxSeenLimit    Will terminate if highest domain value seen this many times
#   maxPresentations Maximum number of presentations
#   verbose       1 if you want pdfs returned, 2 is 1+print, 0 for none
#   makeStim      A helper function to create the required
#                 OPI data type for passing to opiPresent
#   stimChoice    "mean", "median", "mode"
#   ...           Parameters for opiPresent
# Returns a list containing
#   npres    Total number of presentations
#   respSeq  Response sequence stored as a list of (seen,dB) pairs
#   pdfs     Sequence of pdfs used (if verbose)
#
# Note 
#   1) stims are rounded to nearest domain entry 
#   2) opiPresent called infinitely until no error
################################################################################
ZEST <- function(domain=0:40, prior=rep(1/length(domain),length(domain)), 
            likelihood=sapply(0:40, function(tt) { 0.03 + (1-0.03-0.03)*(1-pnorm(0:40, tt, 1)) }),
            stopType="S",
            stopValue=1.5,
            maxSeenLimit=2,
            minNotSeenLimit=2,
            maxPresentations=100,
            verbose=0, makeStim, 
            stimChoice="mean",
            ...) {

    ##########################
    # Validate params
    ##########################
    if (!is.element(stopType, c("S", "H", "N")))
        stop("ZEST: stopType must be one of 'S', 'N', or 'H'")
    if (nrow(likelihood) != length(domain))
        stop(paste("ZEST: not enough rows in likelihood. Expect",length(domain)))
    if (ncol(likelihood) != length(domain))
        stop(paste("ZEST: not enough cols in likelihood. Expect",length(domain)))

    pdf <- prior/sum(prior)

    ##########################
    # little helper functions
    ##########################
    stdev <- function(p) { sqrt(sum(p*domain*domain) - sum(p * domain)^2) }
    entropy <- function(p) {
        z <- which(p > 0)
        return(-sum(p[z] * log2(p[z])))
    }

    ##########################
    # Set up loop, and then go!
    ##########################
    pdfs <- NULL
    if (verbose > 0) 
        pdfs <- c(pdfs, list(pdf))
    fullResponseSeq <- list()
    numPres <- 0
    minNotSeen <- maxSeen <- 0
    keepGoing <- TRUE
    while(keepGoing) {
        if (stimChoice == "mean") {
            stimIndex <- which.min(abs(domain - sum(pdf * domain)))
        } else {
            stop(paste("stimChoice = ",stimChoice," not implemented"))
        }
        stim <- domain[stimIndex]
        opiResp <- opiPresent(stim=makeStim(stim, numPres), nextStim=NULL, ...)
        while (!is.null(opiResp$err))
            opiResp <- opiPresent(stim=makeStim(stim, numPres), nextStim=NULL, ...)
        seen  <- opiResp$seen
        numPres <- numPres + 1
        
        if(seen) { 
            if (stimIndex == length(domain)) maxSeen <- maxSeen + 1
            pdf <- pdf * likelihood[stimIndex, ]
        } else {
            if (stimIndex == 1) minNotSeen <- minNotSeen + 1
            pdf <- pdf * (1 - likelihood[stimIndex, ])
        }
        pdf <- pdf/sum(pdf)

        if (verbose == 2) {
            cat(sprintf("Presentation %2d: ", numPres))
            cat(sprintf("stim= %5s repsonse=%s ", stim, seen))
            cat(sprintf("stdev= %8.4g H= %8.4g\n", stdev(pdf), entropy(pdf)))
        }
        fullResponseSeq[[numPres]] <- c(stim, seen)

        if (verbose > 0)
            pdfs <- c(pdfs, list(pdf))

        keepGoing <- (
            (numPres < maxPresentations) &&
            (minNotSeen < minNotSeenLimit) &&
            (maxSeen < maxSeenLimit) &&
            (
               ((stopType == "S") && (stdev(pdf) > stopValue))
            || ((stopType == "H") && (entropy(pdf) > stopValue))
            || ((stopType == "N") && (numPres < stopValue))
            )
        )
    }# end presentation loop

    return(list(
        npres=length(fullResponseSeq),    # number of presentations
        respSeq=fullResponseSeq,          # reposnse sequence (list of pairs)
        pdfs=pdfs,                        # list of pdfs used (if verbose > 0)
        final=sum(pdf*domain)             # final threshold estimate
    ))
}#ZEST()

############################################################
# Tests
############################################################
##makeStim <- function(db, n) { 
##         s <- list(x=9, y=9, level=dbTocd(db), size=0.43, color="white",
##                  duration=200, responseWindow=1500)
##         class(s) <- "opiStaticStimulus"
##     
##         return(s)
##     }
##
##a <- sapply(1:100, function(i) ZEST(makeStim=makeStim, stopType="H", stopValue=  3, verbose=0, tt=50, fpr=0.03))
##b <- sapply(1:100, function(i) ZEST(makeStim=makeStim, stopType="S", stopValue=1.5, verbose=0, tt=50, fpr=0.03))
##c <- sapply(1:100, function(i) ZEST(makeStim=makeStim, stopType="S", stopValue=2.0, verbose=0, tt=50, fpr=0.03))
##d <- sapply(1:100, function(i) ZEST(makeStim=makeStim, stopType="N", stopValue= 50, verbose=0, tt=50, fpr=0.03))
##
##layout(matrix(1:2,1,2))
##boxplot(lapply(list(a,b,c,d), function(x) unlist(x["final",])))
##boxplot(lapply(list(a,b,c,d), function(x) unlist(x["npres",])))
##
##
##a <- sapply(1:100, function(i) ZEST(makeStim=makeStim, prior=dnorm(0:40,m=0,s=5), tt=30, fpr=0.03))
##b <- sapply(1:100, function(i) ZEST(makeStim=makeStim, prior=dnorm(0:40,m=10,s=5), tt=30, fpr=0.03))
##c <- sapply(1:100, function(i) ZEST(makeStim=makeStim, prior=dnorm(0:40,m=20,s=5), tt=30, fpr=0.03))
##d <- sapply(1:100, function(i) ZEST(makeStim=makeStim, prior=dnorm(0:40,m=30,s=5), tt=30, fpr=0.03))
##layout(matrix(1:2,1,2))
##boxplot(lapply(list(a,b,c,d), function(x) unlist(x["final",])))
##boxplot(lapply(list(a,b,c,d), function(x) unlist(x["npres",])))

## repp <- function(...) sapply(1:100, function(i) ZEST(makeStim=makeStim, ...))
## a <- repp(stopType="H", stopValue=  3, verbose=0, tt=30, fpr=0.03)
## b <- repp(stopType="S", stopValue=1.5, verbose=0, tt=30, fpr=0.03)
## c <- repp(stopType="S", stopValue=2.0, verbose=0, tt=30, fpr=0.03)
## d <- repp(stopType="N", stopValue= 50, verbose=0, tt=30, fpr=0.03)
## e <- repp(prior=dnorm(0:40,m=0,s=5), tt=30, fpr=0.03)
## f <- repp(prior=dnorm(0:40,m=10,s=5), tt=30, fpr=0.03)
## g <- repp(prior=dnorm(0:40,m=20,s=5), tt=30, fpr=0.03)
## h <- repp(prior=dnorm(0:40,m=30,s=5), tt=30, fpr=0.03)
## 
## layout(matrix(1:2,1,2))
## boxplot(lapply(list(a,b,c,d,e,f,g,h), function(x) unlist(x["final",])))
## boxplot(lapply(list(a,b,c,d,e,f,g,h), function(x) unlist(x["npres",])))
