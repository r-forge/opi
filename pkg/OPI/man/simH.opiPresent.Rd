\name{simH.opiPresent}
\alias{simH.opiPresent}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
opiPresent for a simulated patient Henson et al variability.
}
\description{
The response to a stimuli is determined by sampling from a
Frequency-of-Seeing (FoS) curve defined as in Henson et al (2000).
}
\usage{
simH.opiPresent(stim, nextStim=NULL, fpr=0.03, fnr=0.01, tt=30)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stim}{
    An OPI stimulus object.
}
  \item{nextStim}{
    An OPI stimulus object, default is NULL.
}
    \item{fpr}{The false positive rate in [0,1].}
    \item{fnr}{The false positive rate in [0,1].}
    \item{tt}{The true threshold in Humphrey dB.}
}
\details{
The response to a stimuli is determined by sampling from a
Frequency-of-Seeing (FoS) curve (also known as the psychometric
function) with formula \code{fpr+(1-fpr-fnr)*(1-pnorm(x, tt, pxVar))},
where \code{x} is the stimulus value in Humphrey dB, and pxVar is
\code{min(simH.cap, exp(A*tt + B))}. The ceiling \code{simH.cap}
is set with the call to \code{chooseOpi}, and \code{A} and \code{B}
are from Table 1 in Henson et al (2000). Which values are used is
determined by \code{simH.type} which is also set in the call to
\code{chooseOpi}.

}
\value{
    As for \code{\link{opiPresent}}.
}
\references{
David B. Henson, Shaila Chaudry, Paul H. Artes, E. Brian Faragher, and
Alec Ansons.  
Response Variability in the Visual Field: Comparison of Optic Neuritis,
Glaucoma, Ocular Hypertension, and Normal Eyes.
Investigative Ophthalmology & Visual Science, February 2000, Vol. 41(2).

http://perimetry.org/OPI
}
\author{
Andrew Turpin <aturpin@unimelb.edu.au>
}

\seealso{
\code{\link{chooseOpi}}
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

    chooseOpi("SimHenson", type="C", cap=6)
    opiInitialize()

        # Stimulus is Size III white-on-white as in the HFA
    makeStim <- function(db, n) {
        s <- list(x=9, y=9, level=dbTocd(db), size=0.43, color="white",
                 duration=200, responseWindow=1500)
        class(s) <- "opiStaticStimulus"

        return(s)
    }

        # call the Full Threshold staircase
    result <- FT(est=25, makeStim=makeStim, tt=30, fpr=0.15, fnr=0.03)

    opiClose()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }
