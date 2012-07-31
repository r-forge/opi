\name{ZEST}
\alias{ZEST}
\alias{QUEST}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ZEST
}
\description{
An implementation of the Bayesian test procedure of King-Smith et al.
}
\usage{
ZEST(domain=0:40, prior=rep(1/length(domain),length(domain)), 
     likelihood=sapply(0:40, function(tt) { 0.03 + (1-0.03-0.03)*(1-pnorm(0:40, tt, 1)) }),
     stopType="S", stopValue=1.5,
     maxSeenLimit=2, minNotSeenLimit=2,
     maxPresentations=100,
     verbose=0, 
     makeStim, 
     stimChoice="mean",
     ...) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{domain}{
List of values over which pdf is kept.
}
  \item{prior}{
Starting probability distribution over domain.
}
\item{likelihood}{Matrix where \code{likelihood[s,t]} is likelihood of seeing \code{s} given \code{t} 
                  is the true threshold. That is, Pr(s|t) where \code{s} and \code{t} are indexs into domain.
}
\item{stopType}{ \code{N}, for number of presentations; \code{S}, for standard deviation of the pdf; and
\code{H}, for the entropy  of the pdf.
}
\item{stopValue}{Value for number of presentations (\code{stopType=N}), standard deviation (\code{stopType=S)} or
Entropy (\code{stopType=H}).
}
\item{minNotSeenLimit}{ Will terminate if lowest domain value is not seen this many times.
}
\item{maxSeenLimit}{    Will terminate if highest domain value is seen this many times.
}
\item{maxPresentations}{ Maximum number of presentations regarless of \code{stopType}.
}
  \item{verbose}{
\code{verbose=0} does nothing, \code{verbose=1} stores pdfs for returning, and \code{verbose=2} stores pdfs and also prints each
presentaion.
}
  \item{makeStim}{
A function that takes a dB value and numPresentations 
and returns an OPI datatype ready for passing to opiPresent. See examples.
}
  \item{stimChoice}{
    A true ZEST procedure uses the \code{"mean"} of the current pdf as the stimulus, but \code{"median"} and
\code{"mode"} (as used in a QUEST procedure) are provided for your enjoyment.
}
  \item{\dots}{
Extra parameters to pass to the opiPresent function
}
}
\details{
    This is an implementation of King-Smith et al.'s ZEST procedure.
    All presentaions are rounded to an element of the supplied domain.

    Note this function will repeatedly call \code{opiPresent} for a stimulus 
    until \code{opiPresent} returns \code{NULL} (ie no error occured).
}
\value{
Returns a list containing
\item{npres}{Total number of presentations used.}
\item{respSeq}{Response sequence stored as a list of (stimulus, seen) pairs.}
\item{pdfs}{If \code{verbose} is bigger than 0, then this is a list of the pdfs used for each presentation,
otherwise NULL.}
\item{final}{The mean/median/mode of the final pdf, depending on \code{stimChoice}, which is the determined threshold.}
}
\references{

P.E. King-Smith, S.S. Grigsny, A.J. Vingrys, S.C. Benes, and A.
Supowit.  "Efficient and Unbiased Modifications of the QUEST Threshold
Method: Theory, Simulations, Experimental Evaluation and Practical
Implementation", Vision Research 34(7) 1994. Pages 885-912.

A.B. Watson and D.G. Pelli. 
"QUEST: A Bayesian adaptive psychophysical method", 
Perception and Psychophysics 33 1983. 
Pages 113-l20.

http://perimetry.org/OPI
}
\author{
Andrew Turpin <aturpin@unimelb.edu.au>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{dbTocd}}, \code{\link{opiPresent}}
}
\examples{
    # Stimulus is Size III white-on-white as in the HFA
makeStim <- function(db, n) { 
    s <- list(x=9, y=9, level=dbTocd(db), size=0.43, color="white",
             duration=200, responseWindow=1500)
    class(s) <- "opiStaticStimulus"

    return(s)
}
chooseOpi("SimHenson")
if (!is.null(opiInitialize("C", 6)))
    stop("opiInitialize failed")

repp <- function(...) sapply(1:100, function(i) ZEST(makeStim=makeStim, ...))
a <- repp(stopType="H", stopValue=  3, verbose=0, tt=30, fpr=0.03)
b <- repp(stopType="S", stopValue=1.5, verbose=0, tt=30, fpr=0.03)
c <- repp(stopType="S", stopValue=2.0, verbose=0, tt=30, fpr=0.03)
d <- repp(stopType="N", stopValue= 50, verbose=0, tt=30, fpr=0.03)
e <- repp(prior=dnorm(0:40,m=0,s=5), tt=30, fpr=0.03)
f <- repp(prior=dnorm(0:40,m=10,s=5), tt=30, fpr=0.03)
g <- repp(prior=dnorm(0:40,m=20,s=5), tt=30, fpr=0.03)
h <- repp(prior=dnorm(0:40,m=30,s=5), tt=30, fpr=0.03)

layout(matrix(1:2,1,2))
boxplot(lapply(list(a,b,c,d,e,f,g,h), function(x) unlist(x["final",])))
boxplot(lapply(list(a,b,c,d,e,f,g,h), function(x) unlist(x["npres",])))

if (!is.null(opiClose()))
    warning("opiClose() failed")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }
