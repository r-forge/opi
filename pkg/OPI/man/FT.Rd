\name{FT}
\alias{FT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Full Threshold 
}
\description{
FT begins with a 4-2dB staircase at level est. If the final estimate 
(last seen) is more than 4dB away from est, a second 4-2 staircase is
completed beginning at the estimate returned from the first.
}
\usage{
FT(est = 25, instRange = c(0, 40), verbose = FALSE, responder, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{est}{
Starting estimate in dB}
  \item{instRange}{
Dynamic range of the instrument c(min,max) in dB
}
  \item{verbose}{
True if you want each presentation printed
}
  \item{responder}{
A function that takes a dB value and numPresentations 
and returns TRUE (seen) or FALSE (not seen)
}
  \item{\dots}{
Extra parameters to pass to the responder function
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a list containing
\item{npres}{Total number of presentations}
\item{respSeq}{Response sequence stored as a list of (seen,dB) pairs}
\item{first}{First staircase estimate in dB}
\item{final}{Final threshold estimate in dB}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Andrew Turpin <aturpin@unimelb.edu.au>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.


responder <- function(db, n) { return (db < 30) }
result <- FT(responder=responder)

    ## a more complicated responder
rHenson <- function(db, n, tt=30, fpr=0.03, fnr=0.03) {
    pxVar <- min(6, exp(-0.066*tt +2.81))  
    prSeeing <- fpr + (1-fpr-fnr)*(1-pnorm(db, mean=tt, sd=pxVar))  
    return (runif(1) < prSeeing)
}
res1 <- FT(responder=rHenson, tt=30, fpr=0.05, fnr=0.05)
res2 <- FT(responder=rHenson, tt=22, fpr=0.15, fnr=0.15)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }
