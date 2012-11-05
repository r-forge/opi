\name{opi.procedure.save}
\alias{opi.procedure.save}
\title{
opi.procedure.save
}
\description{
Save a list of procedure states that is generated
by \code{ZEST.start}, \code{FT.start}, etc.
}
\usage{
opi.procedure.save(stateList, filename, append=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{stateList}{
    Lists of current states of procedures as
    returned by \code{ZEST.start}, \code{FT.start}, etc.
}
\item{filename}{
    Name of file in/from which to save/load.
}
\item{append}{
    If true, stateList is appended to filename, otherwise stateList becomes the only
    entry in filename.
}
}
\details{
Save a list of procedure states that is generated
by \code{ZEST.start}, \code{FT.start}, etc.
}

\references{
Please cite:
A. Turpin, P.H. Artes and A.M. McKendrick
"The Open Perimetry Interface: An enabling tool for clinical visual psychophysics",
Journal of Vision 12(11) 2012.

http://perimetry.org/OPI
}

\author{
Andrew Turpin <aturpin@unimelb.edu.au>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{ZEST}}, \code{\link{FT}}, \code{\link{opi.procedure.load}}
}
\examples{
makeStimHelper <- function(db,n, x, y) {  # returns a function of (db,n)
    ff <- function(db, n) db+n

    body(ff) <- substitute(
        {s <- list(x=x, y=y, level=dbTocd(db), size=0.43, color="white",
                  duration=200, responseWindow=1500)
         class(s) <- "opiStaticStimulus"
         return(s)
        }
        , list(x=x,y=y))
    return(ff)
}

    # List of (x, y, true threshold) triples
locations <- list(c(9,9,30), c(-9,-9,32), c(9,-9,31), c(-9,9,33))

    # Setup starting states for each location
states <- lapply(locations, function(loc) {
    ZEST.start(
        makeStim=makeStimHelper(db,n,loc[1],loc[2]),
        stopType="S", stopValue= 1.5, tt=loc[3], fpr=0.03, fn=0.01)
})

opi.procedure.save(states, "Zest_states.opi")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }
