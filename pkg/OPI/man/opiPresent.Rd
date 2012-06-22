\name{opiPresent}
\alias{opiPresent}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Use OPI to present stimulus.
}
\description{
Generic function for presentation of stimulus stim. 
Depending on your choice of OPI implementation set using 
\code{chooseOPI()}, 
different \code{opiPresent} methods will be called. 
Use \code{methods(opiPresent)} to list the current.
}
\usage{
opiPresent(stim, nextStim=NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stim}{
    A list of class \code{opiStaticStimulus}, \code{opiKineticStimulus}, or
\code{opiTemporalStimulus}. 
}
  \item{nextStim}{
    As for \code{stim}, but the next presentation to be made. 
    This might be useful
    on some machines, particularly projector based systems, where preparations
    for the next presentation can be made while waiting for a response to the
    current.
}
  \item{\dots}{
    Parameters specific to your chosen opi implementation.
}
}
\details{
\code{opiPresent} 
is blocking in that it will not return until either a response
is obtained, or at least the responseWindow milliseconds has expired. (Note
that more time might have expired.) Specifying \code{nextStim} 
allows the implementing
machine to use the time waiting for a 
response to \code{stim} to make preparations
for the next stimuli. (For example retargeting the projector or moving
aperture and/or filter wheels.) 
There is no guarantee that the next call to
\code{opiPresent} will have \code{nextStim} as the first argument; this 
should be checked by the machine specific implementations.
}
\value{
A list containing
\item{err }{\code{NULL} if no error occurred, otherwise a machine specific error
message.
This should include errors when the specified size cannot be achieved by
the device (for example, in a projection system with an aperture wheel of
predefined sizes.) If \code{stim} is \code{NULL}, then \code{err} contains the status of the
machine.}
\item{seen }{\code{TRUE} if a response was detected in the allowed
\code{responseWindow}, \code{FALSE} otherwise.}
\item{time}{The time in milliseconds from the onset (or offset, machine
specific) of the presentation
until the response from the subject if \code{seen} is \code{TRUE}. 
If \code{seen} is \code{FALSE}, this
value is undefined.
}
}
\references{
http://perimetry.org/OPI
}
\author{
Andrew Tuprin <aturpin@unimelb.edu.au>
}
%\note{
%%%  ~~further notes~~
%}

\seealso{
\code{\link{opiStaticStimulus}}, 
\code{\link{opiKineticStimulus}}, 
\code{\link{opiTemporalStimulus}}, 
\code{\link{chooseOPI}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }
