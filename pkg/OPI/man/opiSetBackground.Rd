\name{opiSetBackground}
\alias{opiSetBackground}
\title{
Set background using OPI.
}
\description{
Generic function for setting background of the chosen OPI implementation that is set with
\code{chooseOpi()}.
}
\usage{
opiSetBackground(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{\dots}{Implementation specific parameters. See details.}
}
%\details{
%}

\value{
Returns NULL if initialization succeeded, 
otherwise an implementation dependant error.
}
\references{
http://perimetry.org/OPI
}
\author{
Andrew Tuprin <aturpin@unimelb.edu.au>
}

\seealso{
\code{\link{chooseOpi}}
}

\examples{
chooseOpi("SimGaussian")
if (!is.null(opiInitialize(sd=2)))
    stop("opiInitialize failed")
if (!is.null(opiSetBackground()))
    stop("opiSetBackground failed, which is very surprising!")
}

\keyword{ misc }
