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
\details{
\subsection{Octopus900}{ 
    \code{opiSetBackground(lum, color, fixation, fixIntensity)} 

    \code{lum} is the brightness of the background. Could be 
    \code{opi.O900.BG_OFF},
    \code{opi.O900.BG_1},
    \code{opi.O900.BG_10},
    \code{opi.O900.BG_100},
    \code{opi.O900.BG_125}, or
    \code{opi.O900.BG_150}. Presumably the numbers are cd/\eqn{\mbox{m}^2}{m^2}. 

    \code{color} is color of background. One of
    \code{opi.O900.COL_WHITE},
    \code{opi.O900.COL_BLUE},
    \code{opi.O900.COL_YELLOW},
    \code{opi.O900.COL_RED},
    \code{opi.O900.COL_GREEN}, or
    \code{opi.O900.COL_USER}, with no gaurantee that any will work!

    \code{fixation} is one of \code{opi.O900.FIX_CENTRE}, \code{opi.O900.FIX_CROSS}, or \code{opi.O900.FIX_RING}.

    \code{fixIntensity} is unknown. Could be tenths of a dB.
}
\subsection{SimHenson}{ \code{opiSetBackground} has no arguments and no effect.  }
\subsection{SimGaussian}{ \code{opiSetBackground} has no arguments and no effect.  }
}

\value{
Returns NULL if succeeded, 
otherwise an implementation dependant error as follows.

\subsection{Octopus900}{ 
    -1 indicates \code{opiInitialize} has not been called.

    -2 indicates could not set the background color.

    -3 indicates could not set the fixation marker.
}
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
