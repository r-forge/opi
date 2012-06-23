\name{chooseOpi}
\alias{chooseOpi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Choose an implementation of the OPI.
}
\description{
Chooses an implementation of the OPI to use.
}
\usage{
chooseOpi(opi = "SimHenson", type = "N", cap=6)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{opi}{
    A character string that is either: "SimHenson", for a simulator that
    uses variability according to Henson et al (2000) where variability
    increases as Humphrey dB value decreases; "Octopus900", for interfacing
    with the Octopus 900; "HEP", for interfacing with the
    Heidelberg Edge Perimeter.
}
  \item{type}{
    An extra controlling parameter that can be used to subspecialise 
    your machine. 
    }
  \item{cap}{
    When \code{opi="SimHenson"}, \code{cap} sets the maximum 
    standard deviation that can be derived from Henson et al's formula
    for determing FoS/psychometric function slope.
    }
}
\details{ 
    When \code{opi} is \code{"SimHenson"}, then \code{type} can take on 3
    values
    \tabular{cl}{
    \code{"N"} \tab which uses the equation for normal variability \cr
    \code{"G"} \tab which uses the equation for POAG variability \cr
    \code{"C"} \tab which uses the equation for combined variability \cr
    }
}
\value{ Returns nothing.}

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

%\note{ }

\examples{
    chooseOpi("SimHenson", type="C", cap=6)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }
