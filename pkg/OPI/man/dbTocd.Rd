\name{dbTocd}
\alias{dbTocd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert Humphrey dB to cd/\eqn{\mbox{m}^2}{m^2}
}
\description{
Given a value in Humphrey dB, return the cd/\eqn{\mbox{m}^2}{m^2} equivalent.
}
\usage{
dbTocd(db)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{db}{
    Value to convert in Humphrey dB
}
}
%\details{
%}
\value{
Returns cd/\eqn{\mbox{m}^2}{m^2} value.
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
%\note{
%}

\seealso{
\code{\link{cdTodb}}
}
\examples{
    cd <- dbTocd(0)   # 10000/pi
    cd <- dbTocd(10)  # 1000/pi
    cd <- dbTocd(20)  # 100/pi
    cd <- dbTocd(30)  # 10/pi
    cd <- dbTocd(40)  # 1/pi
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }
