\name{opiKineticStimulus}
\alias{opiKineticStimulus}
\title{
Stimulus parameter list.
}
\description{
 List containing stimulus parameters
 with an S3 class attribute of \code{opiKineticStimulus}.
}
\details{ 
\tabular{ll}{
path    \tab list of (x,y) coordinates in degrees that is usable by
\code{xy.coords()}\cr
image   \tab \code{image[i]} is the image to display (in a machine specific format) in
the section of the path specified by \code{path[i]..path[i+1]}. \cr
levels  \tab if \code{is.na(image)} then \code{levels[i]} is the stimulus level in
cd/\eqn{\mbox{m}^2}{m^2} in the section of the path specified by
\code{path[i]..path[i+1]}\cr
sizes   \tab \code{sizes[i]} is the size of stimulus (diameter in degrees) to use for
the section of path specified by \code{path[i]..path[i+1]}, or a scaling factor for
\code{images[i]}.\cr
colors  \tab \code{colors[i]} is the color to use for the stimulus in the section of
path specified by \code{path[i]..path[i+1]}. Ignored if !is.na(image). \cr
speeds  \tab \code{speeds[i]} is the speed (degrees per second) for the stimulus to
traverse the path specified by \code{path[i]..path[i+1]}.\cr
... \tab machine specific parameters \cr
}
}

\references{
http://perimetry.org/OPI
}

\author{
Andrew Turpin <aturpin@unimelb.edu.au>
}

\examples{
    # A Size III white kinetic stimuli on
    # a bilinear path {(27,27), (15,20), (0,0)} 
stim <- list(path=list(x=c(27,15,0), y=c(27,20,0)),
                sizes=rep(0.43,2), 
                colors=rep("white",2), 
                levels=rep(318,2), 
                speeds=c(4,3))
class(stim) <- "opiKineticStimulus"
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }