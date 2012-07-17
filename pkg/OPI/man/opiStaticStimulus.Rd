\name{opiStaticStimulus}
\alias{opiStaticStimulus}
\title{
Stimulus parameter list.
}
\description{
 List containing stimulus parameters
 with an S3 class attribute of \code{opiStaticStimulus}.
}
\details{ 
The list mus contain the following elements.
\tabular{ll}{
x     \tab coordinate of the center of stimulus in degrees 
           relative to fixation \cr
y     \tab coordinate of the center of stimulus in degrees 
           relative to fixation \cr
image \tab an image to display in a machine specific format \cr
level \tab stimulus level in cd/m2 (ignored if !is.na(image))\cr
size  \tab diameter of target in degrees, or scaling factor for 
           image if specified \cr
color \tab machine specific stimulus color settings 
           (ignored if !is.na(image)) \cr
duration \tab total stimulus duration in milliseconds maximum \cr
responseWindow \tab time (>= 0) in milliseconds to wait for a response from 
                the onset of the stimulus presentation\cr
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
    stim <- list(x=9, y=9, image=NA, 314, size=0.43, color="white", 
                duration=200, responseWindow=1500)
    class(stim) <- "opiStaticStimulus"
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }
