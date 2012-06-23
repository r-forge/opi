\name{opiTemporalStimulus}
\alias{opiTemporalStimulus}
\title{
Stimulus parameter list.
}
\description{
 List containing stimulus parameters
 with an S3 class attribute of \code{opiTemporalStimulus}.
}
\details{ 
\tabular{ll}{
x     \tab coordinate of the center of stimulus in degrees relative to fixation \cr
y     \tab coordinate of the center of stimulus in degrees relative to fixation \cr
image \tab an image to display in a machine specific format \cr
lut   \tab if \code{is.na(image)} then this is a lookup 
           table (vector) for stimulus level
           at each step of rate Hz in cd/\eqn{m^2}{m^2}. 
           If image is specified, then this is a list of
           images, in the same format as image, that is stepped through at rate Hz.\cr
size  \tab diameter of target in degrees, or scaling factor for image if specified \cr
color \tab machine specific stimulus color settings (ignored if \code{!is.na(image)}) \cr
rate  \tab frequency with which lut is processed in Hz \cr
duration  \tab total length of stimulus flash in milliseconds. There is no guarantee that
\code{duration \%\% length(lut)/rate == 0}. That is, the onus is on the user to
ensure the duration is a multiple of the period of the stimuli. \cr
responseWindow \tab maximum time (>= 0) in milliseconds to wait for a response from the
onset of the stimulus presentation \cr
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
    # A Size III flickering with a 10Hz square wave at
    # location (7,7) with luminance 10 dB (HFA) 
stim <- list(x=7, y=7, size=0.43, color="white", 
                rate=20,        # one lut step per 50 ms 
                lut=c(0,318),   # so one full lut per 100 ms == 10Hz 
                duration=400,   # and 4 cycles per stimulus
                responseWindow=1500) 
class(stim) <- "opiTemporalStimulus"
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }
