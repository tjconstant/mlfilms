\name{dispersion_scan}
\alias{dispersion_scan}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate reflectivity as a function of both angle and wavelength
}
\description{
Calculate reflectivity as a function of both angle and wavelength
}
\usage{
dispersion_scan(angle_range = seq(0, pi/2, , 100), wavelength_range = seq(3.5e-07, 8.5e-07, , 100), polarisation = "p", incident_medium.index = 1 + (0+0i), exit_medium.index = 1 + (0+0i), layers, dispersive.function = "none", dispersive.layers = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{angle_range}{
The angle range in radians. The default angle range is from 0 to pi/2
}
  \item{wavelength_range}{
The wavelength range of the calculated spectra, in meters. The default covers the visible range from 350 nm to 850 nm.
}
  \item{polarisation}{
Linear polarisation of the light. Acceptable arguments are 'p' (Transverse Magnetic) or 's' (Transverse Electric).
}
  \item{incident_medium.index}{
The global incident medium. Default is n=1+0i (air)
}
  \item{exit_medium.index}{
The global exit medium. Default is n=1+0i (air)
}
  \item{layers}{
A list object containing the stack parameters. Must include index, thickness and repetitions. See details and examples for more information.
}
  \item{dispersive.function}{
For dispersive mateirals only, specify a function which returns the refractive index as a function of wavlength for the layer defined using the dispersive.layers varible.
}
  \item{dispersive.layers}{
Vector of layers to replace with dispersive.function. Starting with the top layer in the multilayer stack = 1.
}
}
\details{
The layers list should be constructed like so:

layers<-list(index=..., thickness=..., repetitions=...)

where index and thickness are vectors containing the stack parameters in order from the top interface to the bottom. Repetitions is an integer repeating the stack.

}
\value{
Returns a dataframe object with the following parts:
\item{wavlength}{The wavelength range in meters}
\item{angle}{The angle range in radians}
\item{Reflectivity}{The calculated reflectivity}
}

\references{
%% ~put references to the literature/web site here ~
}
\author{
Tom Constant
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

## The function is currently defined as
function (angle_range = seq(0, pi/2, , 100), wavelength_range = seq(3.5e-07, 
    8.5e-07, , 100), polarisation = "p", incident_medium.index = 1 + 
    (0+0i), exit_medium.index = 1 + (0+0i), layers, dispersive.function = "none", 
    dispersive.layers = NA) 
{
    mtx.exp <- Biodem::mtx.exp
    Reflection <- c()
    cum_angle <- c()
    cum_wavelength <- c()
    layers$index <- c(incident_medium.index, layers$index, exit_medium.index)
    layers$thickness <- c(0, layers$thickness, 0)
    pb <- txtProgressBar(min = 0, max = (length(wavelength_range) * 
        length(angle_range)), style = 3)
    pb.counter <- 0
    for (wavelength in wavelength_range) {
        for (angle in angle_range) {
            M <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2, byrow = TRUE)
            for (layer in 1:length(layers$index)) {
                if (dispersive.function != "none") {
                  disp.index <- match.fun(dispersive.function)
                  layers$index[dispersive.layers + 1] <- disp.index(wavelength)
                }
                L <- TMatrix(lambda0 = wavelength, polarisation = polarisation, 
                  n0 = incident_medium.index, n1 = layers$index[layer], 
                  n2 = exit_medium.index, d1 = layers$thickness[layer], 
                  theta0 = angle)
                if (layer == 1) 
                  gamma0 <- L$gamma0
                if (layer == length(layers$index)) 
                  gamma2 <- L$gamma2
                M <- M \%*\% L$TMatrix
            }
            M <- mtx.exp(M, layers$repetitions)
            r <- rFromTMatrix(M = M, gamma0 = gamma0, gamma2 = gamma2)
            Reflection <- c(Reflection, r * Conj(r))
            cum_angle <- c(cum_angle, angle)
            cum_wavelength <- c(cum_wavelength, wavelength)
            pb.counter <- pb.counter + 1
            setTxtProgressBar(pb = pb, value = pb.counter)
        }
    }
    return(data.frame(angle = cum_angle, wavelength = cum_wavelength, 
        Reflection = Re(Reflection)))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
