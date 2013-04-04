\name{wavelength_scan}
\alias{wavelength_scan}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
spectra calculation at a fixed angle
}
\description{
Function to calculate the reflectivity as a function of wavelength for a given multilayer film.
}
\usage{
wavelength_scan(wavelength_range = seq(3.5e-07, 8.5e-07, , 500), angle = 0, polarisation = "p", incident_medium.index = 1 + (0+0i), exit_medium.index = 1 + (0+0i), layers, dispersive.function = "none", dispersive.layers = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wavelength_range}{
The wavelength range of the calculated spectra, in meters. The default covers the visible range from 350 nm to 850 nm.
}
  \item{angle}{
Fixed angle in radians. Default is 0.
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
Returns a dataframe with the wavelength and Refelctivity
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
layers<-list(index=c(2.35+0i,1.38+0i),thickness=c(550e-9/(4*2.35),550e-9/(4*1.38)),repetitions=6)

R_highlowStack6<-wavelength_scan(incident_medium.index=1+0i,exit_medium.index=1.52+0i,layers=layers)

plot(R_highlowStack6,type='l',lwd=2, ylim=c(0,1))
title("H/L index stack (N=6 & 2): Pedrotti Figure 22-9")

}