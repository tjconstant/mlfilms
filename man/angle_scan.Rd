\name{angle_scan}
\alias{angle_scan}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
fixed wavelengh angle-scan of a thin film stack
}
\description{
Function to calculate the reflectivity as a function of angle for a given multilayer film.
}
\usage{
angle_scan(angle_range = seq(0, pi/2, , 500), wavelength = 6.33e-07, polarisation = "p", incident_medium.index = 1 + (0+0i), exit_medium.index = 1 + (0+0i), layers)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{angle_range}{
The angle range in radians. The default angle range is from 0 to pi/2
}
  \item{wavelength}{
The wavelength in meters. The default is for a HeNe laser (633 nm)
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
}
\details{
The layers list should be constructed like so:

layers<-list(index=..., thickness=..., repetitions=...)

where index and thickness are vectors containing the stack parameters in order from the top interface to the bottom. Repetitions is an integer repeating the stack.

}
\value{
Returns a dataframe with the angle and Refelctivity
}
\author{
Tom Constant
}
\examples{
layers<-list(index=c(0.13+4i),thickness=c(45e-9),repetitions= 1)

R_plasmon<-angle_scan(incident_medium.index=1.5+0i,exit_medium.index=1+0i,layers=layers)

plot(R_plasmon,type='l',lwd=2, ylim=c(0,1))
title("Surface Plasmon in air")
}
