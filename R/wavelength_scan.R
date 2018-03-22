#' Spectra calculation at a fixed angle
#' 
#' @description Function to calculate the reflectivity, transmission and absorption as a function of wavelength for a given multilayer film.
#' 
#' @param wavelengths The wavelength range of the calculated spectra, in meters. The default covers the visible range from 350 nm to 850 nm.
#' @param angle Fixed angle in degrees. Default is 0.
#' @inheritParams angle_scan 
#' @param dispersive.function For dispersive materials only, specify a function which returns the refractive index as a function of wavlength for the layer defined using the dispersive.layers varible.
#' @param dispersive.layers Vector of layers to replace with dispersive.function. Starting with the top layer in the multilayer stack = 1.
#'
#' @return Returns a dataframe with the wavelength, Refelctivity, Transmission and Absorbtion
#' 
#' @inherit angle_scan references details
#' 
#' @export
#'
#' @examples
#' 
#' layers<-list(index=c(2.35+0i,1.38+0i),thickness=c(550e-9/(4*2.35),550e-9/(4*1.38)),repetitions=6)
#'
#' R_highlowStack6<-wavelength_scan(incident_medium.index=1+0i,exit_medium.index=1.52+0i,layers=layers)
#'
#' plot(R_highlowStack6$wavelength,R_highlowStack6$Reflection,type='l',lwd=2, ylim=c(0,1))
#' title("H/L index stack (N=6 & 2): Pedrotti Figure 22-9")

wavelength_scan <- function(layers,
                            wavelengths = seq(350e-9, 850e-9, length.out = 500),
                            angle = 0,
                            polarisation = "p",
                            incident_medium.index = complex(real = 1, imaginary = 0),
                            exit_medium.index = complex(real = 1, imaginary = 0),
                            dispersive.function = "none",
                            dispersive.layers = NA,
                            show.progress = F) {
  
  result <- dispersion_scan(layers = layers,
                            angles = angle,
                            wavelengths = wavelengths,
                            polarisation = polarisation,
                            incident_medium.index = incident_medium.index,
                            exit_medium.index = exit_medium.index,
                            dispersive.function = dispersive.function,
                            dispersive.layers = dispersive.layers,
                            show.progress = show.progress)                  
  
  return(result) 
  
}


