#' Fixed wavelengh angle-scan of a thin film stack
#' 
#' @description Function to calculate the reflectivity, transmission and absorption as a function of angle for a given multilayer film.
#'
#' @param layers A list object containing the stack parameters. Must include index, thickness and repetitions. See details and examples for more information.
#' @param angles The angle range in degrees. The default angle range is from 0 to 90.
#' @param wavelength The wavelength in meters. The default is for a HeNe laser (633 nm)
#' @param polarisation Linear polarisation of the light. Acceptable arguments are 'p' (Transverse Magnetic) or 's' (Transverse Electric).
#' @param incident_medium.index The global incident medium. Default is n=1+0i (air)
#' @param exit_medium.index The global exit medium. Default is n=1+0i (air)
#'
#' @details 
#' The layers list should be constructed like so:
#' 
#' \code{layers <- list(index = ..., thickness = ..., repetitions = ...)}
#'
#' where index and thickness are vectors containing the stack parameters in order from the top interface to the bottom. Repetitions is an integer repeating the stack.
#' @references Introduction to Optics 3rd Edition, Pearson international edition by Frank L. Pedrotti, Leno Matthew Pedrotti, Leno S. Pedrotti
#'
#' @return Returns a dataframe with the angle, Refelctivity, Transmission and Absorbtion
#' @export
#'
#' @examples
#' layers<-list(index=c(0.13+4i),thickness=c(45e-9),repetitions= 1)
#'
#' R_plasmon<-angle_scan(incident_medium.index = 1.5+0i,exit_medium.index = 1+0i,layers = layers)
#'
#' plot(R_plasmon$angle,R_plasmon$Reflection,type='l',lwd=2, ylim=c(0,1))
#' title("Surface Plasmon in air")

angle_scan <- function(layers,
                       angles = seq(0, 90, length.out =  500),
                       wavelength = 633e-9,
                       polarisation = "p",
                       incident_medium.index = complex(real = 1, imaginary = 0),
                       exit_medium.index = complex(real = 1, imaginary = 0),
                       show.progress = F
                       ) {
  
  result <- dispersion_scan(layers = layers,
                            angles = angles,
                            wavelengths = wavelength,
                            polarisation = polarisation,
                            incident_medium.index = incident_medium.index,
                            exit_medium.index =exit_medium.index,
                            show.progress = show.progress)                  
  
  return(result) 
  
}


