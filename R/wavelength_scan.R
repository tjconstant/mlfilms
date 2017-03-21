#' Spectra calculation at a fixed angle
#' 
#' @description Function to calculate the reflectivity as a function of wavelength for a given multilayer film.
#' 
#' @param wavelength_range The wavelength range of the calculated spectra, in meters. The default covers the visible range from 350 nm to 850 nm.
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

wavelength_scan <- function(wavelength_range = seq(350e-9, 850e-9, , 500),
                            angle = 0,
                            polarisation = "p",
                            incident_medium.index = 1 + 0i,
                            exit_medium.index = 1 + 0i,
                            layers,
                            dispersive.function = "none",
                            dispersive.layers = NA) {
  # change to radians
  check_for_radians(angle)
  angle <- angle * pi / 180
  
  # library(Biodem) #need Biodem for raising matrix to a power function (mtx.exp)
  mtx.exp <- Biodem::mtx.exp
  
  # initalize reflection/transmission varible
  Reflection <- numeric(length(wavelength_range))
  Transmission <- numeric(length(wavelength_range))
  r <- numeric(length(wavelength_range))
  t <- numeric(length(wavelength_range))
  
  # prevent numerical instablity by adding an extra entry and exit medium
  layers$index <-
    c(incident_medium.index, layers$index, exit_medium.index)
  layers$thickness <- c(0, layers$thickness, 0)
  
  counting_variable <- 0
  
  
  for (wavelength in wavelength_range) {
    counting_variable <- counting_variable + 1
    
    M <- matrix(c(1, 0, 0, 1),
                nrow = 2,
                ncol = 2,
                byrow = TRUE)
    
    for (layer in 1:length(layers$index)) {
      if (dispersive.function != "none") {
        disp.index <- match.fun(dispersive.function)
        layers$index[dispersive.layers + 1] <- disp.index(wavelength)
      }
      
      L <-
        TMatrix(
          lambda0 = wavelength,
          polarisation = polarisation,
          n0 = incident_medium.index,
          n1 = layers$index[layer],
          n2 = exit_medium.index,
          d1 = layers$thickness[layer],
          theta0 = angle
        )
      if (layer == 1)
        gamma0 <- L$gamma0
      if (layer == length(layers$index))
        gamma2 <- L$gamma2
      M <- M %*% L$TMatrix
      
    }
    
    #repeat unit cells
    M <- mtx.exp(M, layers$repetitions)
    
    r <- rFromTMatrix(M = M,
                      gamma0 = gamma0,
                      gamma2 = gamma2)
    Reflection[counting_variable] <- ReflectionCalc(r)
    
    t <- tFromTMatrix(M = M,
                      gamma0 = gamma0,
                      gamma2 = gamma2)
    Transmission[counting_variable] <-
      TransmissionCalc(t,
                       angle,
                       L$theta2,
                       incident_medium.index,
                       exit_medium.index,
                       polarisation)
    
    
  }
  
  return(
    data.frame(
      wavelength = wavelength_range,
      Reflection = Re(Reflection),
      Transmission = Re(Transmission),
      Absorption = 1 - Re(Transmission) - Re(Reflection),
      r = r,
      t = t
    )
  )
}


