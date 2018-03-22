#' Calculate optical response as a function of both angle and wavelength
#' 
#' @description Calculate reflectivity, transmission and absorption as a function of both angle and wavelength
#'
#' @param layers A list object containing the stack parameters. Must include index, thickness and repetitions. See details and examples for more information.
#' @param angles The angle range in degrees. The default angle range is from 0 to 90.
#' @param wavelengths The wavelength range of the calculated spectra, in meters. The default covers the visible range from 350 nm to 850 nm.
#' @param polarisation Linear polarisation of the light. Acceptable arguments are 'p' (Transverse Magnetic) or 's' (Transverse Electric).
#' @param incident_medium.index The global incident medium. Default is n=1+0i (air)
#' @param exit_medium.index The global exit medium. Default is n=1+0i (air)
#' @param show.progress Determine is a progress bar is to be printed to console
#'
#' @inherit angle_scan references details
#' @return
#' Returns a dataframe object with the following parts:
#' \item{wavlength}{The wavelength range in meters}
#' \item{angle}{The angle range in radians}
#' \item{Reflection}{The calculated reflectivity}
#' \item{Transmission}{The calculated transmission}
#' \item{Absorption}{The calculated absorption}
#' @export
#'
#' @examples
#' layers <- list(index = c(2.35+0i,1.38+0i),
#' thickness = c(550e-9/(4*2.35),550e-9/(4*1.38)),
#' repetitions = 6)
#' 
#' R_highlowStack6 <- dispersion_scan(angles = seq(0,89,,100),
#'                                    incident_medium.index=1+0i,
#'                                    exit_medium.index = 1.52+0i,
#'                                    layers = layers,
#'                                    show.progress = FALSE)
#' 
#' x <- unique(R_highlowStack6$angle)
#' y <- unique(R_highlowStack6$wavelength)
#' image(x = x,
#'       y = y*1e9,
#'       z = matrix(R_highlowStack6$Reflection,nrow=100), 
#'       xlab = expression(angle~(degree)),
#'       ylab = "wavelength (nm)")

dispersion_scan <- function(layers,
                            angles = seq(0, 90, length.out =  100),
                            wavelengths = seq(350e-9, 850e-9, length.out =  100),
                            polarisation = "p",
                            incident_medium.index = complex(real = 1, imaginary = 0),
                            exit_medium.index = complex(real = 1, imaginary = 0),
                            show.progress = TRUE) {
  # change to radians
  check_for_radians(angles)
  angles <- angles * pi / 180
  
  # check layer object
  layers <- parse_layers(layers)
  
  # library(Biodem) #need Biodem for raising matrix to a power function (mtx.exp)
  mtx.exp <- Biodem::mtx.exp
  
  # initalize reflection/transmission varible
  Reflection <- numeric(length(angles) * length(wavelengths))
  Transmission <- numeric(length(angles) * length(wavelengths))
  
  cum_angle <- numeric(length(angles) * length(wavelengths))
  cum_wavelength <- numeric(length(angles) * length(wavelengths))
  counting_variable <- 0
  
  # prevent numerical instablity by adding an extra entry and exit medium
  layers$index <- c(incident_medium.index, layers$index, exit_medium.index)
  layers$thickness <- c(0, layers$thickness, 0)
  
  if(show.progress == TRUE){
   pb <-
     utils::txtProgressBar(min = 0,
                     max = (length(wavelengths) * length(angles)),
                    style = 3)
    pb.counter <- 0
  }
  
  ## Dispersive Function Bit Before Loop
  # Note the dispersive layers
  
  dispersive.layers <- which(lapply(layers$index, typeof) == "closure")
  unparsed_layers <- layers$index
    
  for (wavelength in wavelengths) {
    
    # Dispersive Function bit After Loop
    # Replace dispersive functions with refractive index for this wavelength
    
    for(i in dispersive.layers){
      layers$index[[i]] <- unparsed_layers[[i]](wavelength)
    }
    layers$index <- unlist(layers$index)
    
    for (angle in angles) {
      
      counting_variable <- counting_variable + 1
      
      M <- matrix(c(1, 0, 0, 1),
                  nrow = 2,
                  ncol = 2,
                  byrow = TRUE)

      for (layer in 1:length(layers$index)) {

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
      Reflection[counting_variable] <- r * Conj(r)
      
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
      
      cum_angle[counting_variable] <- angle
      cum_wavelength[counting_variable] <- wavelength
      
      #pb.counter<-pb.counter+1
      if(show.progress == TRUE) utils::setTxtProgressBar(pb = pb, value = counting_variable)
      
    }
  }
  return(
    data.frame(
      angle = cum_angle * 180 / pi,
      wavelength = cum_wavelength,
      Reflection = Re(Reflection),
      Transmission = Re(Transmission),
      Absorption = 1 - Re(Transmission) - Re(Reflection)
    )
  )
}

