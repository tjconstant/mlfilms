#' Calculate reflectivity as a function of both angle and wavelength
#' 
#' @description Calculate reflectivity, transmission and absorption as a function of both angle and wavelength
#'
#' @inheritParams angle_scan 
#' @inheritParams wavelength_scan
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
                            dispersive.function = "none",
                            dispersive.layers = NA,
                            show.progress = TRUE) {
  # change to radians
  check_for_radians(angles)
  angles <- angles * pi / 180
  
  # library(Biodem) #need Biodem for raising matrix to a power function (mtx.exp)
  mtx.exp <- Biodem::mtx.exp
  
  # initalize reflection/transmission varible
  # Reflection<-c()
  Reflection <- numeric(length(angles) * length(wavelengths))
  Transmission <-
    numeric(length(angles) * length(wavelengths))
  
  cum_angle <- numeric(length(angles) * length(wavelengths))
  cum_wavelength <-
    numeric(length(angles) * length(wavelengths))
  counting_variable <- 0
  
  # prevent numerical instablity by adding an extra entry and exit medium
  layers$index <-
    c(incident_medium.index, layers$index, exit_medium.index)
  layers$thickness <- c(0, layers$thickness, 0)
  
  if(show.progress == TRUE){
   pb <-
     utils::txtProgressBar(min = 0,
                     max = (length(wavelengths) * length(angles)),
                    style = 3)
    pb.counter <- 0
  }
  
  for (wavelength in wavelengths) {
    for (angle in angles) {
      counting_variable <- counting_variable + 1
      
      M <- matrix(c(1, 0, 0, 1),
                  nrow = 2,
                  ncol = 2,
                  byrow = TRUE)
      
      for (layer in 1:length(layers$index)) {
        if (dispersive.function != "none") {
          disp.index <- match.fun(dispersive.function)
          layers$index[dispersive.layers + 1] <-
            disp.index(wavelength)
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

