#' Calculate reflectivity as a function of both angle and wavelength
#' 
#' @description Calculate reflectivity as a function of both angle and wavelength
#'
#' @inheritParams angle_scan 
#' @inheritParams wavelength_scan
#'
#' @inherit angle_scan references details
#' @return
#' Returns a dataframe object with the following parts:
#' \item{wavlength}{The wavelength range in meters}
#' \item{angle}{The angle range in radians}
#' \item{Reflectivity}{The calculated reflectivity}
#' @export
#'
#' @examples
#' layers <- list(index = c(2.35+0i,1.38+0i),
#' thickness = c(550e-9/(4*2.35),550e-9/(4*1.38)),
#' repetitions = 6)
#' 
#' R_highlowStack6 <- dispersion_scan(angle_range = seq(0,89,,100),
#'                                    incident_medium.index=1+0i,
#'                                    exit_medium.index = 1.52+0i,
#'                                    layers = layers)
#' 
#' x <- unique(R_highlowStack6$angle)
#' y <- unique(R_highlowStack6$wavelength)
#' image(x = x,
#'       y = y*1e9,
#'       z = matrix(R_highlowStack6$Reflection,nrow=100), 
#'       xlab = expression(angle~(degree)),
#'       ylab = "wavelength (nm)")

dispersion_scan <- function(angle_range = seq(0, 90, , 100),
                            wavelength_range = seq(350e-9, 850e-9, , 100),
                            polarisation = "p",
                            incident_medium.index = 1 + 0i,
                            exit_medium.index = 1 + 0i,
                            layers,
                            dispersive.function = "none",
                            dispersive.layers = NA) {
  # change to radians
  check_for_radians(angle_range)
  angle_range <- angle_range * pi / 180
  
  # library(Biodem) #need Biodem for raising matrix to a power function (mtx.exp)
  mtx.exp <- Biodem::mtx.exp
  
  # initalize reflection/transmission varible
  # Reflection<-c()
  Reflection <- numeric(length(angle_range) * length(wavelength_range))
  Transmission <-
    numeric(length(angle_range) * length(wavelength_range))
  
  cum_angle <- numeric(length(angle_range) * length(wavelength_range))
  cum_wavelength <-
    numeric(length(angle_range) * length(wavelength_range))
  counting_variable <- 0
  
  # prevent numerical instablity by adding an extra entry and exit medium
  layers$index <-
    c(incident_medium.index, layers$index, exit_medium.index)
  layers$thickness <- c(0, layers$thickness, 0)
  
  pb <-
    utils::txtProgressBar(min = 0,
                   max = (length(wavelength_range) * length(angle_range)),
                   style = 3)
  pb.counter <- 0
  
  for (wavelength in wavelength_range) {
    for (angle in angle_range) {
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
      if (polarisation == "s") {
        Transmission[counting_variable] <-
          t * Conj(t) * (exit_medium.index * cos(L$theta2) / (incident_medium.index *
                                                                cos(angle)))
      } else if (polarisation == "p") {
        Transmission[counting_variable] <-
          t * Conj(t) * (exit_medium.index * Conj(cos(L$theta2)) / (incident_medium.index *
                                                                      Conj(cos(angle))))
      }
      
      cum_angle[counting_variable] <- angle
      cum_wavelength[counting_variable] <- wavelength
      
      #pb.counter<-pb.counter+1
      utils::setTxtProgressBar(pb = pb, value = counting_variable)
      
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

