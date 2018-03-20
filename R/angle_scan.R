#' Fixed wavelengh angle-scan of a thin film stack
#' 
#' @description Function to calculate the reflectivity as a function of angle for a given multilayer film.
#'
#' @param layers A list object containing the stack parameters. Must include index, thickness and repetitions. See details and examples for more information.
#' @param angle_range The angle range in degrees. The default angle range is from 0 to 90.
#' @param wavelength The wavelength in meters. The default is for a HeNe laser (633 nm)
#' @param polarisation Linear polarisation of the light. Acceptable arguments are 'p' (Transverse Magnetic) or 's' (Transverse Electric).
#' @param incident_medium.index The global incident medium. Default is n=1+0i (air)
#' @param exit_medium.index The global exit medium. Default is n=1+0i (air)
#'
#' @details 
#' The layers list should be constructed like so:
#' 
#' layers<-list(index=..., thickness=..., repetitions=...)
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
                       angle_range = seq(0, 90, length.out =  500),
                       wavelength = 633e-9,
                       polarisation = "p",
                       incident_medium.index = complex(real = 1, imaginary = 0),
                       exit_medium.index = complex(real = 1, imaginary = 0)
                       ) {
  # change to radians
  check_for_radians(angle_range)
  angle_range <- angle_range * pi / 180
  
  # library(Biodem) #need Biodem for raising matrix to a power function (mtx.exp)
  mtx.exp <- Biodem::mtx.exp
  
  # if(is.vector(layers)) layers<-list(index=layers[1],thickness=layers[2],repetitions=layers[3] )
  
  # initalize reflection/transmisson varible
  Reflection <- numeric(length(angle_range))
  Transmission <- numeric(length(angle_range))
  r <- numeric(length(angle_range))
  t <- numeric(length(angle_range))
  
  # prevent numerical instablity by adding an extra entry and exit medium
  layers$index <-
    c(incident_medium.index, layers$index, exit_medium.index)
  layers$thickness <- c(0, layers$thickness, 0)
  
  counting_variable <- 0
  
  
  for (angle in angle_range) {
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
    
    # repeat unit cells
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
      angle = angle_range * 180 / pi,
      Reflection = Re(Reflection),
      Transmission = Re(Transmission),
      Absorption = 1 - Re(Transmission) - Re(Reflection)
    )
  )
}


