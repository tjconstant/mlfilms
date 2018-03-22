# Template File for Dispersive Material Function

material_function <- function(wavelength){
  # Wavelength will be passed by mlfilms in meters
  
  
  # Place dispersive functions / file reads / interpolation code here
  dummy_index <- 1e9 * wavelength/100 + 1i*1e9 * wavelength/300 # Dummy Code for example (Not a real material!)
  
  refractive_index <- dummy_index
  
  # Error catching can go here
  if(!is.complex(refractive_index)) warning("Dispersive material returned non-complex value")
  
  # The value returned should be the refractive index of type 'complex'
  return(refractive_index)
  
}
