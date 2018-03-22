
# Check to see if repetitions are specified, if not assume 1.
check_repetitions <- function(layers){
  
  if(!("repetitions" %in% names(layers))){
    layers$repetitions = 1
    warning("Number of repetitions not specified, defaulting to 1.")
  }
  
  return(layers)
}

# Check each layer has a refractive index and a thickness
check_lengths <- function(layers){
  
  n_index <- length(layers$index)
  n_thickness <- length(layers$thickness)
  
  if(!(n_index == n_thickness)){
    stop(paste0(
      "Number of thicknesses specified (",n_thickness,") does not equal the number of refractive indexes (",n_index,")."))
  }
}

# parse_layers checks and modifies the layers variable
parse_layers <- function(layers){
  
  check_lengths(layers)
  layers <- check_repetitions(layers)
  return(layers)
  
}