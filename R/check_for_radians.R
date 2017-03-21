check_for_radians <- function(angle){
  if (round(max(angle),5) == round(pi/2,5)){
    warning("Looks like you're using radians \n Since v0.2.4, mlfilms uses degrees as it's default angle unit")
  }
}