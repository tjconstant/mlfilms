#' Create dispersive material template file
#'
#'
#' @examples
create_material <-function(){
  
  skeleton_location <- system.file("skeleton.R", package = "mlfilms")
  
  file.copy(from = skeleton_location,
            to = paste0(getwd(),"/my_material.R"),overwrite = FALSE)
  
  utils::file.edit("my_material.R")
  
}

# create_material()
# file.remove("my_material.R")
