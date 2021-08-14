#' Copy simulation template to path
#'
#' @param path

ffs_copy_template <- function(path){

  run <- TRUE

  if(file.exists(path)) {
    run <- FALSE
    run <- askYesNo(paste0("Overwrite file already found at ",path,"?"),
                    default = FALSE)
    }

  if(!run) return(NULL)

  if(file.exists(path)) stop("File already found at ",path,)

  from <- system.file("custom_simulation_template.R", package = "ffsimulator")
  file.copy(from, path, overwrite = TRUE)
}
