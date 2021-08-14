#' Copy simulation template to path
#'
#' Creates a simulation template file with all of the components of ff_simulate, ready for tinkering!
#'
#' @param filename New file name, defaults to putting "ff_simulation.R" into your current directory
#' @param overwrite a logical (or NULL) - overwrite if existing file found?
#' @example \donttest{
#'
#'   tmp <- tempfile()
#'   ffs_copy_template(tmp)
#'
#' }
#'
#' @return a success message signalling success/failure.

ffs_copy_template <- function(path = "ff_simulation.R", overwrite = NULL){

  checkmate::assert_flag(overwrite, null.ok = TRUE)

  run <- TRUE

  if(file.exists(path) && !isTRUE(overwrite)) run <- FALSE


  if(!run && is.null(overwrite) && interactive()){
    run <- utils::menu(
      choices = c("Yes","No"),
      title = glue::glue("Overwrite file already found at {path}?"))

    run <- run == 1
  }

  if(!run) return(cli::cli_alert_info("Did not copy template to {path} - found existing file!"))

  from <- system.file("custom_simulation_template.R", package = "ffsimulator")

  file.copy(from, path, overwrite = TRUE)

  cli::cli_alert_success("Successfully copied template to {path}!")
}
