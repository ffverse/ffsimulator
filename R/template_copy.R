#' Copy simulation template to filename
#'
#' Creates a simulation template file with all of the components of ff_simulate, ready for tinkering!
#'
#' @param filename New file name, defaults to putting "ff_simulation.R" into your current directory
#' @param overwrite a logical (or NULL) - overwrite if existing file found?
#' @examples \donttest{
#'
#' tmp <- tempfile()
#' ffs_copy_template(tmp)
#' }
#' @export
#' @return a success message signalling success/failure.

ffs_copy_template <- function(filename = "ff_simulation.R", overwrite = NULL) {
  checkmate::assert_flag(overwrite, null.ok = TRUE)

  run <- TRUE

  if (file.exists(filename) && !isTRUE(overwrite)) run <- FALSE


  if (!run && is.null(overwrite) && interactive()) {
    run <- utils::menu(
      choices = c("Yes", "No"),
      title = glue::glue("Overwrite file already found at {filename}?")
    )

    run <- run == 1
  }

  if (!run) {
    return(cli::cli_alert_info("Did not copy template to {filename} - found existing file!"))
  }

  from <- system.file("custom_simulation_template.R", package = "ffsimulator")

  file.copy(from, filename, overwrite = TRUE)

  cli::cli_alert_success("Successfully copied template to {filename}!")
}
