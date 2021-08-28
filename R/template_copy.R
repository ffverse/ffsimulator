#' Copy simulation template to filename
#'
#' Creates a simulation template file with all of the components of ff_simulate, ready for tinkering!
#'
#' @param filename New file name, defaults to putting "ff_simulation.R" into your current directory
#' @param template choice of template: one of "season", "rest_of_season", "week"
#' @param overwrite a logical (or NULL) - overwrite if existing file found?
#' @examples \donttest{
#'
#' tmp <- tempfile()
#' ffs_copy_template(tmp)
#' }
#' @export
#' @return a success message signalling success/failure.

ffs_copy_template <- function(
  filename = "ff_simulation.R",
  template = c("season","rest_of_season","week"),
  overwrite = NULL) {
  template <- rlang::arg_match(template)
  checkmate::assert_flag(overwrite, null.ok = TRUE)

  template_file <- switch(
    template,
    "season" = "simulation_template_season.R",
    stop("No template found")
  )

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

  from <- system.file("simulation_template_season.R", package = "ffsimulator")

  file.copy(from, filename, overwrite = TRUE)

  cli::cli_alert_success("Successfully copied template to {filename}!")
}
