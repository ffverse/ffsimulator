#### UTILS ###

#' Access cached function data
#'
#' @keywords internal
#' @export
.ffs_cache_example <- function(filename) {
    readRDS(
      system.file(
        file.path("examples", filename),
        package = "ffsimulator"
      )
    )
}

set_verbose <- function(verbose = NULL) {

  if(isTRUE(verbose)) options(ffsimulator.verbose = TRUE)
  if(!isTRUE(verbose)) options(ffsimulator.verbose = FALSE)

  return(NULL)
}

vcli_start <- function(...){
  v <- getOption("ffsimulator.verbose", default = TRUE)

  if(!v) return(NULL)

  cli::cli_process_start(...)
}

vcli_end <- function(...){
  v <- getOption("ffsimulator.verbose", default = TRUE)

  if(!v) return(NULL)

  cli::cli_process_done(...)
}

vcli_rule <- function(...){
  v <- getOption("ffsimulator.verbose", default = TRUE)

  if(!v) return(NULL)

  cli::cli_rule(...)
}
