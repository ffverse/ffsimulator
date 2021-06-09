#' Parse Raw RDS
#'
#' Useful for parsing the raw-content of RDS files downloaded from nflfastr repo
#'
#' @param raw raw-content that is known to be an RDS file
#'
#' @seealso `httr::set_cookies`
#'
#' @keywords internal

parse_raw_rds <- function(raw) {
  con <- gzcon(rawConnection(raw))

  on.exit(close(con))

  readRDS(con) %>%
    tibble::tibble()
}
