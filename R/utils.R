#### UTILS ###
# External functions imported and sometimes re-exported

#' @keywords internal
#' @importFrom rlang .data `%||%` .env
#' @importFrom utils str

NULL

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL

#' Parse Raw RDS
#'
#' Useful for parsing the raw-content of RDS files downloaded from various github repos
#'
#' @param raw raw-content that is known to be an RDS file
#'
#' @keywords internal
parse_raw_rds <- function(raw) {
  con <- gzcon(rawConnection(raw))

  on.exit(close(con))

  readRDS(con) %>%
    tibble::tibble()
}
