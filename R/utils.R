#### UTILS ###
# External functions imported and sometimes re-exported

# data.table is generally careful to minimize the scope for namespace
# conflicts (i.e., functions with the same name as in other packages);
# a more conservative approach using @importFrom should be careful to
# import any needed data.table special symbols as well, e.g., if you
# run DT[ , .N, by='grp'] in your package, you'll need to add
# @importFrom data.table .N to prevent the NOTE from R CMD check.
# See ?data.table::`special-symbols` for the list of such symbols
# data.table defines; see the 'Importing data.table' vignette for more
# advice (vignette('datatable-importing', 'data.table')).
#
#' @importFrom data.table .N .SD `:=`
#' @keywords internal
NULL

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

#' Access cached function data
#'
#' @noRd
#' @export
.ffs_cache <- function(filename) {
  file.path("cache", filename) %>%
    system.file(package = "ffsimulator") %>%
    readRDS()
}

#' Check that dataframe has x column names
#'
#' @param dataframe dataframe to check
#' @param required_columns required column names
#'
#' @return silent if ok or else an error if something is missing.
#'
#' @keywords internal
assert_columns <- function(dataframe, required_columns) {
  d <- as.character(rlang::enexpr(dataframe))

  n <- names(dataframe)
  r <- required_columns %in% n
  if (all(r)) {
    return(NULL)
  }

  stop(
    glue::glue(
      "Assertion on `{d}` failed: requires columns ",
      "({paste(required_columns, collapse = ', ')})",
      " and is missing ({paste(required_columns[!r],collapse = ';')})"
    ),
    call. = FALSE
  )
}
