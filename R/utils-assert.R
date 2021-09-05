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

  rlang::abort(
    glue::glue(
      "Assertion on `{d}` failed: requires columns ",
      "({paste(required_columns, collapse = ', ')})",
      " and is missing ({paste(required_columns[!r],collapse = ';')})"
    )
  )
}

