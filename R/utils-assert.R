#' Check that df has x column names
#'
#' @param df df to check
#' @param required_columns required column names
#' @param .env caller environment to complain from
#'
#' @return silent if ok or else an error if something is missing.
#'
#' @keywords internal
assert_df <- function(df, required_columns, .env = rlang::caller_env()) {

  if (!inherits(df, "data.frame")) {
    cli::cli_abort(
      "{.arg {rlang::caller_arg(df)}} must be coercible to a data.frame",
      call = .env
    )
  }

  if (length(required_columns) == 0) return(invisible(TRUE))

  n <- names(df)
  r <- required_columns %in% n

  if (all(r)) return(invisible(TRUE))

  missing <- required_columns[!r]
  d <- rlang::caller_arg(df)

  cli::cli_abort(
    c(
      "!" = "Assertion on {.arg {d}} failed",
      "i" = "Requires columns ({.val {required_columns}})",
      "i" = "and is missing: ({.val {missing}})"
    ),
    call = .env
  )
}

assert_character <- function(x, .env = rlang::caller_env()) {
  if (is.character(x)) return(invisible(TRUE))
  cli::cli_abort(
    "Assertion on {.arg {rlang::caller_arg(x)} failed: must be a character vector",
    call = .env
  )
}
