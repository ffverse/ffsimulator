#' Check that dataframe has x column names
#'
#' @param dataframe dataframe to check
#' @param required_columns required column names
#' @param .env caller environment to complain from
#'
#' @return silent if ok or else an error if something is missing.
#'
#' @keywords internal
assert_columns <- function(dataframe, required_columns, .env = rlang::caller_env()) {

  if(!inherits(dataframe, "data.frame")){
    cli::cli_abort(
      "{.arg {rlang::caller_arg(dataframe)}} is not a data.frame",
      call = .env
    )
  }

  n <- names(dataframe)
  r <- required_columns %in% n

  if (all(r)) return(invisible(TRUE))

  missing <- required_columns[!r]
  d <- rlang::caller_arg(dataframe)

  cli::cli_abort(
    c(
      "!" = "Assertion on {.arg {d}} failed",
      "i" = "Requires columns ({required_columns}) and is missing ({missing})"
    ),
    call = .env
  )
}

assert_character <- function(x, .env = rlang::caller_env()){
  if(is.character(x)) return(invisible(TRUE))
  cli::cli_abort(
    "Assertion on {.arg {rlang::caller_arg(x)} failed: must be a character vector",
    call = .env
  )
}
