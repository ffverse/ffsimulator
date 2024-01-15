#' FP injury table
#'
#' This dataframe contains a column (`prob_gp`) for each positional ranking that
#' describes the probability of a player with that preseason ADP playing in a
#' given game. It is modelled from historical rankings data and the number of
#' games played per season for a given positional rank.
#'
#' By default, it returns bundled package data unless the `ffsimulator.cache_directory`
#' option is configured, in which case it will look for a rds file named
#' `fp_injury_table.rds` in that directory.
#'
#' To update, use `ffs_copy_template("rankings_update")` to find the bundled update script
#'
#' @return dataframe of injury probability by position rank
#' @export
fp_injury_table <- function() {
  .fp_injury_table <- .ffs_read_data("fp_injury_table.rds")

  assert_df(.fp_injury_table, c("pos", "rank", "prob_gp"))
  return(.fp_injury_table)
}

#' Historical draft position ranks
#'
#' This dataframe has historical positional draft rankings starting in 2012 for
#' QB/RB/WR/TE/PK and 2015 DL/LB/DB, as gathered by the ffpros package.
#'
#' By default, it returns bundled package data unless the `ffsimulator.cache_directory`
#' option is configured, in which case it will look for a rds file named
#' `fp_rankings_history.rds` in that directory.
#'
#' To update, use `ffs_copy_template("rankings_update")` to find the bundled update script
#'
#' @return dataframe of preseason/draft rankings from ffpros
#' @export
#'
fp_rankings_history <- function() {
  .fp_rankings_history <- .ffs_read_data("fp_rankings_history.rds")
  assert_df(
    .fp_rankings_history,
    c("season", "fantasypros_id", "sportradar_id",
      "player_name", "pos", "team", "rank", "ecr", "sd")
  )
  return(.fp_rankings_history)
}

#' Historical position ranks
#'
#' This dataframe has historical positional in-season rankings starting in 2012 for
#' QB/RB/WR/TE/PK and 2015 for DL/LB/DB, as gathered by the ffpros package.
#'
#' By default, it returns bundled package data unless the `ffsimulator.cache_directory`
#' option is configured, in which case it will look for a rds file named
#' `fp_rankings_history_week.rds` in that directory.
#'
#' To update, use `ffs_copy_template("rankings_update")` to find the bundled update script
#'
#' @return dataframe of in-season positional rankings
#' @export
fp_rankings_history_week <- function() {
  .fp_rankings_history_week <- .ffs_read_data("fp_rankings_history_week.rds")
  assert_df(
    .fp_rankings_history_week,
    c("season", "week", "fantasypros_id", "sportradar_id",
      "player_name", "pos", "team", "rank", "ecr", "sd")
  )
  return(.fp_rankings_history_week)
}

#' Read data from cache or pkg data
#'
#' @param file_name filename to find
#' @param .env for internal use in error messages
#'
#' @rdname data-cache
#' @export
.ffs_read_data <- function(file_name, .env = rlang::caller_env()) {
  cache_file <- .ffs_cache_dir(file_name)

  if (!checkmate::test_file_exists(cache_file)) {
    out <- readRDS(.ffs_cache_pkgdir(file_name))
    return(out)
  }

  cli::cli_inform(
    "Using {.file {file_name}} found in cache directory {.path { .ffs_cache_dir()}}",
    .frequency = "regularly",
    .frequency_id = paste0("ffsimulator_cache_", file_name)
  )

  rlang::try_fetch(
    out <- readRDS(cache_file),
    error = function(e) {
      cli::cli_abort("Could not parse file {cache_file} as RDS!", call = .env)
    }
  )

  return(out)
}

#' @export
#' @param ... passed to `file.path()`
#' @rdname data-cache
.ffs_cache_dir <- function(...) {
  dir <- getOption(
    "ffsimulator.cache_directory",
    default = rappdirs::user_cache_dir("ffsimulator", "ffverse")
  )
  file.path(dir, ...)
}

.ffs_cache_pkgdir <- function(...) {
  system.file(
    "pkgdata",
    ...,
    package = "ffsimulator",
    mustWork = TRUE
  )
}

#' Copy template for updating rankings
#'
#' Creates a template file for updating package rankings. This involves some
#' devel packages eg ffpros, so is only provided as a script rather than as a
#' package function.
#'
#' @param filename New file name, defaults to putting "ff_simulation.R" into your current directory
#' @param overwrite a logical (or NULL) - overwrite if existing file found?
#' @examples \donttest{
#' tmp <- tempfile()
#' ffs_update_rankings_template(tmp)
#' }
#' @export
#' @return a success message signalling success/failure.

ffs_update_data_template <- function(filename = "ffsim_update_data.R",
                                     overwrite = NULL) {
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

  from <- system.file("update_rankings_data.R", package = "ffsimulator")

  file.copy(from, filename, overwrite = TRUE)

  cli::cli_alert_success("Successfully copied template to {filename}!")
}

