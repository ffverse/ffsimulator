#' Connects ff_scoringhistory to past ADP rankings
#'
#' The backbone of the ffsimulator resampling process is coming up with a population of weekly outcomes for every preseason positional rank. This function creates that dataframe by connecting historical FantasyPros.com rankings to nflverse-based scoring data, as created by `ffscrapr::ff_scoringhistory()`.
#'
#'
#' @param scoring_history a scoring history table as created by `ffscrapr::ff_scoringhistory()`
#' @param gp_model either "simple" or "none" - simple uses the average games played per season for each position/adp combination, none assumes every game is played.
#' @param pos_filter a character vector: filter the positions returned to these specific positions, default: c("QB","RB","WR","TE)
#'
#' @return a dataframe with position, rank, probability of games played, and a corresponding nested list per row of all week score outcomes.
#'
#' @examples
#' \donttest{
#' # cached data
#' scoring_history <- .ffs_cache_example("mfl_scoring_history.rds")
#'
#' ffs_adp_outcomes(scoring_history, gp_model = "simple")
#' ffs_adp_outcomes(scoring_history, gp_model = "none")
#' }
#'
#' @seealso `fp_rankings_history` for the included historical rankings
#' @seealso `fp_injury_table` for the historical injury table
#' @seealso `vignette("custom")` for usage details.
#'
#' @export
ffs_adp_outcomes <- function(scoring_history,
                             gp_model = "simple",
                             pos_filter = c("QB", "RB", "WR", "TE"),
                             version = c("v2", "v1")) {
  # ASSERTIONS #
  version <- rlang::arg_match(version)
  checkmate::assert_choice(gp_model, choices = c("simple", "none"))
  checkmate::assert_character(pos_filter)
  checkmate::assert_data_frame(scoring_history)
  assert_df(scoring_history, c("gsis_id", "team", "season", "points"))

  ao <- switch(
    version,
    "v2" = ffs_adp_outcomes_week(scoring_history, pos_filter = pos_filter),
    "v1" = .ffs_adp_outcomes_v1(scoring_history = scoring_history, pos_filter = pos_filter)
  )

  ao <- .ff_apply_gp_model(ao, model_type = gp_model)

  return(ao)
}

#' Applies various injury models to adp outcomes
#'
#' @keywords internal
#' @return same adp outcomes dataframe but with a prob_gp column
.ff_apply_gp_model <- function(adp_outcomes, model_type) {
  if (model_type == "none") {
    adp_outcomes$prob_gp <- 1
  }

  if (model_type == "simple") {
    adp_outcomes <- adp_outcomes[data.table::as.data.table(fp_injury_table()), on = c("pos", "rank")]
  }

  adp_outcomes
}

#' Expand one rank into a vector of five ranks to broaden population of possible outcomes
#' @keywords internal
.ff_rank_expand <- function(.x, size = 2) {
  .x <- seq.int(from = .x - size, to = .x + size, by = 1)
  ifelse(.x <= 0, 1, .x)
}
