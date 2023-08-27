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
                             pos_filter = c("QB", "RB", "WR", "TE")) {
  # ASSERTIONS #
  checkmate::assert_choice(gp_model, choices = c("simple", "none"))
  checkmate::assert_character(pos_filter)
  checkmate::assert_data_frame(scoring_history)
  assert_df(scoring_history, c("gsis_id", "team", "season", "points"))

  gsis_id <- NULL
  fantasypros_id <- NULL
  pos <- NULL
  rank <- NULL
  points <- NULL
  week <- NULL
  week_outcomes <- NULL
  player_name <- NULL
  season <- NULL
  games_played <- NULL

  sh <- data.table::as.data.table(scoring_history)[
    !is.na(gsis_id) & week <= 17
    , c("gsis_id", "team", "season", "points")
  ]
  fp_rh <- data.table::as.data.table(fp_rankings_history())[, -"page_pos"]
  dp_id <- data.table::as.data.table(ffscrapr::dp_playerids())[
    !is.na(gsis_id) & !is.na(fantasypros_id)
    , c("fantasypros_id", "gsis_id")
  ]

  ao <- fp_rh[
    dp_id
    , on = "fantasypros_id"
    , nomatch = 0
  ][
    !is.na(gsis_id) & pos %in% pos_filter
  ][
    sh
    , on = c("season", "gsis_id")
    , nomatch = 0
  ][
    , list(week_outcomes = list(points), games_played = .N)
    , by = c("season", "pos", "rank", "fantasypros_id", "player_name")
  ][
    , list(
      season = rep(season, each = 5),
      pos = rep(pos, each = 5),
      fantasypros_id = rep(fantasypros_id, each = 5),
      player_name = rep(player_name, each = 5),
      games_played = rep(games_played, each = 5),
      week_outcomes = rep(week_outcomes, each = 5),
      rank = unlist(lapply(rank, .ff_rank_expand))
    )
  ]

  ao <- .ff_apply_gp_model(ao, gp_model)

  ao <- ao[
    , list(
      week_outcomes = list(c(unlist(week_outcomes))),
      player_name = list(player_name),
      fantasypros_id = list(fantasypros_id)
    )
    , by = c("pos", "rank", "prob_gp")
  ][
    order(pos, rank)
  ][
    !is.na(fantasypros_id)
  ]

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
.ff_rank_expand <- function(.x) {
  .x <- seq.int(from = .x - 2, to = .x + 2, by = 1)
  ifelse(.x <= 0, 1, .x)
}
