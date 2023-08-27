#' Connects ff_scoringhistory to past ADP rankings
#'
#' The backbone of the ffsimulator resampling process is coming up with a population of weekly outcomes for every inseason weekly rank. This function creates that dataframe by connecting historical FantasyPros.com rankings to nflfastR-based scoring data, as created by `ffscrapr::ff_scoringhistory()`.
#'
#' @param scoring_history a scoring history table as created by `ffscrapr::ff_scoringhistory()`
# @param gp_model either "simple" or "none" - simple uses the average games played per season for each position/adp combination, none assumes every game is played.
#' @param pos_filter a character vector: filter the positions returned to these specific positions, default: c("QB","RB","WR","TE)
#'
#' @return a dataframe with position, rank, probability of games played, and a corresponding nested list per row of all week score outcomes.
#'
#' @examples
#' \donttest{
#' # cached data
#' scoring_history <- .ffs_cache_example("mfl_scoring_history.rds")
#' ffs_adp_outcomes_week(scoring_history, pos_filter = c("QB","RB","WR","TE"))
#' }
#'
#' @seealso `fp_rankings_history_week` for the included historical rankings
#'
#' @export
ffs_adp_outcomes_week <- function(scoring_history,
                                  pos_filter = c("QB", "RB", "WR", "TE")) {
  # ASSERTIONS #
  assert_character(pos_filter)
  assert_df(scoring_history, c("gsis_id", "week", "season", "points"))

  gsis_id <- NULL
  fantasypros_id <- NULL
  pos <- NULL
  rank <- NULL
  points <- NULL
  week <- NULL
  week_outcomes <- NULL
  player_name <- NULL
  fantasypros_id <- NULL
  len <- NULL
  season <- NULL
  games_played <- NULL

  sh <- data.table::as.data.table(scoring_history)[!is.na(gsis_id) & week <= 16,c("gsis_id","week", "season", "points")]
  fp_rh <- data.table::as.data.table(fp_rankings_history_week())[,-"page_pos"]
  dp_id <- data.table::as.data.table(ffscrapr::dp_playerids())[!is.na(gsis_id) & !is.na(fantasypros_id),c("fantasypros_id","gsis_id")]

  ao <- fp_rh[
    dp_id
    , on = "fantasypros_id"
    , nomatch = 0
  ][
    !is.na(gsis_id) & pos %in% pos_filter
  ][
    sh
    , on = c("season","week","gsis_id")
    , nomatch = 0
  ][
    , list(week_outcomes = list(points), games_played = .N)
    , by = c("season","pos","rank","fantasypros_id","player_name")
  ][
    , list(
      season = rep(season, each = 5),
      pos = rep(pos, each = 5),
      fantasypros_id = rep(fantasypros_id, each = 5),
      player_name = rep(player_name,each =  5),
      games_played = rep(games_played, each = 5),
      week_outcomes = rep(week_outcomes, each = 5),
      rank = unlist(lapply(rank, .ff_rank_expand))
    )
  ][
    , list(week_outcomes = list(c(unlist(week_outcomes))),
           player_name = list(player_name),
           fantasypros_id = list(fantasypros_id)
    ),
    by = c("pos","rank")
  ][
    , len := sapply(week_outcomes,length)
  ][
    , len := max(len)-len
  ][
    ,`:=`(week_outcomes = mapply(.ff_rep_na, week_outcomes, len, SIMPLIFY = FALSE), len = NULL)
  ][
    order(pos,rank)
  ]

  return(ao)
}

.ff_rep_na <- function(week_outcomes,len){
  c(unlist(week_outcomes), rep(NA, times = len))
}
