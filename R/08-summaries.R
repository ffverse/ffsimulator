#' Summarise simulation outputs
#'
#' These functions are used to summarise the simulation outputs, typically by joining the optimal scores with a matching schedule.
#'
#' @param optimal_scores a dataframe of optimized lineups as created by `ffs_optimize_lineups()`
#' @param schedules a dataframe of schedules as created by `ffs_build_schedules()` or `ffs_actual_schedules()`
#'
#' @seealso `vignette("custom")` for example usage
#'
#' @examples \donttest{
#' # cached examples
#' optimal_scores <- .ffs_cache_example("optimal_scores.rds")
#' schedules <- .ffs_cache_example("schedules.rds")
#'
#' summary_week <- ffs_summarise_week(optimal_scores, schedules)
#' summary_week
#' summary_season <- ffs_summarise_season(summary_week)
#' summary_season
#' summary_simulation <- ffs_summarise_simulation(summary_season)
#' summary_simulation
#' }
#'
#' @rdname ffs_summaries
#' @return ffs_summarise_week: a dataframe summarising team results by simulation week
#' @export
ffs_summarise_week <- function(optimal_scores, schedules) {
  checkmate::assert_data_frame(optimal_scores)
  assert_columns(
    optimal_scores,
    c("season", "week", "season", "week", "actual_score", "league_id", "franchise_id")
  )
  data.table::setDT(optimal_scores)

  checkmate::assert_data_frame(schedules)
  assert_columns(
    schedules,
    c("league_id","franchise_id","opponent_id","season", "week")
  )

  data.table::setDT(schedules)

  actual_score <- NULL
  allplay_wins <- NULL
  allplay_games <- NULL

  team <- data.table::copy(optimal_scores)

  team[
    ,
    `:=`(allplay_wins = data.table::frank(actual_score)-1,
         allplay_games = .N -1),
    by = c("season","week")
  ][,`:=`(allplay_pct = round(allplay_wins / allplay_games, 3))]

  opponent <- data.table::copy(team)
  data.table::setnames(team,old = "actual_score",new = "team_score")
  data.table::setnames(opponent,
                       old = c("actual_score","franchise_id","franchise_name"),
                       new = c("opponent_score","opponent_id","opponent_name"))

  opponent <- opponent[,c("opponent_score","opponent_id","opponent_name","league_id","season","week")]

  team_score <- NULL
  opponent_score <- NULL
  optimal_score <- NULL
  lineup_efficiency <- NULL

  summary_week <- schedules[
    team, on = c("league_id","franchise_id", "season", "week"), nomatch = 0
  ][opponent, on = c("league_id","opponent_id", "season", "week"), nomatch = 0
  ][,`:=`(result = data.table::fcase(team_score > opponent_score, "W",
                                     team_score < opponent_score, "L",
                                     team_score == opponent_score, "T",
                                     TRUE, NA_character_),
          team_score = round(team_score,2),
          optimal_score = round(optimal_score,2),
          opponent_score = round(opponent_score,2),
          lineup_efficiency = round(lineup_efficiency,3))
  ][,c("season",
       "week",
       "franchise_name",
       "optimal_score",
       "lineup_efficiency",
       "team_score",
       "opponent_score",
       "result",
       "opponent_name",
       "allplay_wins",
       "allplay_games",
       "allplay_pct",
       "league_id",
       "franchise_id",
       "optimal_player_id",
       "optimal_player_score")]

  data.table::setorderv(summary_week,c("season","week","franchise_id"))

  return(summary_week)
}

#' Summarise Season
#'
#' @param summary_week a dataframe as created by `ffs_summarise_week()`
#' @rdname ffs_summaries
#' @return ffs_summarise_season: a dataframe summarising franchise results across each simulation season
#' @export
ffs_summarise_season <- function(summary_week) {
  checkmate::assert_data_frame(summary_week)
  assert_columns(
    summary_week,
    c(
      "season", "league_id", "franchise_id", "franchise_name",
      "result", "allplay_wins", "allplay_games",
      "team_score", "opponent_score", "optimal_score"
    )
  )
  data.table::setDT(summary_week)

  result <- NULL
  allplay_wins <- NULL
  allplay_games <- NULL
  team_score <- NULL
  opponent_score <- NULL
  optimal_score <- NULL

  summary_season <- summary_week[
    ,
    .(h2h_wins = sum(result == "W", na.rm = TRUE),
      h2h_winpct = round(sum(result == "W", na.rm = TRUE) / .N,3),
      allplay_wins = sum(allplay_wins, na.rm = TRUE),
      allplay_games = sum(allplay_games, na.rm = TRUE),
      allplay_winpct = round(sum(allplay_wins, na.rm = TRUE) / sum(allplay_games, na.rm = TRUE),3),
      points_for = sum(team_score, na.rm = TRUE),
      points_against = sum(opponent_score, na.rm = TRUE),
      potential_points = sum(optimal_score, na.rm = TRUE)
    ),
    by = c("season","league_id","franchise_id","franchise_name")
  ]

  return(summary_season)
}

#' Summarise Simulation
#'
#' @param summary_season a dataframe as created by `ffs_summarise_season()`
#' @rdname ffs_summaries
#' @return ffs_summarise_simulation: a dataframe summarising franchise results across the simulation
#' @export
ffs_summarise_simulation <- function(summary_season) {
  checkmate::assert_data_frame(summary_season)
  assert_columns(
    summary_season,
    c(
      "league_id", "franchise_id", "franchise_name",
      "h2h_wins", "h2h_winpct", "allplay_wins", "allplay_winpct",
      "points_for", "points_against", "potential_points"
    )
  )
  data.table::setDT(summary_season)

  allplay_winpct <- NULL

  summary_simulation <- summary_season[
    ,c(list(seasons = .N),
       lapply(.SD, ffs_mean)),
    by = c("league_id","franchise_id","franchise_name"),
    .SDcols = c("h2h_wins", "h2h_winpct", "allplay_wins", "allplay_winpct", "points_for", "points_against", "potential_points")
  ][order(-allplay_winpct)]

  return(summary_simulation)
}

ffs_mean <- function(...){round(mean(...,na.rm = TRUE),3)}

#' Summarise Inseason Simulation
#'
#' @param summary_week a dataframe as created by `ffs_summarise_week()`
#' @param n number of weeks
#' @rdname ffs_summaries
#' @return ffs_summarise_inseason: a dataframe summarising franchise results for the inseason simulation
#' @export
ffs_summarise_inseason <- function(summary_week,n) {
  checkmate::assert_number(n)
  checkmate::assert_data_frame(summary_week)
  assert_columns(
    summary_week,
    c(
      "league_id", "franchise_id", "franchise_name",
      "result", "allplay_wins", "allplay_games",
      "team_score", "opponent_score", "optimal_score"
    )
  )
  data.table::setDT(summary_week)

  result <- NULL
  allplay_wins <- NULL
  allplay_games <- NULL
  team_score <- NULL
  opponent_score <- NULL
  optimal_score <- NULL

  summary_season <- summary_week[
    ,
    .(h2h_wins = sum(result == "W", na.rm = TRUE),
      h2h_winpct = round(sum(result == "W", na.rm = TRUE) / .N,3),
      allplay_wins = sum(allplay_wins, na.rm = TRUE),
      allplay_games = sum(allplay_games, na.rm = TRUE),
      allplay_winpct = round(sum(allplay_wins, na.rm = TRUE) / sum(allplay_games, na.rm = TRUE),3),
      points_for = round(sum(team_score, na.rm = TRUE)/n,2),
      points_against = round(sum(opponent_score, na.rm = TRUE)/n,2),
      potential_points = round(sum(optimal_score, na.rm = TRUE)/n,2)
    ),
    by = c("league_id","franchise_id","franchise_name")
  ]

  return(summary_season)
}


#' @rdname ffs_summaries
#' @export
ffs_summarize_week <- ffs_summarise_week
#' @rdname ffs_summaries
#' @export
ffs_summarize_season <- ffs_summarise_season
#' @rdname ffs_summaries
#' @export
ffs_summarize_simulation <- ffs_summarise_simulation
