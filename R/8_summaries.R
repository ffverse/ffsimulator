#' Summarise simulation outputs
#'
#' These functions are used to summarise the simulation outputs, typically by joining the optimal scores with a matching schedule.
#'
#' @param optimal_scores a dataframe of optimized lineups as created by `ffs_optimize_lineups()`
#' @param schedules a dataframe of schedules as created by `ffs_build_schedules()`
#'
#' @seealso `vignette("Custom Simulations")` for example usage
#'
#' @examples \donttest{
#' # cached examples
#' optimal_scores <- .ffs_cache("optimal_scores.rds")
#' schedules <- .ffs_cache("schedules.rds")
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
  checkmate::assert_data_frame(schedules)
  assert_columns(
    schedules,
    c("season", "week", "team", "opponent")
  )

  scores <- optimal_scores %>%
    dplyr::group_by(.data$season, .data$week) %>%
    dplyr::mutate(
      allplay_wins = rank(.data$actual_score) - 1,
      allplay_games = dplyr::n() - 1,
      allplay_pct = round(.data$allplay_wins / .data$allplay_games, 3),
      schedule_id = rank(paste0(.data$league_id, .data$franchise_id))
    ) %>%
    dplyr::ungroup()

  matchups <- schedules %>%
    dplyr::left_join(scores %>%
      dplyr::rename("team_score" = "actual_score"),
    by = c("team" = "schedule_id", "season", "week")
    ) %>%
    dplyr::left_join(scores %>%
      dplyr::select(
        "opponent_score" = "actual_score",
        "schedule_id",
        "opponent_name" = "franchise_name",
        "season", "week"
      ),
    by = c("opponent" = "schedule_id", "season", "week")
    ) %>%
    dplyr::mutate(
      result = dplyr::case_when(
        .data$team_score > .data$opponent_score ~ "W",
        .data$team_score < .data$opponent_score ~ "L",
        .data$team_score == .data$opponent_score ~ "T",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::mutate_if(is.numeric, round, 3) %>%
    dplyr::select(
      "season",
      "season_week" = "week",
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
      "optimal_player_score"
    )

  return(matchups)
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

  summary_season <- summary_week %>%
    dplyr::group_by(.data$season, .data$league_id, .data$franchise_id, .data$franchise_name) %>%
    dplyr::summarise(
      h2h_wins = sum(.data$result == "W", na.rm = TRUE),
      h2h_winpct = round(.data$h2h_wins / dplyr::n(), 3),
      allplay_wins = sum(.data$allplay_wins, na.rm = TRUE),
      allplay_games = sum(.data$allplay_games, na.rm = TRUE),
      allplay_winpct = round(.data$allplay_wins / .data$allplay_games, 3),
      points_for = sum(.data$team_score, na.rm = TRUE),
      points_against = sum(.data$opponent_score, na.rm = TRUE),
      potential_points = sum(.data$optimal_score, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()

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

  summary_simulation <- summary_season %>%
    dplyr::group_by(.data$league_id, .data$franchise_id, .data$franchise_name) %>%
    dplyr::summarise(
      seasons = dplyr::n(),
      dplyr::across(c("h2h_wins", "h2h_winpct", "allplay_wins", "allplay_winpct", "points_for", "points_against", "potential_points"), ~ mean(.x, na.rm = TRUE) %>% round(3))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(-.data$allplay_winpct)

  return(summary_simulation)
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
