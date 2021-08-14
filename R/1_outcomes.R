#' Connects ff_scoringhistory to past ADP rankings
#'
#' The backbone of the ffsimulator resampling process is coming up with a population of weekly outcomes for every preseason positional rank. This function creates that dataframe by connecting historical FantasyPros.com rankings to nflfastR-based scoring data, as created by `ffscrapr::ff_scoringhistory()`.
#'
#'
#' @param scoring_history a scoring history table as created by `ffscrapr::ff_scoringhistory()`
#' @param injury_model either "simple" or "none" - simple uses the average games played per season for each position/adp combination, none assumes every game is played.
#' @param pos_filter a character vector: filter the positions returned to these specific positions, default: c("QB","RB","WR","TE)
#'
#' @return a tibble with position, rank, probability of games played, and a corresponding nested list per row of all week score outcomes.
#'
#' @examples
#' \donttest{
#'  #cached data
#'  scoring_history <- .ffs_cache("mfl_scoring_history.rds")
#'
#'  ffs_adp_outcomes(scoring_history, injury_model = "simple")
#'  ffs_adp_outcomes(scoring_history, injury_model = "none")
#' }
#'
#' @seealso `fp_rankings_history` for the included historical rankings
#' @seealso `fp_injury_table` for the historical injury table
#' @seealso `vignette("Custom Simulation")` for usage details.
#'
#' @export
ffs_adp_outcomes <- function(scoring_history,
                             injury_model = "simple",
                             pos_filter = c("QB","RB","WR","TE")) {

  checkmate::assert_choice(injury_model, choices = c("simple", "none"))
  checkmate::assert_character(pos_filter)
  checkmate::assert_data_frame(scoring_history)
  checkmate::assert_subset(c("gsis_id", "team", "season", "points"), names(scoring_history))

  adp_outcomes <- ffsimulator::fp_rankings_history %>%
    dplyr::select(-"page_pos") %>%
    dplyr::left_join(
      ffscrapr::dp_playerids() %>%
        dplyr::select("fantasypros_id", "gsis_id"),
      by = "fantasypros_id"
    ) %>%
    dplyr::filter(!is.na(.data$gsis_id), .data$pos %in% pos_filter) %>%
    dplyr::left_join(
      scoring_history %>%
        dplyr::filter(!is.na(.data$gsis_id), .data$week <= 17) %>%
        dplyr::select("season", "gsis_id", "team", "points"),
      by = c("season", "gsis_id")
    ) %>%
    dplyr::filter(!is.na(.data$points)) %>%
    dplyr::group_by(
      .data$season,
      .data$pos,
      .data$rank,
      .data$fantasypros_id,
      .data$player_name
    ) %>%
    dplyr::summarise(
      week_outcomes = list(.data$points),
      games_played = dplyr::n()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$season, .data$pos) %>%
    dplyr::mutate(rank = purrr::map(.data$rank, ~ c(ifelse(.x - 1 == 0, .x, .x - 1), .x, .x + 1) %>% tidyr::replace_na(.x))) %>%
    dplyr::ungroup() %>%
    tidyr::unnest(rank) %>%
    .ff_apply_injury_model(injury_model) %>%
    dplyr::group_by(.data$pos, .data$rank, .data$prob_gp) %>%
    dplyr::summarise(
      week_outcomes = list(c(unlist(.data$week_outcomes))),
      player_name = list(.data$player_name),
      fantasypros_id = list(.data$fantasypros_id)
    ) %>%
    dplyr::ungroup()

  return(adp_outcomes)
}

#' Applies various injury models to adp outcomes
#'
#' @keywords internal
#' @return same adp outcomes dataframe but with a prob_gp column
.ff_apply_injury_model <- function(adp_outcomes, model_type) {
  if (model_type == "none") {
    adp_outcomes$prob_gp <- 1
  }

  if (model_type == "simple") {
    adp_outcomes <- adp_outcomes %>%
      dplyr::left_join(ffsimulator::fp_injury_table, by = c("pos", "rank"))
  }

  adp_outcomes
}
