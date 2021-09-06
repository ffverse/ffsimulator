#' Connects ff_scoringhistory to past ADP rankings
#'
#' The backbone of the ffsimulator resampling process is coming up with a population of weekly outcomes for every inseason weekly rank. This function creates that dataframe by connecting historical FantasyPros.com rankings to nflfastR-based scoring data, as created by `ffscrapr::ff_scoringhistory()`.
#'
#' @param scoring_history a scoring history table as created by `ffscrapr::ff_scoringhistory()`
# @param gp_model either "simple" or "none" - simple uses the average games played per season for each position/adp combination, none assumes every game is played.
#' @param pos_filter a character vector: filter the positions returned to these specific positions, default: c("QB","RB","WR","TE)
#'
#' @return a tibble with position, rank, probability of games played, and a corresponding nested list per row of all week score outcomes.
#'
#' @examples
#' \donttest{
#' # cached data
#' scoring_history <- .ffs_cache("mfl_scoring_history.rds")
#' ffs_adp_outcomes_week(scoring_history, pos_filter = c("QB","RB","WR","TE"))
#' }
#'
#' @seealso `fp_rankings_history_week` for the included historical rankings
#'
#' @export
ffs_adp_outcomes_week <- function(scoring_history,
                                  # gp_model = "simple",
                                  pos_filter = c("QB", "RB", "WR", "TE")) {
  # ASSERTIONS #
  # checkmate::assert_choice(gp_model, choices = c("simple", "none"))
  checkmate::assert_character(pos_filter)
  checkmate::assert_data_frame(scoring_history)
  assert_columns(scoring_history, c("gsis_id", "team", "season", "points"))

  adp_outcomes <- ffsimulator::fp_rankings_history_week %>%
    dplyr::select(-"page_pos") %>%
    dplyr::left_join(
      ffscrapr::dp_playerids() %>%
        dplyr::select("fantasypros_id", "gsis_id"),
      by = "fantasypros_id"
    ) %>%
    dplyr::filter(!is.na(.data$gsis_id), .data$pos %in% pos_filter) %>%
    dplyr::left_join(
      scoring_history %>%
        dplyr::filter(!is.na(.data$gsis_id)) %>%
        dplyr::select("season", "week", "gsis_id", "team", "points"),
      by = c("season", "week", "gsis_id")
    ) %>%
    dplyr::mutate(
      rank = purrr::map(.data$rank, ~ c(ifelse(.x - 1 == 0, .x, .x - 1), .x, .x + 1))
    ) %>%
    tidyr::unnest(rank) %>%
    dplyr::group_by(.data$pos, .data$rank) %>%
    dplyr::summarise(
      week_outcomes = list(.data$points),
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
.ff_apply_gp_model <- function(adp_outcomes, model_type) {
  if (model_type == "none") {
    adp_outcomes$prob_gp <- 1
  }

  if (model_type == "simple") {
    adp_outcomes <- adp_outcomes %>%
      dplyr::left_join(ffsimulator::fp_injury_table, by = c("pos", "rank"))
  }

  adp_outcomes
}
