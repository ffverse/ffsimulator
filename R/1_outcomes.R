#' Connects ff_scoringhistory to past rankings
#'
#' @return dataframe with position, rank, probability of games played, all week score outcomes
ffs_adp_outcomes <- function(scoring_history, injury_model){

  adp_outcomes <- ffsimulator::fp_rankings_history %>%
    dplyr::select(-"page_pos") %>%
    dplyr::left_join(
      ffscrapr::dp_playerids() %>%
        dplyr::select("fantasypros_id","gsis_id"),
      by = "fantasypros_id"
    ) %>%
    dplyr::filter(!is.na(.data$gsis_id), pos %in% c("QB","RB","WR","TE")) %>%
    dplyr::left_join(
      scoring_history %>%
        dplyr::filter(!is.na(.data$gsis_id), .data$week <= 17) %>%
        dplyr::select("season","gsis_id", "team", "points")
      , by = c("season","gsis_id")
    ) %>%
    dplyr::filter(!is.na(.data$points)) %>%
    dplyr::group_by(.data$season,
                    .data$pos,
                    .data$rank,
                    .data$fantasypros_id,
                    .data$player_name
    ) %>%
    dplyr::summarise(
      week_outcomes = list(points),
      games_played = dplyr::n()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$season, .data$pos) %>%
    dplyr::mutate(rank = purrr::map(.data$rank, ~c(ifelse(.x-1==0,.x,.x-1),.x,.x+1) %>% tidyr::replace_na(.x))) %>%
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
.ff_apply_injury_model <- function(adp_outcomes, model_type){

  if(model_type == "none") {adp_outcomes$prob_gp <- 1}

  if(model_type == "simple") {
    adp_outcomes <- adp_outcomes %>%
      dplyr::left_join(ffsimulator::fp_injury_table, by = c("pos","rank"))
  }

  adp_outcomes
}

