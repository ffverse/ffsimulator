
#' Join all the data together for preprocessing
#'
#' @keywords internal
#' @export
.ff_join_data <- function(conn, rosters, latest_rankings, adp_outcomes){
  UseMethod(".ff_join_data")
}

#' MFL joins
#' @export
.ff_join_data.mfl_conn <- function(conn, rosters, latest_rankings, adp_outcomes){

  joined_data <- rosters %>%
    dplyr::filter(.data$pos %in% c("QB","RB","WR","TE")) %>%
    dplyr::left_join(
      ffscrapr::dp_playerids() %>%
        dplyr::select("mfl_id","fantasypros_id"),
      by = c("player_id"="mfl_id"),
      na_matches = "never"
    ) %>%
    dplyr::left_join(
      latest_rankings %>%
        dplyr::select("fantasypros_id", "ecr"),
      by = c("fantasypros_id"),
      na_matches = "never"
    ) %>%
    dplyr::group_by(.data$pos) %>%
    dplyr::mutate(rank = round(.data$ecr)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(
      adp_outcomes %>%
        dplyr::select("pos", "rank", "prob_gp", "week_outcomes"),
      by = c("pos","rank"),
      na_matches = "never"
    ) %>%
    dplyr::mutate(
      pos = factor(.data$pos, levels = c("QB","RB","WR","TE"))
    ) %>%
    dplyr::select(
      "franchise_id",
      "franchise_name",
      "player_id",
      "player_name",
      "pos",
      "team",
      "age",
      "ecr",
      "rank",
      "prob_gp",
      "week_outcomes"
    )

  return(joined_data)
}

#' sleeper joins
#' @export
.ff_join_data.sleeper_conn <- function(conn, rosters, latest_rankings, adp_outcomes){}

#' espn joins
#' @export
.ff_join_data.espn_conn <- function(conn, rosters, latest_rankings, adp_outcomes){}

#' fleaflicker joins
#' @export
.ff_join_data.flea_conn <- function(conn, rosters, latest_rankings, adp_outcomes){}
