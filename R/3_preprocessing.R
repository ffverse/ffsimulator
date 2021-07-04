
#' Preprocess data for simulation
#'
#' Performs joins, cleaning, filtering.
#'
#' @export
#' @rdname ffs_preprocess_data
ffs_preprocess_data <- function(conn, rosters, latest_rankings, adp_outcomes){
  UseMethod("ffs_preprocess_data")
}

#' @rdname ffs_preprocess_data
#' @export
ffs_preprocess_data.mfl_conn <- function(conn, rosters, latest_rankings, adp_outcomes){

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
      dplyr::any_of(c(
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
      )))

  return(joined_data)
}

#' @rdname ffs_preprocess_data
#' @export
ffs_preprocess_data.sleeper_conn <- function(conn, rosters, latest_rankings, adp_outcomes){

  joined_data <- rosters %>%
    dplyr::filter(.data$pos %in% c("QB","RB","WR","TE")) %>%
    dplyr::left_join(
      ffscrapr::dp_playerids() %>%
        dplyr::select("sleeper_id","fantasypros_id"),
      by = c("player_id"="sleeper_id"),
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
      dplyr::any_of(c(
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
      )))

}

#' @rdname ffs_preprocess_data
#' @export
ffs_preprocess_data.espn_conn <- function(conn, rosters, latest_rankings, adp_outcomes){

  joined_data <- rosters %>%
    dplyr::filter(.data$pos %in% c("QB","RB","WR","TE")) %>%
    dplyr::mutate(player_id = as.character(.data$player_id)) %>%
    dplyr::left_join(
      ffscrapr::dp_playerids() %>%
        dplyr::select("espn_id","fantasypros_id"),
      by = c("player_id"="espn_id"),
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
      dplyr::any_of(c(
        "franchise_id",
        "franchise_name",
        "player_id",
        "player_name",
        "pos",
        "team",
        "ecr",
        "rank",
        "prob_gp",
        "week_outcomes"
      )))


}

#' @rdname ffs_preprocess_data
#' @export
ffs_preprocess_data.flea_conn <- function(conn, rosters, latest_rankings, adp_outcomes){

  joined_data <- rosters %>%
    dplyr::filter(.data$pos %in% c("QB","RB","WR","TE")) %>%
    dplyr::left_join(
      ffscrapr::dp_playerids() %>%
        dplyr::select("sportradar_id","fantasypros_id"),
      by = c("sportradar_id"),
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
      dplyr::any_of(c(
        "franchise_id",
        "franchise_name",
        "player_id",
        "player_name",
        "pos",
        "team",
        "ecr",
        "rank",
        "prob_gp",
        "week_outcomes"
      )))

}
