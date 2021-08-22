#### Get Rosters ####

#' Get Rosters
#'
#' This function lightly wraps `ffscrapr::ff_rosters()` and adds fantasypros_id, which is a required column for ffsimulator.
#'
#' @param conn a connection object as created by `ffscrapr::ff_connect()` and friends.
#'
#' @return a dataframe of rosters that includes a fantasypros_id column
#'
#' @examples
#' \donttest{
#' # cached examples
#' conn <- .ffs_cache("mfl_conn.rds")
#'
#' ffs_rosters(conn)
#' }
#'
#' @seealso vignette("Custom Simulations") for more detailed example usage
#'
#' @export
ffs_rosters <- function(conn) {
  UseMethod("ffs_rosters")
}

#' @rdname ffs_rosters
#' @export
ffs_rosters.mfl_conn <- function(conn) {
  rosters <- ffscrapr::ff_rosters(conn) %>%
    dplyr::left_join(
      ffscrapr::dp_playerids() %>%
        dplyr::select("mfl_id", "fantasypros_id"),
      by = c("player_id" = "mfl_id"),
      na_matches = "never"
    ) %>%
    dplyr::mutate(league_id = as.character(conn$league_id),
                  franchise_id = as.character(.data$franchise_id))

  return(rosters)
}

#' @rdname ffs_rosters
#' @export
ffs_rosters.sleeper_conn <- function(conn) {
  rosters <- ffscrapr::ff_rosters(conn) %>%
    dplyr::left_join(
      ffscrapr::dp_playerids() %>%
        dplyr::select("sleeper_id", "fantasypros_id"),
      by = c("player_id" = "sleeper_id"),
      na_matches = "never"
    ) %>%
    dplyr::mutate(league_id = as.character(conn$league_id),
                  franchise_id = as.character(.data$franchise_id))

  return(rosters)
}

#' @rdname ffs_rosters
#' @export
ffs_rosters.flea_conn <- function(conn) {
  rosters <- ffscrapr::ff_rosters(conn) %>%
    dplyr::left_join(
      ffscrapr::dp_playerids() %>%
        dplyr::select("sportradar_id", "fantasypros_id"),
      by = c("sportradar_id"),
      na_matches = "never"
    ) %>%
    dplyr::mutate(league_id = as.character(conn$league_id),
                  franchise_id = as.character(.data$franchise_id))

  return(rosters)
}

#' @rdname ffs_rosters
#' @export
ffs_rosters.espn_conn <- function(conn) {
  rosters <- ffscrapr::ff_rosters(conn) %>%
    dplyr::mutate(player_id = as.character(.data$player_id)) %>%
    dplyr::left_join(
      ffscrapr::dp_playerids() %>%
        dplyr::select("espn_id", "fantasypros_id"),
      by = c("player_id" = "espn_id"),
      na_matches = "never"
    ) %>%
    dplyr::mutate(league_id = as.character(conn$league_id),
                  franchise_id = as.character(.data$franchise_id))

  return(rosters)
}


#' @noRd
#' @export
ffs_rosters.default <- function(conn) {
  # nocov start
  stop(glue::glue("Could not find a method of <ff_rosters> for class {class(conn)} - was this created by ff_connect()?"),
    call. = FALSE
  )
  # nocov end
}

#' Get Franchises
#'
#' This function lightly wraps `ffscrapr::ff_franchises()` and adds league_id, which is a required column for ffsimulator.
#'
#' @param conn a connection object as created by `ffscrapr::ff_connect()` and friends.
#'
#' @return a dataframe of franchises that includes the league_id column
#'
#' @examples
#' \donttest{
#' # cached examples
#' conn <- .ffs_cache("mfl_conn.rds")
#'
#' ffs_franchises(conn)
#' }
#'
#' @seealso vignette("Custom Simulations") for more detailed example usage
#'
#' @export
ffs_franchises <- function(conn){
  ffscrapr::ff_franchises(conn) %>%
    dplyr::mutate(league_id = as.character(conn$league_id),
                  franchise_id = as.character(.data$franchise_id)
                  )
}

