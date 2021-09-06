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
#' @seealso vignette("custom") for more detailed example usage
#'
#' @export
ffs_rosters <- function(conn) {
  UseMethod("ffs_rosters")
}

#' @rdname ffs_rosters
#' @export
ffs_rosters.mfl_conn <- function(conn) {
  r <- ffscrapr::ff_rosters(conn)

  r$player_id <- as.character(r$player_id)

  r <- merge(r,
             ffscrapr::dp_playerids()[,c("mfl_id","fantasypros_id")],
             by.x = "player_id",
             by.y = "mfl_id",
             all.x = TRUE)
  r$league_id <- as.character(conn$league_id)
  r$franchise_id <- as.character(r$franchise_id)

  return(r)
}

#' @rdname ffs_rosters
#' @export
ffs_rosters.sleeper_conn <- function(conn) {
  r <- ffscrapr::ff_rosters(conn)

  r$player_id <- as.character(r$player_id)

  r <- merge(r,
             ffscrapr::dp_playerids()[,c("sleeper_id","fantasypros_id")],
             by.x = "player_id",
             by.y = "sleeper_id",
             all.x = TRUE)
  r$league_id <- as.character(conn$league_id)
  r$franchise_id <- as.character(r$franchise_id)

  return(r)
}

#' @rdname ffs_rosters
#' @export
ffs_rosters.flea_conn <- function(conn) {
  r <- ffscrapr::ff_rosters(conn)

  r$player_id <- as.character(r$player_id)

  r <- merge(r,
             ffscrapr::dp_playerids()[,c("sportradar_id","fantasypros_id")],
             by.x = "sportradar_id",
             by.y = "sportradar_id",
             all.x = TRUE)
  r$league_id <- as.character(conn$league_id)
  r$franchise_id <- as.character(r$franchise_id)

  return(r)
}

#' @rdname ffs_rosters
#' @export
ffs_rosters.espn_conn <- function(conn) {
  r <- ffscrapr::ff_rosters(conn)

  r$player_id <- as.character(r$player_id)

  r <- merge(r,
             ffscrapr::dp_playerids()[,c("espn_id","fantasypros_id")],
             by.x = "player_id",
             by.y = "espn_id",
             all.x = TRUE)
  r$league_id <- as.character(conn$league_id)
  r$franchise_id <- as.character(r$franchise_id)

  return(r)
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
  f <- ffscrapr::ff_franchises(conn)
  f$league_id <- as.character(conn$league_id)
  f$franchise_id <- as.character(f$franchise_id)

  return(f)
}

