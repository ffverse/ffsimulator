#### ffscrapr imports ####

#' Connect to a league
#'
#' See `ffscrapr::ff_connect()` for details.
#'
#' @name ff_connect
#' @family ffscrapr-imports
#' @importFrom ffscrapr ff_connect
#' @return a connection object to be used with `ff_*` functions
#' @export ff_connect
NULL

#' Connect to a league
#'
#' See `ffscrapr::mfl_connect()` for details.
#'
#' @name mfl_connect
#' @family ffscrapr-imports
#' @importFrom ffscrapr mfl_connect
#' @return a connection object to be used with `ff_*` functions
#' @export mfl_connect
NULL
#' Connect to a league
#'
#' See `ffscrapr::sleeper_connect()` for details.
#'
#' @name sleeper_connect
#' @family ffscrapr-imports
#' @importFrom ffscrapr sleeper_connect
#' @return a connection object to be used with `ff_*` functions
#' @export sleeper_connect
NULL
#' Connect to a league
#'
#' See `ffscrapr::fleaflicker_connect()` for details.
#'
#' @name fleaflicker_connect
#' @family ffscrapr-imports
#' @importFrom ffscrapr fleaflicker_connect
#' @return a connection object to be used with `ff_*` functions
#' @export fleaflicker_connect
NULL
#' Connect to a league
#'
#' See `ffscrapr::espn_connect()` for details.
#'
#' @name espn_connect
#' @family ffscrapr-imports
#' @importFrom ffscrapr espn_connect
#' @return a connection object to be used with `ff_*` functions
#' @export espn_connect
NULL

#' Get league scoring history
#'
#' See `ffscrapr::ff_scoringhistory` for details.
#'
#' @name ff_scoringhistory
#' @rdname ff_scoringhistory
#' @family ffscrapr-imports
#' @importFrom ffscrapr ff_scoringhistory
#' @return A tidy dataframe of weekly fantasy scoring data, one row per player per week
#' @export ff_scoringhistory
NULL

#' Get league starter positions
#'
#' See `ffscrapr::ff_starter_positions` for details.
#'
#' @name ff_starter_positions
#' @importFrom ffscrapr ff_starter_positions
#' @family ffscrapr-imports
#' @return A tidy dataframe of positional lineup rules, one row per position with minimum and maximum starters as well as total starter calculations.
#' @export ff_starter_positions
NULL

#' Get league starter positions
#'
#' This function lightly wraps `ffscrapr::ff_starter_positions()` and cleans up some abbreviations (PK -> K)
#'
#' @param conn a connection object as created by `ffscrapr::ff_connect()` and friends.
#'
#' @return A tidy dataframe of positional lineup rules, one row per position with minimum and maximum starters as well as total starter calculations.
#'
#' @examples
#' \donttest{
#' # cached examples
#' conn <- .ffs_cache("mfl_conn.rds")
#'
#' ffs_starter_positions(conn)
#' }
#'
#' @export
ffs_starter_positions <- function(conn){
  sp <- ffscrapr::ff_starter_positions(conn)
  sp$pos <- replace(sp$pos,sp$pos == "PK","K")
  sp
}
