#' Wins Added
#'
#' Run base simulation once for n seasons/weeks etc
#'
#' For every player on every team (except replacement level players),
#'
#' - remove them from that specific roster
#' - reoptimize the lineups just for that roster without the player
#' - use that set of weekly scores as the score for that week
#' - summarise the new simulation
#' - return new wins total and points total for that franchise
#'
#' Summarise WA as the difference between the sim with the player and the sim without them
#'
#' @param conn an connection to a league made with `ff_connect()` and friends (required)
#' @param ... parameters passed to `ff_simulate()`
#' @inheritDotParams ff_simulate
#'
#' @examples
#' \donttest {
#'
#' ff_wins_added(mfl_connect(2021,54040))
#'
#' }
#'
#' @return a dataframe summarising the net effect of each player on their team's wins
#'
#' @export
ff_wins_added <- function(conn, ...){

  #### TEST STUFF ####
  conn <- mfl_connect(2021,54040)
  verbose <- NULL

  #### ASSERTIONS ####

  if (!class(conn) %in% c("mfl_conn", "sleeper_conn", "flea_conn", "espn_conn")) {
    stop(
      "conn should be a connection object created by `ff_connect()` and friends!",
      call. = FALSE
    )
  }

  vcli_rule("BASE SIMULATION")

  # Run base simulation once for n seasons/weeks etc
  base_simulation <- ff_simulate(conn = conn,
                                 ...,
                                 return = "all")

  pos <- NULL
  allplay_winpct <- NULL

  rosters <- base_simulation$rosters[
    pos %in% base_simulation$simulation_params$pos_filter[[1]],
    c("player_id","player_name","league_id","franchise_name","franchise_id")
  ]

  vcli_rule("Start WAR calcs {Sys.time()}")

  war <- progressr::with_progress({
    p <- function() NULL

    if(requireNamespace("progressr",quietly = TRUE) &&
       getOption("ffsimulator.verbose", default = TRUE)){
      p <- progressr::progressor(nrow(rosters))
    }

    rosters[,
            .ffs_win_add(.SD, base_simulation,p),
            by = c("league_id","franchise_id","franchise_name","player_id","player_name"),
            .SDcols = c("player_id","player_name","franchise_id")
    ][order(-allplay_winpct)]
  })

  vcli_rule("WAR calcs complete! {Sys.time()}")

  return(war)
}

.ffs_win_add <- function(rosters, base_simulation, p = NULL){
  p_id <- rosters$player_id
  p_name <- rosters$player_name
  f_id <- rosters$franchise_id

  projected_score <- NULL
  franchise_id <- NULL
  player_id <- NULL

  wa_scores <- base_simulation$roster_scores[franchise_id == f_id][player_id == p_id, projected_score := NA]

  wa_optimal <- ffs_optimise_lineups(
    roster_scores = wa_scores,
    lineup_constraints = base_simulation$lineup_constraints,
    best_ball = base_simulation$simulation_params$best_ball,
    pos_filter = base_simulation$simulation_params$pos_filter[[1]]
  )

  all_scores <- data.table::rbindlist(
    list(base_simulation$optimal_scores[franchise_id != f_id],
         wa_optimal)
  )

  wa_week <- ffs_summarise_week(optimal_scores = all_scores, schedules = base_simulation$schedules)
  wa_season <- ffs_summarise_season(wa_week)
  wa_simulation <- ffs_summarise_simulation(wa_season)[franchise_id == f_id]
  o_sim <- base_simulation$summary_simulation[franchise_id == f_id]

  h2h_wins <- NULL
  h2h_winpct <- NULL
  allplay_wins <- NULL
  allplay_winpct <- NULL
  points_for <- NULL
  points_against <- NULL
  potential_points <- NULL

  war_simulation <- wa_simulation[
    franchise_id == f_id,
    list(
      h2h_wins = o_sim$h2h_wins - h2h_wins,
      h2h_winpct = o_sim$h2h_winpct - h2h_winpct,
      allplay_wins = o_sim$allplay_wins - allplay_wins,
      allplay_winpct = o_sim$allplay_winpct - allplay_winpct,
      points_for = o_sim$points_for - points_for,
      points_against = o_sim$points_against - points_against,
      potential_points = o_sim$potential_points - potential_points
    )]

  if(!is.null(p)) p()

  return(war_simulation)
}

# .ffs_win_add(p_id = "12186", p_name = "Diggs, Stefon", f_id = "0010", base_simulation = base_simulation)

