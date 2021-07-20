#' Simulate Fantasy Seasons
#'
#' The main function of the package - uses bootstrap resampling to run fantasy football season simulations supported by historical rankings and nflfastR data, calculating optimal lineups, and returns aggregated results.
#'
#' @param conn an connection to a league made with `ff_connect()` and friends (required)
#' @param n_seasons number of seasons to simulate, default = 100
#' @param n_weeks number of weeks per season, default = 14
#' @param best_ball a logical: are weekly wins based on optimal lineups?
#' @param seed an integer to control reproducibility
#' @param injury_model select between "simple", "none"
#' @param base_seasons a numeric vector that selects seasons as base data, earliest available is 2012
#' @param parallel a logical: use parallel processing for optimizing lineups, default is FALSE
#'
#' @examples
#'
#' \donttest{
#'   conn <- mfl_connect(2021, 22627)
#'   ff_simulate(conn, n_seasons = 25)
#' }
#'
#' @return an `ff_simulation` object which can be passed to `plot()` and contains the output data from the simulation.
#'
#' @seealso `vignette("Basic Simulations")` for example usage
#' @seealso `vignette("Custom Simulations")` for examples on using the subfunctions for your own processes.
#'
#' @export
ff_simulate <- function(
  conn,
  n_seasons = 100,
  n_weeks = 14,
  best_ball = FALSE,
  seed = NULL,
  injury_model = c("simple", "none"),
  base_seasons = 2012:2020,
  parallel = FALSE
) {

  #### Assertions ####

  if (!class(conn) %in% c("mfl_conn", "sleeper_conn", "flea_conn", "espn_conn")) {
    stop("conn should be a connection object created by `ff_connect()` and friends!",
      call. = FALSE
    )
  }

  injury_model <- match.arg(injury_model)
  checkmate::assert_numeric(base_seasons, lower = 2012, upper = 2020)
  checkmate::assert_int(n_seasons, lower = 1)
  checkmate::assert_int(n_weeks, lower = 1)
  checkmate::assert_int(seed, null.ok = TRUE)
  if (!is.null(seed)) set.seed(seed)
  checkmate::assert_flag(best_ball)
  checkmate::assert_flag(parallel)

  #### Import Data ####

  league_info <- ffscrapr::ff_league(conn)

  scoring_history <- ffscrapr::ff_scoringhistory(conn, base_seasons)

  latest_rankings <- ffs_latest_rankings()

  rosters <- ffs_rosters(conn)

  lineup_constraints <- ffscrapr::ff_starter_positions(conn)

  #### Generate Projections ####

  adp_outcomes <- ffs_adp_outcomes(
    scoring_history = scoring_history,
    injury_model = injury_model
  )
  projected_scores <- ffs_generate_projections(
    adp_outcomes = adp_outcomes,
    latest_rankings = latest_rankings,
    n_seasons = n_seasons,
    n_weeks = n_weeks,
    rosters = rosters
  )
  #### Calculate Roster Scores ####

  roster_scores <- ffs_score_rosters(
    projected_scores = projected_scores,
    rosters = rosters
  )

  optimal_scores <- ffs_optimise_lineups(
    roster_scores = roster_scores,
    lineup_constraints = lineup_constraints,
    best_ball = best_ball,
    parallel = parallel
  )

  #### Generate Schedules ####

  schedules <- ffs_build_schedules(
    n_teams = length(unique(rosters$franchise_id)),
    n_seasons = n_seasons,
    n_weeks = n_weeks
  )

  #### Summarise Season ####

  summary_week <- ffs_summarise_week(optimal_scores, schedules)
  summary_season <- ffs_summarise_season(summary_week)
  summary_simulation <- ffs_summarise_simulation(summary_season)

  #### Build and Return ####

  out <- structure(
    list(
      summary_simulation = summary_simulation,
      summary_season = summary_season,
      summary_week = summary_week,
      roster_scores = roster_scores,
      projected_scores = projected_scores,
      league_info = league_info,
      simulation_params = list(
        n_seasons = n_seasons,
        n_weeks = n_weeks,
        best_ball = best_ball,
        seed = seed,
        injury_model = injury_model,
        base_seasons = 2012:2020
      )
    ),
    class = "ff_simulation"
  )

  return(out)
}

#' @export
#' @noRd
print.ff_simulation <- function(x,...){
  cat("<ff_simulation: ",
      x$simulation_params$n_seasons,
      " simulated seasons of ",
      x$league_info$league_name,
      ">\n",
      sep = ""
      )
  str(x,max.level = 2)
  invisible(x)
}
