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
#' @param actual_schedule a logical: use actual ff_schedule? default is FALSE
# @param parallel a logical: use parallel processing for optimizing lineups, default is FALSE
#' @param verbose a logical: print status messages? default is TRUE, configure with options(ffsimulator.verbose)
#'
#' @examples
#' \donttest{
#' conn <- mfl_connect(2021, 22627)
#' ff_simulate(conn, n_seasons = 25)
#' }
#'
#' @return an `ff_simulation` object which can be passed to `plot()` and contains the output data from the simulation.
#'
#' @seealso `vignette("Basic Simulations")` for example usage
#' @seealso `vignette("Custom Simulations")` for examples on using the subfunctions for your own processes.
#'
#' @export
ff_simulate <- function(conn,
                        n_seasons = 100,
                        n_weeks = 14,
                        best_ball = FALSE,
                        seed = NULL,
                        injury_model = c("simple", "none"),
                        base_seasons = 2012:2020,
                        actual_schedule = FALSE,
                        verbose = getOption("ffsimulator.verbose", default = TRUE)) {

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
  checkmate::assert_flag(verbose)
  checkmate::assert_flag(actual_schedule)

  start_logger <- verbose_logger(verbose, "start")
  end_logger <- verbose_logger(verbose, "end")

  #### Import Data ####

  if (verbose) cli::cli_rule("Starting simulation {Sys.time()}")

  start_logger(msg = "Importing data")

  league_info <- ffscrapr::ff_league(conn)

  scoring_history <- ffscrapr::ff_scoringhistory(conn, base_seasons)

  latest_rankings <- ffs_latest_rankings()

  franchises <- ffs_franchises(conn)
  rosters <- ffs_rosters(conn)

  lineup_constraints <- ffscrapr::ff_starter_positions(conn)

  end_logger(msg_done = "Importing data...done! {Sys.time()}")

  if(actual_schedule) schedule <- ffs_schedule(conn)

  #### Generate Projections ####

  start_logger(msg = "Generating Projections")

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

  end_logger(msg_done = "Generating Projections...done! {Sys.time()}")

  #### Calculate Roster Scores ####
  start_logger(msg = "Calculating Roster Scores")

  roster_scores <- ffs_score_rosters(
    projected_scores = projected_scores,
    rosters = rosters
  )

  end_logger(msg_done = "Calculating Roster Scores...done! {Sys.time()}")

  start_logger(msg = "Optimizing Lineups")

  optimal_scores <- ffs_optimise_lineups(
    roster_scores = roster_scores,
    lineup_constraints = lineup_constraints,
    best_ball = best_ball
  )

  end_logger(msg = "Optimizing Lineups...done! {Sys.time()}")

  #### Generate Schedules ####

  start_logger(msg = "Building Schedules")

  if(actual_schedule) {
    schedules <- ffs_repeat_schedules(n_seasons = n_seasons,
                                      actual_schedule = schedule)
    }

  if(!actual_schedule){
    schedules <- ffs_build_schedules(
      n_teams = NULL,
      n_seasons = n_seasons,
      n_weeks = n_weeks,
      franchises = franchises
    )
  }

  end_logger(msg_done = "Building Schedules...done! {Sys.time()}")

  #### Summarise Season ####
  start_logger(msg = "Summarising Simulation Data")

  summary_week <- ffs_summarise_week(optimal_scores, schedules)
  summary_season <- ffs_summarise_season(summary_week)
  summary_simulation <- ffs_summarise_simulation(summary_season)

  end_logger(msg_done = "Summarising Simulation Data...done! {Sys.time()}")

  #### Build and Return ####

  out <- structure(
    list(
      summary_simulation = summary_simulation,
      summary_season = summary_season,
      summary_week = summary_week,
      roster_scores = roster_scores,
      projected_scores = projected_scores,
      league_info = league_info,
      simulation_params = tibble::tibble(
        n_seasons = n_seasons,
        n_weeks = n_weeks,
        scrape_date = latest_rankings$scrape_date[[1]],
        best_ball = best_ball,
        seed = seed,
        injury_model = injury_model,
        actual_schedule = actual_schedule,
        base_seasons = list(base_seasons)
      )
    ),
    class = "ff_simulation"
  )

  if (verbose) cli::cli_rule("Simulation complete! {Sys.time()}")

  return(out)
}

#' @export
#' @noRd
print.ff_simulation <- function(x, ...) {
  cat("<ff_simulation: ",
    x$simulation_params$n_seasons,
    " simulated seasons of ",
    x$league_info$league_name,
    ">\n",
    sep = ""
  )
  str(x, max.level = 1)
  invisible(x)
}

dump_function <- function(...) NULL

verbose_logger <- function(verbose, type) {
  if (!verbose) {
    return(dump_function)
  }
  if (type == "start") {
    return(cli::cli_process_start)
  }
  if (type == "end") {
    return(cli::cli_process_done)
  }
}
