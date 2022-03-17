#' Simulate Fantasy Seasons
#'
#' The main function of the package - uses bootstrap resampling to run fantasy football season simulations supported by historical rankings and nflfastR data, calculating optimal lineups, and returns aggregated results.
#'
#' @param conn an connection to a league made with `ff_connect()` and friends (required)
#' @param n_seasons number of seasons to simulate, default = 100
#' @param n_weeks number of weeks per season, default = 14
#' @param best_ball a logical: are weekly wins based on optimal lineups?
#' @param seed an integer to control reproducibility
#' @param gp_model select between "simple", "none" to apply a model for whether a player played in a given game, defaults to "simple"
#' @param base_seasons a numeric vector that selects seasons as base data, earliest available is 2012
#' @param actual_schedule a logical: use actual ff_schedule? default is FALSE
#' @param replacement_level a logical: use best available on waiver as  replacement level? defaults to TRUE
#' @param pos_filter a character vector of positions to filter/run, default is c("QB","RB","WR","TE","K")
#' @param verbose a logical: print status messages? default is TRUE, configure with options(ffsimulator.verbose)
#' @param playoffs one of c("six_team","four_team", "none") will automatically simulate a playoff bracket
#' @param return one of c("default", "all") - what objects to return in the output list
#'
#' @examples
#' \donttest{
#' try({ # try block to prevent CRAN-related issues
#' conn <- mfl_connect(2021, 22627)
#' ff_simulate(conn, n_seasons = 25)
#' })
#' }
#'
#' @return an `ff_simulation` object which can be passed to `plot()` and contains the output data from the simulation.
#'
#' @seealso `vignette("basic")` for example usage
#' @seealso `vignette("custom")` for examples on using the subfunctions for your own processes.
#'
#' @export
ff_simulate <- function(conn,
                        n_seasons = 100,
                        n_weeks = 14,
                        best_ball = FALSE,
                        seed = NULL,
                        gp_model = c("simple", "none"),
                        base_seasons = 2012:2020,
                        actual_schedule = FALSE,
                        replacement_level = TRUE,
                        pos_filter = c("QB","RB","WR","TE","K"),
                        verbose = NULL,
                        playoffs = c("six_team","four_team","none"),
                        return = c("default", "all")
) {

  #### TEST ####

  # conn <- mfl_connect(2021,54040)
  # conn <- sleeper_connect(2021,"734442977157603328")
  # verbose <- NULL
  # base_seasons = 2012:2020
  # gp_model = "simple"
  # pos_filter = c("QB","RB","WR","TE","K")
  # n_seasons = 100
  # n_weeks = 14
  # best_ball = FALSE
  # seed = NULL
  # base_seasons = 2012:2020
  # actual_schedule = TRUE
  # pos_filter = c("QB","RB","WR","TE","K")
  # verbose = TRUE
  # return = "all"
  # playoffs = "six_team"

  #### Assertions ####

  if (!class(conn) %in% c("mfl_conn", "sleeper_conn", "flea_conn", "espn_conn")) {
    stop("conn should be a connection object created by `ff_connect()` and friends!",
         call. = FALSE
    )
  }

  gp_model <- rlang::arg_match0(gp_model, c("simple","none"))
  return <- rlang::arg_match0(return, c("default","all"))
  checkmate::assert_subset(pos_filter, c("QB","RB","WR","TE","K"))
  checkmate::assert_numeric(base_seasons, lower = 2012, upper = 2020)
  checkmate::assert_int(n_seasons, lower = 1)
  checkmate::assert_int(n_weeks, lower = 1)
  checkmate::assert_int(seed, null.ok = TRUE)
  if (!is.null(seed)) set.seed(seed)
  checkmate::assert_flag(best_ball)
  if(!is.null(verbose)) set_verbose(verbose)
  checkmate::assert_flag(actual_schedule)
  checkmate::assert_flag(replacement_level)

  #### Import Data ####

  vcli_rule("Starting simulation {Sys.time()}")

  vcli_start(msg = "Importing data")

  league_info <- ffscrapr::ff_league(conn)

  scoring_history <- ffscrapr::ff_scoringhistory(conn, base_seasons)

  latest_rankings <- ffs_latest_rankings(type = "draft")

  franchises <- ffs_franchises(conn)
  rosters <- ffs_rosters(conn)

  lineup_constraints <- ffs_starter_positions(conn)

  weeks <- seq_len(n_weeks)

  if(actual_schedule) {
    schedule <- ffs_schedule(conn)

    weeks <- unique(schedule$week)

    if(length(weeks)==0) {

      cli::cli_alert_danger("No unplayed weeks to simulate!")
      out <- structure(list(schedule = ffscrapr::ff_schedule(conn),
                            league_info = league_info,
                            simulation_params = list(
                              n_seasons = n_seasons,
                              n_weeks = n_weeks,
                              scrape_date = latest_rankings$scrape_date[[1]],
                              best_ball = best_ball,
                              seed = seed,
                              gp_model = gp_model,
                              actual_schedule = actual_schedule,
                              base_seasons = list(base_seasons)
                            )),
                       class = "ff_simulation")

      return(out)

    }

    cli::cli_alert_info("Simulating only unplayed weeks: {
                        min(weeks,na.rm = TRUE)}-{
                        max(weeks, na.rm = TRUE)}")
  }

  vcli_end(msg_done = "Importing data...done! {Sys.time()}")

  #### Generate Projections ####

  vcli_start(msg = "Generating Projections")

  if(!replacement_level) rosters_rl <- rosters

  if(replacement_level){
    rosters_rl <- ffs_add_replacement_level(rosters = rosters,
                                            latest_rankings = latest_rankings,
                                            franchises = franchises,
                                            lineup_constraints = lineup_constraints,
                                            pos_filter = pos_filter)
  }

  adp_outcomes <- ffs_adp_outcomes(
    scoring_history = scoring_history,
    gp_model = gp_model,
    pos_filter = pos_filter
  )

  projected_scores <- ffs_generate_projections(
    adp_outcomes = adp_outcomes,
    latest_rankings = latest_rankings,
    n_seasons = n_seasons,
    weeks = weeks,
    rosters = rosters_rl
  )

  vcli_end(msg_done = "Generating Projections...done! {Sys.time()}")

  vcli_start(msg = "Calculating Roster Scores")

  roster_scores <- ffs_score_rosters(
    projected_scores = projected_scores,
    rosters = rosters_rl
  )

  vcli_end(msg_done = "Calculating Roster Scores...done! {Sys.time()}")

  vcli_start(msg = "Optimizing Lineups")

  optimal_scores <- ffs_optimise_lineups(
    roster_scores = roster_scores,
    lineup_constraints = lineup_constraints,
    best_ball = best_ball,
    pos_filter = pos_filter
  )

  vcli_end(msg = "Optimizing Lineups...done! {Sys.time()}")

  vcli_start(msg = "Building Schedules")

  if(actual_schedule) {
    schedules <- ffs_repeat_schedules(n_seasons = n_seasons,
                                      actual_schedule = schedule)
  }

  if(!actual_schedule){
    schedules <- ffs_build_schedules(
      n_seasons = n_seasons,
      n_weeks = n_weeks,
      franchises = franchises
    )
  }

  vcli_end(msg_done = "Building Schedules...done! {Sys.time()}")

  vcli_start(msg = "Summarising Simulation Data")

  summary_week <- ffs_summarise_week(optimal_scores, schedules)
  summary_season <- ffs_summarise_season(summary_week)
  summary_simulation <- ffs_summarise_simulation(summary_season)

  vcli_end(msg_done = "Summarising Simulation Data...done! {Sys.time()}")

  if(return == "default"){

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
          scrape_date = latest_rankings$scrape_date[[1]],
          best_ball = best_ball,
          seed = seed,
          gp_model = gp_model,
          actual_schedule = actual_schedule,
          base_seasons = list(base_seasons),
          pos_filter = list(pos_filter)
        )
      ),
      class = "ff_simulation"
    )
  }

  if(return == "all"){

    out <- structure(
      list(
        summary_simulation = summary_simulation,
        summary_season = summary_season,
        summary_week = summary_week,
        schedules = schedules,
        optimal_scores = optimal_scores,
        roster_scores = roster_scores,
        projected_scores = projected_scores,
        scoring_history = scoring_history,
        franchises = franchises,
        rosters = rosters,
        lineup_constraints = lineup_constraints,
        latest_rankings = latest_rankings,
        league_info = league_info,
        simulation_params = list(
          n_seasons = n_seasons,
          n_weeks = n_weeks,
          scrape_date = latest_rankings$scrape_date[[1]],
          best_ball = best_ball,
          seed = seed,
          gp_model = gp_model,
          actual_schedule = actual_schedule,
          base_seasons = list(base_seasons),
          pos_filter = list(pos_filter)
        )
      ),
      class = "ff_simulation"
    )
  }

  vcli_rule("Simulation complete! {Sys.time()}")

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
  str(x, max.level = 1, give.attr = FALSE)
  invisible(x)
}
