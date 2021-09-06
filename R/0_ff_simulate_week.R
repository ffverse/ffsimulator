#' Simulate Fantasy Week
#'
#' This function simulates a single upcoming week using the same methodology as in the season-long simulation, `ff_simulate()`.
#'
#' @param conn an connection to a league made with `ff_connect()` and friends (required)
#' @param n number of times to simulate the upcoming week, default is 1000
#' @param best_ball a logical: are weekly wins based on optimal lineups?
#' @param seed an integer to control reproducibility
#' @param base_seasons a numeric vector that selects seasons as base data, earliest available is 2012
#' @param actual_schedule a logical: use actual ff_schedule? default is TRUE
#' @param pos_filter a character vector of positions to filter/run, default is c("QB","RB","WR","TE","K")
#' @param verbose a logical: print status messages? default is TRUE, configure with options(ffsimulator.verbose)
#'
#' @examples
#' \donttest{
#' conn <- mfl_connect(2021, 22627)
#' ff_simulate_week(conn, n = 1000, actual_schedule = TRUE)
#' }
#'
#' @return an `ff_simulation` object which can be passed to `plot()` and contains the output data from the simulation.
#'
#' @seealso `vignette("basic")` for example usage
#' @seealso `vignette("custom")` for examples on using the subfunctions for your own processes.
#'
#' @export
ff_simulate_week <- function(conn,
                             n = 1000,
                             best_ball = FALSE,
                             seed = NULL,
                             base_seasons = 2012:2020,
                             actual_schedule = TRUE,
                             pos_filter = c("QB","RB","WR","TE","K"),
                             verbose = getOption("ffsimulator.verbose", default = TRUE)) {

  #### Assertions ####

  if (!class(conn) %in% c("mfl_conn", "sleeper_conn", "flea_conn", "espn_conn")) {
    stop("conn should be a connection object created by `ff_connect()` and friends!",
         call. = FALSE
    )
  }

  # gp_model <- rlang::arg_match(gp_model)
  checkmate::assert_subset(pos_filter, c("QB","RB","WR","TE","K"))
  checkmate::assert_numeric(base_seasons, lower = 2012, upper = 2020)
  checkmate::assert_int(n, lower = 1)
  checkmate::assert_int(seed, null.ok = TRUE)
  if (!is.null(seed)) set.seed(seed)
  checkmate::assert_flag(best_ball)
  checkmate::assert_flag(verbose)
  checkmate::assert_flag(actual_schedule)

  vcli_start <- verbose_cli(verbose, "start")
  vcli_end <- verbose_cli(verbose, "end")
  vcli_rule <- verbose_cli(verbose,"rule")

  #### Import Data ####

  vcli_rule("Starting simulation {Sys.time()}")

  vcli_start(msg = "Importing data")

  league_info <- ffscrapr::ff_league(conn)

  scoring_history <- ffscrapr::ff_scoringhistory(conn, base_seasons)

  latest_rankings <- ffs_latest_rankings(type = "week")

  franchises <- ffs_franchises(conn)
  rosters <- ffs_rosters(conn)

  lineup_constraints <- ffscrapr::ff_starter_positions(conn) %>%
    dplyr::mutate(pos = replace(.data$pos,.data$pos == "PK","K"))

  if(actual_schedule) {
    schedule <- ffs_schedule(conn) %>%
      dplyr::filter(.data$week==min(.data$week))

    if(nrow(schedule)==0) {
      cli::cli_alert_danger("No unplayed weeks to simulate!")
      out <- structure(list(schedule = ffscrapr::ff_schedule(conn),
                            league_info = league_info,
                            simulation_params = tibble::tibble(
                              n = n,
                              scrape_date = latest_rankings$scrape_date[[1]],
                              best_ball = best_ball,
                              seed = seed,
                              # gp_model = gp_model,
                              actual_schedule = actual_schedule,
                              base_seasons = list(base_seasons)
                            )),
                       class = "ff_simulation")
      return(out)
    }
  }

  vcli_end(msg_done = "Importing data...done! {Sys.time()}")

  #### Generate Projections ####

  vcli_start(msg = "Generating Projections")

  adp_outcomes <- ffs_adp_outcomes_week(
    scoring_history = scoring_history,
    pos_filter = pos_filter
  )

  projected_scores <- ffs_generate_projections_week(
    adp_outcomes = adp_outcomes,
    latest_rankings = latest_rankings,
    n = n,
    rosters = rosters
  )

  vcli_end(msg_done = "Generating Projections...done! {Sys.time()}")

  #### Calculate Roster Scores ####
  vcli_start(msg = "Calculating Roster Scores")

  roster_scores <- ffs_score_rosters(
    projected_scores = projected_scores,
    rosters = rosters
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

  #### Generate Schedules ####

  vcli_start(msg = "Building Schedules")

  if(actual_schedule) {
    schedules <- ffs_repeat_schedules(n_seasons = n,
                                      actual_schedule = schedule) %>%
      dplyr::mutate(week = .data$season,
                    season = 1)
  }

  if(!actual_schedule){
    schedules <- ffs_build_schedules(
      n_seasons = 1000,
      n_weeks = 1,
      franchises = franchises
    ) %>%
      dplyr::mutate(
        week = .data$season,
        season = 1
      )
  }

  vcli_end(msg_done = "Building Schedules...done! {Sys.time()}")

  #### Summarise Season ####
  vcli_start(msg = "Summarising Simulation Data")

  summary_week <- ffs_summarise_week(optimal_scores, schedules)
  summary_season <- ffs_summarise_season(summary_week) %>%
    dplyr::mutate_at(c("points_for","points_against","potential_points"), ~round(.x/n,2))

  summary_simulation <- ffs_summarise_simulation(summary_season)

  vcli_end(msg_done = "Summarising Simulation Data...done! {Sys.time()}")

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
        n = n,
        scrape_date = latest_rankings$scrape_date[[1]],
        best_ball = best_ball,
        seed = seed,
        actual_schedule = actual_schedule,
        base_seasons = list(base_seasons),
        pos_filter = list(pos_filter)
      )
    ),
    class = "ff_simulation_week"
  )

  vcli_rule("Simulation complete! {Sys.time()}")

  return(out)
}

#' @export
#' @noRd
print.ff_simulation_week <- function(x, ...) {
  cat("<ff_simulation: ",
      x$simulation_params$n,
      " simulated seasons of ",
      x$league_info$league_name,
      ">\n",
      sep = ""
  )
  str(x, max.level = 1)
  invisible(x)
}

dump_function <- function(...) NULL

verbose_cli <- function(verbose, type) {
  if (!verbose) return(dump_function)

  if (type == "start") return(cli::cli_process_start)

  if (type == "end") return(cli::cli_process_done)

  if (type == "rule") return(cli::cli_rule)

  return(NULL)
}
