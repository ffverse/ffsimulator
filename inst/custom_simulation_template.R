# Custom Simulation Template

#### PACKAGE LOAD ####
options(ffscrapr.cache = "filesystem")

library(ffsimulator)
library(ffscrapr)

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(furrr)
library(future)

library(cli)
library(progressr)


set.seed(NULL)

#### PARAMETERS ####

future::plan(future::multisession)

conn <- ffscrapr::mfl_connect(season = 2021, league_id = 22627)

n_seasons <-  100 # any number of seasons
n_weeks <-  14 # any number of weeks per season
best_ball <-  FALSE # or TRUE
injury_model <-  "simple" # or none
base_seasons <-  2012:2020 # any numeric vector between 2012 and 2020

#### IMPORT DATA ####
cli::cli_rule("Starting simulation {Sys.time()}")

cli::cli_process_start(msg = "Importing data")

league_info <- ffscrapr::ff_league(conn = conn)

scoring_history <- ffscrapr::ff_scoringhistory(conn = conn, season = base_seasons)

latest_rankings <- ffsimulator::ffs_latest_rankings()

rosters <- ffsimulator::ffs_rosters(conn = conn)

lineup_constraints <- ffscrapr::ff_starter_positions(conn = conn)

cli::cli_process_done(msg_done = "Importing data...done! {Sys.time()}")

#### GENERATE PROJECTIONS ####
cli::cli_process_start(msg = "Generating Projections")

adp_outcomes <- ffsimulator::ffs_adp_outcomes(
  scoring_history = scoring_history,
  injury_model = injury_model
)
projected_scores <- ffsimulator::ffs_generate_projections(
  adp_outcomes = adp_outcomes,
  latest_rankings = latest_rankings,
  n_seasons = n_seasons,
  n_weeks = n_weeks,
  rosters = rosters
)

cli::cli_process_done(msg_done = "Generating Projections...done! {Sys.time()}")


#### CALCULATE ROSTER SCORES AND OPTIMAL LINEUPS ####
cli::cli_process_start(msg = "Calculating Roster Scores")

roster_scores <- ffsimulator::ffs_score_rosters(
  projected_scores = projected_scores,
  rosters = rosters
)

cli::cli_process_done(msg_done = "Calculating Roster Scores...done! {Sys.time()}")

cli::cli_alert_info("Optimizing Lineups...")

optimal_scores <- progressr::with_progress(
  ffsimulator::ffs_optimise_lineups(
    roster_scores = roster_scores,
    lineup_constraints = lineup_constraints,
    best_ball = best_ball,
    pos_filter = c("QB","RB","WR","TE"),
    parallel = TRUE,
    verbose = TRUE)
)

cli::cli_alert_success("...done! {Sys.time()}")


#### SUMMARISE SIMULATION DATA ####
cli::cli_process_start(msg = "Summarising Simulation Data")

schedules <- ffsimulator::ffs_build_schedules(
  n_teams = length(unique(rosters$franchise_id)),
  n_seasons = n_seasons,
  n_weeks = n_weeks
)

summary_week <- ffsimulator::ffs_summarise_week(optimal_scores, schedules)
summary_season <- ffsimulator::ffs_summarise_season(summary_week)
summary_simulation <- ffsimulator::ffs_summarise_simulation(summary_season)

cli::cli_process_done(msg_done = "Summarising Simulation Data...done! {Sys.time()}")

simulation <- structure(
  list(
    summary_season = summary_season,
    summary_week = summary_week,
    league_info = league_info,
    simulation_params = tibble::tibble(
      n_seasons = n_seasons,
      n_weeks = n_weeks,
      best_ball = best_ball,
      seed = seed,
      injury_model = injury_model,
      base_seasons = base_seasons
    )
  ),
  class = "ff_simulation"
)

autoplot(out, type = "wins")
autoplot(out, type = "points")
autoplot(out, type = "ranks")
