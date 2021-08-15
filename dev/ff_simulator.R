# Custom Simulation Template

#### PACKAGE LOAD ####
options(ffscrapr.cache = "filesystem")

pkgload::load_all()
# library(ffsimulator)
library(ffscrapr)

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
# library(furrr)
# library(future)

library(cli)
library(progressr)

set.seed(NULL)

#### PARAMETERS ####

# future::plan(multisession)

conn <- ffscrapr::mfl_connect(season = 2021, league_id = 22627)

n_seasons <-  1000 # any number of seasons
n_weeks <-  14 # any number of weeks per season
best_ball <-  FALSE # or TRUE
injury_model <-  "simple" # or none
base_seasons <-  2012:2020 # any numeric vector between 2012 and 2020

#### IMPORT DATA ####


league_info <- ffscrapr::ff_league(conn = conn)

scoring_history <- ffscrapr::ff_scoringhistory(conn = conn, season = base_seasons)

latest_rankings <- ffs_latest_rankings()

rosters <- ffs_rosters(conn = conn)

lineup_constraints <- ffscrapr::ff_starter_positions(conn = conn)


#### GENERATE PROJECTIONS ####

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

# optimal_scores <- progressr::with_progress(
# tictoc::tic()
# roster_scores <- ffs_score_rosters(
#   projected_scores = projected_scores,
#   rosters = rosters
# )
#
# optimal_scores <-
#   ffs_optimise_lineups(
#     roster_scores = roster_scores,
#     lineup_constraints = lineup_constraints,
#     best_ball = best_ball,
#     pos_filter = c("QB","RB","WR","TE"),
#     parallel = TRUE,
#     verbose = TRUE
#     )
# tictoc::toc()

tictoc::tic()
roster_scores_dt <- ffs_dt_score_rosters(
  projected_scores = projected_scores,
  rosters = rosters
)
tictoc::toc()
tictoc::tic()
optimal_scores_dt <- ffs_optimise_lineups_dt(
  roster_scores = roster_scores_dt,
  lineup_constraints = lineup_constraints,
  best_ball = best_ball,
  pos_filter = c("QB","RB","WR","TE")
)
tictoc::toc()

#### SUMMARISE SIMULATION DATA ####

schedules <- ffs_build_schedules(
  n_teams = length(unique(rosters$franchise_id)),
  n_seasons = n_seasons,
  n_weeks = n_weeks
)

summary_week <- ffs_summarise_week(optimal_scores, schedules)
summary_season <- ffs_summarise_season(summary_week)
summary_simulation <- ffs_summarise_simulation(summary_season)


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
