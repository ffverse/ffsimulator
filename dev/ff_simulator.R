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

library(cli)
library(progressr)


#### PARAMETERS ####

conn <- mfl_connect(season = 2021, league_id = 22627)

n_seasons <-  500 # any number of seasons
n_weeks <-  14 # any number of weeks per season
best_ball <-  FALSE # or TRUE
injury_model <-  "simple" # or none
base_seasons <-  2012:2020 # any numeric vector between 2012 and 2020
seed <- 613

set.seed(seed)

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
#### CALCULATE ROSTER SCORES AND OPTIMAL LINEUPS ####
roster_scores <- ffs_score_rosters(
  projected_scores = projected_scores,
  rosters = rosters
)
optimal_scores <-
  ffs_optimise_lineups(
    roster_scores = roster_scores,
    lineup_constraints = lineup_constraints,
    best_ball = best_ball,
    pos_filter = c("QB","RB","WR","TE")
  )
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
    summary_simulation = summary_simulation,
    summary_season = summary_season,
    summary_week = summary_week,
    league_info = league_info,
    simulation_params = tibble::tibble(
      n_seasons = n_seasons,
      n_weeks = n_weeks,
      best_ball = best_ball,
      seed = seed,
      injury_model = injury_model,
      base_seasons = base_seasons,
      scrape_date = latest_rankings$scrape_date[[1]]
    )
  ),
  class = "ff_simulation"
)

autoplot(simulation, type = "wins")
autoplot(simulation, type = "points")
autoplot(simulation, type = "rank")
