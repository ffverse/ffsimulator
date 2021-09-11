# CUSTOM SEASON SIMULATION TEMPLATE

#### PACKAGE LOAD ####
options(ffscrapr.cache = "filesystem")

# pkgload::load_all()
library(ffsimulator)
library(ffscrapr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(ggridges)

#### PARAMETERS ####

conn <- mfl_connect(season = 2021, league_id = 22627)

n_seasons <-  200 # any number of seasons
n_weeks <-  14 # any number of weeks per season
best_ball <-  FALSE # or TRUE
gp_model <-  "simple" # or none
base_seasons <-  2012:2020 # any numeric vector between 2012 and 2020
seed <- 613
pos_filter <-  c("QB","RB","WR","TE","K")

set.seed(seed)

#### IMPORT DATA ####

league_info <- ffscrapr::ff_league(conn)

scoring_history <- ffscrapr::ff_scoringhistory(conn, base_seasons)

latest_rankings <- ffs_latest_rankings(type = "draft")

franchises <- ffs_franchises(conn)
rosters <- ffs_rosters(conn)

lineup_constraints <- ffscrapr::ff_starter_positions(conn) %>%
  dplyr::mutate(pos = replace(.data$pos,.data$pos == "PK","K"))

#### SCHEDULES ####

#### Uncomment this section to use actual season schedule (and simulate only unplayed weeks) ####
# schedules <- ffs_repeat_schedules(actual_schedule = ffs_schedule(conn),
#                                   n_seasons = n_seasons)
#
# weeks <- unique(schedule$week)

#### Comment out this section if the above section is used - this generates "fake" schedules ####
weeks <- seq_len(n_weeks)

schedules <- ffs_build_schedules(
  n_seasons = n_seasons,
  n_weeks = n_weeks,
  franchises = franchises
)

#### GENERATE PROJECTIONS ####

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
    pos_filter = pos_filter
  )

#### SUMMARISE SIMULATION DATA ####

summary_week <- ffs_summarise_week(optimal_scores, schedules)
summary_season <- ffs_summarise_season(summary_week)
summary_simulation <- ffs_summarise_simulation(summary_season)

simulation <- structure(
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
      base_seasons = list(base_seasons)
    )
  ),
  class = "ff_simulation"
)

plot(simulation, type = "wins")
plot(simulation, type = "points")
plot(simulation, type = "rank")
