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

n <-  1000 # any number of seasons
best_ball <-  FALSE # or TRUE
base_seasons <-  2012:2020 # any numeric vector between 2012 and 2020
seed <- 613
pos_filter <-  c("QB","RB","WR","TE","K")

set.seed(seed)

#### IMPORT DATA ####

league_info <- ffscrapr::ff_league(conn)

scoring_history <- ffscrapr::ff_scoringhistory(conn, base_seasons)

latest_rankings <- ffs_latest_rankings(type = "week")

franchises <- ffs_franchises(conn)
rosters <- ffs_rosters(conn)

lineup_constraints <- ffs_starter_positions(conn)

#### SCHEDULES ####


#### Uncomment out this section to use "fake" schedules instead of the real upcoming matchup ####
#
# schedules <- ffs_build_schedules(
#   n_seasons = n,
#   n_weeks = 1,
#   franchises = franchises
# ) %>%
#   dplyr::mutate(week = season,
#                 season = 1
#   )

#### Use real upcoming matchup, comment out this section if the above section is used ####

schedules <- ffs_schedule(conn) %>%
  dplyr::filter(week == min(week)) %>%
  ffs_repeat_schedules(n_seasons = n) %>%
  dplyr::mutate(week = season,
                season = 1)

#### GENERATE PROJECTIONS ####

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
summary_season <- ffs_summarise_inseason(summary_week, n)

simulation <- structure(
  list(
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
      actual_schedule = TRUE, # or FALSE depending on what you changed above
      base_seasons = list(base_seasons)
    )
  ),
  class = "ff_simulation_week"
)

plot(simulation, type = "luck")
plot(simulation, type = "points")
