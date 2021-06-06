
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ffsimulator

<!-- badges: start -->

[![CRAN
status](https://img.shields.io/cran/v/ffsimulator?style=flat-square&logo=R&label=CRAN)](https://CRAN.R-project.org/package=ffsimulator)
[![Dev
status](https://img.shields.io/github/r-package/v/dynastyprocess/ffsimulator/main?label=dev&style=flat-square&logo=github)](https://ffsimulator.dynastyprocess.com/dev/)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg?style=flat-square)](https://lifecycle.r-lib.org/articles/stages.html)
[![R build
status](https://img.shields.io/github/workflow/status/dynastyprocess/ffsimulator/R-CMD-check?label=R%20check&style=flat-square&logo=github)](https://github.com/DynastyProcess/ffsimulator/actions)
[![nflverse
discord](https://img.shields.io/discord/591914197219016707.svg?color=5865F2&label=nflverse%20discord&logo=discord&logoColor=5865F2&style=flat-square)](https://discord.com/invite/5Er2FBnnQa)

<!-- badges: end -->

The goal of ffsimulator is to simulate fantasy football seasons. This
helps analyze distributions of:

-   expected season finish
-   player contribution to wins
-   etc?

by simulating 100 fantasy seasons with historical weekly finishes +
fantasypros current redraft rankings to determine overall/relative team
strengths.

## Installation

Install the development version from GitHub with:

``` r
# install.packages("remotes")

remotes::install_github("dynastyprocess/ffsimulator", ref = "dev")
```

## Roadmap

-   Connect via ffsimulator
-   DOWNLOAD SCORING HISTORY
-   CALCULATES SEASON RANKS AND THE POPULATION OF PLAYER SCORES FOR THAT
    RANK
-   DOWNLOADS LATEST FANTASYPROS RANKINGS
-   CONNECTS ROSTERS TO FANTASYPROS RANKINGS + PLAYER SCORES
-   RANDOMLY SELECTS N GAMES FOR EACH RANK
-   CALCULATE OPTIMAL LINEUPS
    -   two paths: a) develop heuristics b) use lpSolve
    -   prefer lpSolve given the current output of ff\_starterpositions
-   LOGIC FOR CALCULATING STARTED LINEUPS
    -   1.  Always choose highest ranked player by fantasypros

    -   1.  Assume random percentage between 70-85% of optimal points
-   APPLY WEEK SCORES INTO FANTASY SEASON SCHEDULES (use tony’s ffsched
    package)
-   CALCULATE WIN/LOSS (H2H, ALL-PLAY) + TOTAL SEASON POINTS FOR/
    POTENTIAL POINTS
-   figure out plots and visualizations later?

``` r
# library(ffsimulator)
library(ffscrapr)
library(dplyr)
library(tidyr)
library(purrr)
library(arrow)

## CONNECT TO LEAGUE

conn <- mfl_connect(2021, 54040)

## DOWNLOAD SCORING HISTORY

scoring_history <- ffsimulator::ff_scoringhistory(conn,2006:2020)

## CALCULATES SEASON RANKS AND THE POPULATION OF PLAYER SCORES FOR THAT RANK

pos_rank <- scoring_history %>% 
  group_by(season, gsis_id, mfl_id, player_name, pos, team) %>% 
  summarise(
    games = n(),
    ppg = mean(points, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  filter(
    games > 4
  ) %>% 
  group_by(season, pos) %>% 
  mutate(rank = rank(desc(ppg),ties.method = "random")) %>% 
  ungroup() %>% 
  arrange(season, pos, rank) %>% 
  left_join(
    scoring_history %>% select(season, gsis_id, week, points),
    by = c("season", "gsis_id")
  ) %>% 
  group_by(pos, rank) %>% 
  summarise(
    all_points = list(points)
  ) %>% 
  ungroup() %>% 
  mutate(one_season = map(all_points, sample, size = 1000, replace = TRUE))

# fp_parquet <- tempfile()
# download.file("https://github.com/dynastyprocess/data/raw/master/files/db_fpecr.parquet",fp_parquet)

## DOWNLOADS LATEST FANTASYPROS RANKINGS

fantasypros <- arrow::read_parquet("db_fpecr(1).parquet") %>% 
  filter(ecr_type == "ro", scrape_date == max(scrape_date), pos %in% c("QB","RB","WR","TE")) %>% 
  select(player, fantasypros_id = id, pos, team, ecr, sd) %>% 
  group_by(pos) %>% 
  mutate(rank = rank(ecr, ties.method = "random")) %>% 
  ungroup()

## CONNECTS ROSTERS TO FANTASYPROS + PLAYER SCORES 

rosters <- ff_rosters(conn) %>% 
  select(franchise_id, franchise_name, mfl_id = player_id, player_name, pos, team, age) %>% 
  left_join(dp_playerids() %>% select(fantasypros_id, mfl_id),
            by = "mfl_id") %>% 
  left_join(fantasypros %>% select(fantasypros_id, rank),
            by = "fantasypros_id") %>% 
  left_join(pos_rank %>% select(pos,rank,all_points),
            by = c("pos","rank"))

## RANDOMLY SELECTS N GAMES FOR EACH RANK

  mutate(
    season_games = map_if(all_points,!is.na(rank), sample, size = 17, replace = TRUE)
  )

## CALCULATE OPTIMAL LINEUPS

# two paths: a) develop heuristics b) use lpSolve
# ideally solver does the optimisation

starter_positions <- ffsimulator::ff_starter_positions(conn)

## LOGIC FOR CALCULATING STARTED LINEUPS
# a) Always choose highest ranked player by fantasypros
# b) Assume random percentage between 70-85% of optimal points

## APPLY WEEK SCORES INTO FANTASY SEASON SCHEDULES
# (use tony's ffsched package)

## CALCULATE WIN/LOSS (H2H, ALL-PLAY) + TOTAL SEASON POINTS FOR/ POTENTIAL POINTS

## figure out plots and visualizations later? 
```
