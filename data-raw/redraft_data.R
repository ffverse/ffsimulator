library(ffscrapr)
library(tidyverse)
library(fpscrapr)

redraft_rankings <- tibble(season = 2016:2020) %>%
  mutate(
    redraft_rankings = map(season, ~fp_rankings("ppr-cheatsheets", year = .x))
  ) %>%
  unnest(redraft_rankings) %>%
  select(season, player_id, player_name,pos = player_position_id, sportsdata_id,
         rank_ecr, rank_min, rank_max, rank_ave, rank_std)

write_csv(redraft_rankings, "data-raw/fp_redraft_20162020.csv")
