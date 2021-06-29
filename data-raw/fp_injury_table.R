library(tidyverse)
library(mgcv)
library(ffscrapr)
library(ffsimulator)
# pkgload::load_all()

conn <- mfl_connect(2021,54040)
base_seasons <- 2010:2020
scoring_history <- ff_scoringhistory(conn, base_seasons)

model_gam <- function(data){
  gam(games_played_rate ~  s(rank,bs = "cs"), data = data)
}

fp_injury_table <- ffsimulator::fp_rankings_history %>%
  dplyr::select(-"page_pos") %>%
  dplyr::left_join(
    ffscrapr::dp_playerids() %>%
      dplyr::select("fantasypros_id","gsis_id"),
    by = "fantasypros_id"
  ) %>%
  dplyr::filter(!is.na(.data$gsis_id), pos %in% c("QB","RB","WR","TE")) %>%
  dplyr::left_join(
    scoring_history %>%
      dplyr::filter(!is.na(.data$gsis_id), .data$week <= 17) %>%
      dplyr::select("season","gsis_id", "team", "points")
    , by = c("season","gsis_id")
  ) %>%
  dplyr::group_by(.data$season,
                  .data$pos,
                  .data$rank,
                  .data$fantasypros_id,
                  .data$player_name
  ) %>%
  dplyr::summarise(
    week_outcomes = list(points),
    games_played = dplyr::n(),
    games_missed = 16-games_played,
    games_played_rate = games_played / 16
  ) %>%
  ungroup() %>%
  group_by(pos) %>%
  nest() %>%
  mutate(
    model = map(data, model_gam),
    prob_gp = map2(model,data, ~as.numeric(predict(.x, new_data = .y))),
    model = NULL
  ) %>%
  ungroup() %>%
  unnest(c(data,prob_gp)) %>%
  distinct(pos, rank, prob_gp) %>%
  arrange(pos, rank)

usethis::use_data(fp_injury_table, overwrite = TRUE)

