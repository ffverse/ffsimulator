library(ffpros) # https://ffpros.ffverse.com
library(tidyverse)
library(ffscrapr)
library(furrr)

options(future.rng.onMisuse = "ignore",
        ffpros.cache = "filesystem")

plan(multisession)

fp_set_ratelimit(rate_limit = FALSE)

seasons <- 2012:2015
weeks <- 1:16
pages <- c(
  "qb",
  "rb",
  "wr",
  "te",
  "k",
  "dst",
  "dl",
  "lb",
  "db"
)

fp_rankings_history <- crossing(pages, seasons, weeks) %>%
  mutate(rankings = future_pmap(
    list(pages, seasons, weeks),
    possibly(~fp_rankings(page = ..1, year = ..2, week = ..3),
             tibble())
  )) %>%
  unnest(rankings) %>%
  transmute(
    page_pos =
      str_remove_all(pages, "cheatsheets|^ppr|\\-") %>%
      toupper() %>%
      str_squish(),
    season = seasons,
    week = weeks,
    fantasypros_id = as.character(fantasypros_id),
    sportradar_id,
    player_name = dp_cleannames(player_name),
    pos = case_when(
      pos %in% c("CB", "S") ~ "DB",
      pos %in% c("OLB", "LB") ~ "LB",
      pos %in% c("DE", "DT", "NT") ~ "DL",
      TRUE ~ pos
    ),
    team,
    rank,
    ecr,
    sd
  ) %>%
  filter(page_pos == pos)

seasons2 <- 2016:nflreadr::most_recent_season()
pages2 <- c(
  "qb",
  "ppr-rb",
  "ppr-wr",
  "ppr-te",
  "k",
  "dst",
  "dl",
  "lb",
  "db"
)

fp_rankings_history2 <- crossing(pages2, seasons2, weeks) %>%
  mutate(rankings = future_pmap(
    list(pages2, seasons2, weeks),
    possibly(~fp_rankings(page = ..1, year = ..2, week = ..3),
             tibble())
  )) %>%
  unnest(rankings) %>%
  transmute(
    page_pos =
      str_remove_all(pages2, "cheatsheets|^ppr|\\-") %>%
      toupper() %>%
      str_squish(),
    season = seasons2,
    week = weeks,
    fantasypros_id = as.character(fantasypros_id),
    sportradar_id,
    player_name = dp_cleannames(player_name),
    pos,
    team,
    rank,
    ecr,
    sd
  ) %>%
  filter(page_pos == pos)

fp_rankings_history_week <- bind_rows(fp_rankings_history, fp_rankings_history2)

usethis::use_data(fp_rankings_history_week, overwrite = TRUE)
