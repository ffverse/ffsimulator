pkgload::load_all()
library(ffscrapr)
library(tidyverse)

nflfastr_rosters <- nflfastr_rosters(2012:2020) %>%
  select(
    season,
    gsis_id,
    sportradar_id,
    player_name = full_name,
    pos = position,
    team
  ) %>%
  filter(pos %in% c("QB", "RB", "WR", "TE")) %>%
  group_by(gsis_id) %>%
  fill(sportradar_id, .direction = "downup") %>%
  ungroup()

fp_rankings <- ffsimulator::fp_rankings_history %>%
  filter(pos %in% c("QB", "RB", "WR", "TE")) %>%
  group_by(fantasypros_id) %>%
  fill(sportradar_id, .direction = "downup") %>%
  ungroup() %>%
  dplyr::left_join(
    ffscrapr::dp_playerids() %>%
      dplyr::select("fantasypros_id", "gsis_id"),
    by = "fantasypros_id"
  ) %>%
  filter(is.na(gsis_id)) %>%
  distinct() %>%
  transmute(
    season,
    fantasypros_id,
    sportradar_id,
    player_name,
    merge_name = ffscrapr::dp_cleannames(player_name, lowercase = TRUE),
    pos
  ) %>%
  left_join(
    nflfastr_rosters %>%
      transmute(
        gsis_id,
        merge_name = ffscrapr::dp_cleannames(player_name, lowercase = TRUE),
        season,
        pos,
        nfl_team = team
      ) %>%
      distinct(gsis_id, .keep_all = TRUE),
    by = c("merge_name", "pos", "season")
  ) %>%
  filter(
    !gsis_id %in% c("00-0029169")
  ) %>%
  mutate(
    gsis_id = case_when(
      fantasypros_id == "9579" ~ "00-0020337", # Carolina/Baltimore Steve Smith
      fantasypros_id == "9580" ~ "00-0025438", # Other Steve Smith
      fantasypros_id == "9636" ~ "00-0025425", # Oakland/Seattle Zach Miller
      TRUE ~ gsis_id
    )
  ) %>%
  distinct(fantasypros_id, .keep_all = TRUE) %>%
  select(player_name, pos, fantasypros_id, sportradar_id, gsis_id)

write.csv(fp_rankings %>% filter(!is.na(gsis_id)), "data-raw/supplemental_gsis_ids.csv")
write.csv(fp_rankings %>% filter(is.na(gsis_id)), "data-raw/missing_gsis_ids.csv")

janitor::get_dupes(fp_rankings, fantasypros_id, season) %>% View()
