library(fpscrapr) # https://fpscrapr.dynastyprocess.com
library(tidyverse)
library(ffscrapr)

seasons <- 2016:2020

pages <- c("qb-cheatsheets",
           "ppr-rb-cheatsheets",
           "ppr-wr-cheatsheets",
           "ppr-te-cheatsheets",
           "k-cheatsheets",
           "dst-cheatsheets",
           "dl-cheatsheets",
           "lb-cheatsheets",
           "db-cheatsheets"
           )

fp_rankings_history <- crossing(pages,seasons) %>%
  mutate(rankings = map2(pages,seasons, ~fp_rankings(page = .x, year = .y))) %>%
  unnest(rankings) %>%
  transmute(
    page_pos =
      str_remove_all(pages,"cheatsheets|^ppr|\\-") %>%
      toupper() %>%
      str_squish(),
    season = seasons,
    fantasypros_id = as.character(player_id),
    player_name = dp_cleannames(player_name),
    pos = case_when(player_position_id %in% c("CB","S") ~ "DB",
                    player_position_id %in% c("OLB","LB") ~ "LB",
                    player_position_id %in% c("DE","DT","NT") ~ "DL",
                    TRUE ~ player_position_id),
    rank = rank_ecr,
    ecr = as.numeric(rank_ave),
    sd = as.numeric(rank_std)
  ) %>%
  filter(page_pos == pos)

usethis::use_data(fp_rankings_history, overwrite = TRUE)
