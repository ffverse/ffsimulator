
library(ffscrapr)
pkgload::load_all()
library(tidyverse)
options(future.rng.onMisuse = "ignore")

# future::plan(multisession)

tictoc::tic()

conn <- mfl_connect(2021, 22627)

scoring_history <- ffscrapr::ff_scoringhistory(conn, 2016:2020)

adp_outcomes <- .ff_adp_outcomes(scoring_history = scoring_history,
                                 injury_model = "simple")

latest_rankings <- .ff_latest_rankings()

rosters <- ffscrapr::ff_rosters(conn)

lineup_constraints <- ffscrapr::ff_starter_positions(conn)

joined_data <- rosters %>%
  dplyr::filter(pos %in% c("QB","RB","WR","TE")) %>%
  dplyr::left_join(
    ffscrapr::dp_playerids() %>%
      dplyr::select("mfl_id","fantasypros_id"),
    by = c("player_id"="mfl_id")
  ) %>%
  dplyr::left_join(
    latest_rankings %>%
      dplyr::select("fantasypros_id", "ecr"),
    by = c("fantasypros_id")
  ) %>%
  dplyr::group_by(pos) %>%
  dplyr::mutate(rank = round(ecr)) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(
    adp_outcomes %>%
      dplyr::select("pos", "rank", "prob_gp", "week_outcomes"),
    by = c("pos","rank")
  ) %>%
  dplyr::select(
    "franchise_id",
    "franchise_name",
    "player_id",
    # "fantasypros_id",
    "player_name",
    "pos",
    "team",
    "age",
    "ecr",
    "rank",
    "prob_gp",
    "week_outcomes"
  )

n_weeks <- 1300
set.seed(613)

projected_score <- joined_data %>%
  dplyr::filter(!is.na(ecr)) %>%
  dplyr::mutate(
    projection = purrr::map(week_outcomes,
                            ~sample(.x, size = n_weeks, replace = TRUE)),
    injury_model = purrr::map(prob_gp,
                              ~rbinom(n = n_weeks, size = 1, prob = .x)),
    n = purrr::map(n_weeks, seq_len),
    prob_gp = NULL,
    week_outcomes = NULL
  ) %>%
  tidyr::unnest(c(projection,injury_model,n)) %>%
  dplyr::arrange(n, franchise_id, pos, ecr) %>%
  mutate(proj_score = projection * injury_model) %>%
  group_by(n,franchise_id,pos) %>%
  mutate(pos_rank = rank(desc(proj_score),ties.method = "random")) %>%
  ungroup() %>%
  left_join(lineup_constraints %>% select(pos,max),
            by = "pos") %>%
  filter(pos_rank <= max)

#
# franchise_scores <- projected_score %>%
#   dplyr::filter(franchise_name == "Team Pikachu", n == 1) %>%
#   dplyr::transmute(
#     franchise_id,
#     franchise_name,
#     pos,
#     player_id,
#     player_name,
#     proj_score = projection * injury_model)

####

# x <- .ff_optimize_lineups(pikachu_scores,lineup_constraints)


#### EXAMPLE MIP ####

# n <- 5

# weights <- matrix(rpois(n * n, 5), ncol = n, nrow = n)

# result <- MIPModel() %>%
#   add_variable(x[i, j], i = 1:n, j = 1:n, type = "binary") %>%
#   set_objective(sum_expr(weights[i, j] * x[i, j], i = 1:n, j = 1:n)) %>%
#   add_constraint(sum_expr(x[i, j], j = 1:n) == 1, i = 1:n) %>%
#   solve_model(with_ROI("glpk", verbose = TRUE))

# get_solution(result, x[i, j]) %>%
#   dplyr::filter(value == 1)


tictoc::tic()
optimal_scores <- projected_score %>%
  group_by(franchise_id, franchise_name, n) %>%
  nest() %>%
  ungroup() %>%
  mutate(
    optimals = map(data, ffsimulator::.ff_optimize_lineups, lineup_constraints)
  )
tictoc::toc()

optimal_scores <- optimal_scores %>%
  unnest_wider(optimals) %>%
  select(-data,-optimal_lineup) %>%
  group_by(n) %>%
  mutate(all_play_wins = rank(optimal_score)-1,
         all_play_pct = all_play_wins/(n()-1)) %>%
  ungroup()

tictoc::tic()
schedules <- ff_build_schedules(n_teams = 12, n_seasons = 100, n_weeks = 14)
tictoc::toc()

matchups <- schedules %>%
  group_by(team) %>%
  mutate(n = row_number()) %>%
  ungroup() %>%
  left_join(optimal_scores %>%
              mutate(franchise_id = as.integer(franchise_id)) %>%
              rename(team_score = optimal_score),
            by = c("team"="franchise_id", "n")) %>%
  left_join(optimal_scores %>%
              mutate(franchise_id = as.integer(franchise_id)) %>%
              select(opponent_score = optimal_score,
                     franchise_id,
                     opponent_name = franchise_name,
                     n),
            by = c("opponent"="franchise_id","n")
            ) %>%
  mutate(
    result = case_when(
      team_score > opponent_score ~ "W",
      team_score < opponent_score ~ "L",
      team_score == opponent_score ~ "T",
      TRUE ~ NA_character_
    )
  )

season_summaries <- matchups %>%
  group_by(season,franchise_name) %>%
  summarise(
    h2h_wins = sum(result == "W"),
    h2h_winpct = h2h_wins / n(),
    all_play_wins = sum(all_play_wins),
    all_play_pct = all_play_wins / (n() * 11)
  ) %>%
  ungroup()

tictoc::toc()

library(hrbrthemes)
library(ggridges)
library(stringi)
library(stringr)

hrbrthemes::import_plex_sans()

season_summaries %>%
  mutate(franchise_name = str_remove_all(franchise_name,"<[^>]*>"),
         franchise_name = stri_trans_general(franchise_name,"latin-ascii")) %>%
  group_by(franchise_name) %>%
  mutate(mean_h2hwinpct = mean(h2h_winpct)) %>%
  ungroup() %>%
  mutate(franchise_name = fct_reorder(franchise_name, mean_h2hwinpct)) %>%
  ggplot(aes(x = h2h_wins, y = franchise_name, fill = franchise_name)) +
  geom_density_ridges(
    color = "white",
    stat = "binline",
    alpha = 0.80,
    binwidth = 1
    # quantile_lines = TRUE
  ) +
  theme_modern_rc(base_family = "IBM Plex Sans") +
  theme(
    plot.title.position = "plot",
    legend.position = "none"
  ) +
  scale_x_continuous(limits = c(-1,15), n.breaks = 9) +
  xlab("Season Wins (13 game regular season)") +
  ylab(NULL) +
  labs(
    title = "FourEight Dynasty - Season Simulation Rankings",
    subtitle = "100 Simulated Seasons",
    caption = "@_TanHo | ffsimulator pkg"
  )

# library(hrbrthemes)
# library(ggridges)
#
# hrbrthemes::import_plex_sans()
#
# optimal_scores %>%
#   group_by(franchise_name) %>%
#   mutate(mean_ap = mean(all_play_wins)) %>%
#   ungroup() %>%
#   mutate(franchise_name = fct_reorder(franchise_name, mean_ap)) %>%
#   ggplot(aes(x = all_play_wins, y = franchise_name, fill = franchise_name)) +
#   geom_density_ridges(
#     color = "white",
#     stat = "binline",
#     alpha = 0.95,
#     binwidth = 1
#     # quantile_lines = TRUE
#   ) +
#   theme_modern_rc(base_family = "IBM Plex Sans") +
#   theme(
#     plot.title.position = "plot",
#     legend.position = "none"
#   ) +
#   xlab("All Play Wins Per Week") +
#   ylab(NULL) +
#   labs(
#     title = "SSB Dynasty League Rankings",
#     subtitle = "100 Simulated Weeks",
#     caption = "@_TanHo | ffsimulator pkg"
#   )
