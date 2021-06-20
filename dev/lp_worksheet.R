library(ffscrapr)
pkgload::load_all()

conn <- mfl_connect(2021, 54040)

scoring_history <- ffscrapr::ff_scoringhistory(conn, 2016:2020)

adp_outcomes <- .ff_adp_outcomes(scoring_history = scoring_history,
                                 injury_model = "simple")

latest_rankings <- .ff_latest_rankings()

rosters <- ffscrapr::ff_rosters(conn)

lineup_constraints <- ffscrapr::ff_starter_positions(conn)

projected_score <- rosters %>%
  dplyr::left_join(
    ffscrapr::dp_playerids() %>% dplyr::select("mfl_id","fantasypros_id"),
    by = c("player_id"="mfl_id")
  ) %>%
  dplyr::left_join(
    latest_rankings %>% dplyr::select("fantasypros_id", "ecr"),
    by = c("fantasypros_id")
  ) %>%
  dplyr::group_by(pos) %>%
  dplyr::mutate(rank = round(ecr)) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(
    adp_outcomes %>% dplyr::select("pos", "rank", "prob_gp", "week_outcomes"),
    by = c("pos","rank")
  ) %>%
  dplyr::select(
    # "franchise_id",
    "franchise_name",
    # "mfl_id"="player_id",
    # "fantasypros_id",
    "player_name",
    "pos",
    "team",
    "age",
    "ecr",
    "rank",
    "prob_gp",
    "week_outcomes"
  ) %>%
  dplyr::mutate(
    projection = purrr::map_if(week_outcomes,Negate(is.null), ~sample(.x, size = 1, replace = TRUE)),
    injury_model = purrr::map_if(prob_gp, Negate(is.null), ~rbinom(n = 1, size = 1, prob = .x)),
    prob_gp = NULL,
    week_outcomes = NULL
  ) %>%
  tidyr::unnest(c(projection,injury_model))

pikachu_scores <- projected_score %>%
  dplyr::filter(franchise_name == "Team Pikachu") %>%
  dplyr::transmute(
    franchise_name,
    pos,
    player_name,
    proj_score = projection * injury_model)

####
pak::pak(c("ROI", "ROI.plugin.glpk", "ROI.plugin.lpsolve", "ompr.roi"))
library(ompr)
library(ompr.roi)
library(ROI)
library(tidyverse)

set.seed(1)

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

#### DESIRED INPUT MATRIX
# QBs, RBs, WRs, TEs

player_ids <- pikachu_scores %>%
  select(player_name,pos) %>%
  group_by(pos) %>%
  mutate(row_id = row_number()) %>%
  ungroup() %>%
  pivot_wider(names_from = pos, values_from = player_name) %>%
  select(-row_id) %>%
  as.matrix()

player_scores <- pikachu_scores %>%
  select(proj_score,pos) %>%
  group_by(pos) %>%
  mutate(row_id = row_number()) %>%
  ungroup() %>%
  pivot_wider(names_from = pos, values_from = proj_score) %>%
  select(-row_id) %>%
  mutate_all(replace_na,0) %>%
  as.matrix()

#### ####
result <- MIPModel() %>%
  add_variable(
    x[i, j],
    i = seq_len(nrow(player_scores)),
    j = seq_len(ncol(player_scores)), type = "binary") %>%
  set_objective(
    sum_expr(
      player_scores[i, j] * x[i, j],
      i = seq_len(nrow(player_scores)),
      j = seq_len(ncol(player_scores)))) %>%
  add_constraint(
    sum_expr(x[i, j], j = seq_len(ncol(player_scores))) == 1,
    i = seq_len(nrow(player_scores))) %>%
  solve_model(with_ROI("glpk", verbose = TRUE))

get_solution(result, x[i, j]) %>%
  dplyr::filter(value == 1) %>%
  dplyr::mutate(
    player_id = map2(i,j,~player_ids[.x,.y])
  )

