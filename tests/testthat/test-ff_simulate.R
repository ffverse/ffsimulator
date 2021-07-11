
cache <- tibble::tibble(file = list.files("cache",full.names = TRUE)) %>%
  dplyr::transmute(
    data = purrr::map(.data$file,readRDS),
    name = stringr::str_remove_all(.data$file,"^cache/|\\.rds$")
    ) %>%
  dplyr::select("name","data") %>%
  tibble::deframe()

#### Test the functions below ####

test_that("ffs_adp_outcomes() works for both the simple and none injury models",{
  adp_outcomes <- ffs_adp_outcomes(scoring_history = cache$mfl_scoring_history,
                                   injury_model = "simple")

  adp_outcomes_noinjury <- ffs_adp_outcomes(scoring_history = cache$espn_scoring_history,
                                            injury_model = "none")

  checkmate::expect_tibble(adp_outcomes, min.rows = 500)
  checkmate::expect_tibble(adp_outcomes_noinjury, min.rows = 500)

  checkmate::expect_subset(
    names(adp_outcomes),
    c("pos","rank","prob_gp","week_outcomes","player_name","fantasypros_id"))
  checkmate::expect_subset(
    names(adp_outcomes_noinjury),
    c("pos","rank","prob_gp","week_outcomes","player_name","fantasypros_id"))

  cache$adp_outcomes <<- adp_outcomes

})

test_that("preprocessing works for the snapshotted data from each platform",{

  preprocessed_data <-
    ffs_preprocess_data(conn = cache$mfl_conn,
                        rosters = cache$mfl_rosters,
                        latest_rankings =  cache$latest_rankings,
                        adp_outcomes = cache$adp_outcomes)

  preprocessed_data_sleeper <-
    ffs_preprocess_data(conn = cache$sleeper_conn,
                        rosters = cache$sleeper_rosters,
                        latest_rankings =  cache$latest_rankings,
                        adp_outcomes = cache$adp_outcomes)

  preprocessed_data_espn <-
    ffs_preprocess_data(conn = cache$espn_conn,
                        rosters = cache$espn_rosters,
                        latest_rankings =  cache$latest_rankings,
                        adp_outcomes = cache$adp_outcomes)

  preprocessed_data_fleaflicker <-
    ffs_preprocess_data(conn = cache$fleaflicker_conn,
                        rosters = cache$fleaflicker_rosters,
                        latest_rankings =  cache$latest_rankings,
                        adp_outcomes = cache$adp_outcomes)

  checkmate::expect_tibble(preprocessed_data, min.rows = 350)
  checkmate::expect_tibble(preprocessed_data_sleeper, min.rows = 300)
  checkmate::expect_tibble(preprocessed_data_espn, min.rows = 150)
  checkmate::expect_tibble(preprocessed_data_fleaflicker, min.rows = 300)

  checkmate::expect_subset(
    c("franchise_id","player_id","pos","prob_gp","week_outcomes"),
    names(preprocessed_data))
  checkmate::expect_subset(
    c("franchise_id","player_id","pos","prob_gp","week_outcomes"),
    names(preprocessed_data_espn)
    )
  checkmate::expect_subset(
    c("franchise_id","player_id","pos","prob_gp","week_outcomes"),
    names(preprocessed_data_fleaflicker)
    )
  checkmate::expect_subset(
    c("franchise_id","player_id","pos","prob_gp","week_outcomes"),
    names(preprocessed_data_sleeper)
    )

  cache$preprocessed_data <<- preprocessed_data
})

test_that("ffs_generate_predictions() returns a tibble and specific columns",{

  projected_scores <- ffs_generate_predictions(
    preprocessed_data = cache$preprocessed_data,
    n_weeks =  20)

  checkmate::expect_tibble(projected_scores, min.rows = 3500)

  checkmate::expect_subset(
    c("franchise_id","player_id","pos","projected_score","pos_rank","n"),
    names(projected_scores)
  )

  cache$projected_scores <<- projected_scores

})

test_that("ffs_optimize_lineups() returns a tibble and specific columns", {

  future::plan("sequential")

  optimal_scores <- ffs_optimize_lineups(
    projected_scores = cache$projected_scores,
    lineup_constraints = cache$mfl_lineup_constraints,
    best_ball = FALSE,
    parallel = FALSE)

  optimal_scores_parallel_bestball <- ffs_optimize_lineups(
    projected_scores = cache$projected_scores,
    lineup_constraints = cache$mfl_lineup_constraints,
    best_ball = TRUE,
    parallel = TRUE)

  checkmate::expect_tibble(optimal_scores, min.rows = 120)
  checkmate::expect_tibble(optimal_scores_parallel_bestball, min.rows = 120)

  checkmate::expect_subset(
    c("franchise_id","franchise_name","n","optimal_score","optimal_lineup","n","lineup_efficiency","actual_score"),
    names(optimal_scores)
  )
  checkmate::expect_subset(
    c("franchise_id","franchise_name","n","optimal_score","optimal_lineup","n","lineup_efficiency","actual_score"),
    names(optimal_scores_parallel_bestball)
  )

  expect_equal(optimal_scores_parallel_bestball$optimal_score,
               optimal_scores_parallel_bestball$actual_score)

  cache$optimal_scores <<- optimal_scores

})

test_that("schedules returns a tibble and specific columns",{

  schedules <- ffs_build_schedules(n_teams =
                                     cache$mfl_rosters$franchise_id %>%
                                     unique() %>%
                                     length(),
                                   n_seasons = 2,
                                   n_weeks = 10)

  checkmate::expect_tibble(schedules, nrows = 240)

  checkmate::expect_subset(
    c("season","week","team","opponent"),
    names(schedules)
  )

  cache$schedules <<- schedules
})

test_that("summary functions return tibbles", {

  summary_week <- ffs_summarise_week(optimal_scores = cache$optimal_scores,
                                     schedules =  cache$schedules)
  summary_season <- ffs_summarise_season(summary_week = summary_week)
  summary_simulation <- ffs_summarise_simulation(summary_season = summary_season)

  checkmate::expect_tibble(summary_week, nrows = 240)
  checkmate::expect_tibble(summary_season, nrows = 24)
  checkmate::expect_tibble(summary_simulation, nrows = 12)

  checkmate::expect_subset(
    c("season", "franchise_id", "franchise_name", "h2h_wins", "h2h_winpct",
      "allplay_wins", "allplay_games", "allplay_winpct", "points_for",
      "points_against", "potential_points"),
    names(summary_season),
    label = "summary_season names check"
  )
  checkmate::expect_subset(
    c("n", "season", "season_week", "franchise_name", "optimal_score",
      "lineup_efficiency", "team_score", "opponent_score", "result",
      "opponent_name", "allplay_wins", "allplay_games", "allplay_pct",
      "franchise_id", "optimal_lineup"),
    names(summary_week),
    label = "summary_week names check")
  checkmate::expect_subset(
    c("franchise_id", "franchise_name", "seasons", "h2h_wins", "h2h_winpct",
      "allplay_wins", "allplay_winpct", "points_for", "points_against",
      "potential_points"),
    names(summary_simulation),
    label = "summary_simulation names check"
  )
})

