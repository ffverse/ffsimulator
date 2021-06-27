#' Simulate Fantasy Seasons
#'
#' The main function of the package
#'
#' @param conn an connection to a league made with `ff_connect()` and friends (required)
#' @param n_seasons number of seasons to simulate, default = 100
#' @param weeks_per_season number of weeks per season, default = 17
#' @param best_ball Are weekly wins based on optimal score?
#' @param seed an integer to control reproducibility
#' @param custom_rankings a dataframe with x specification to pass in as latest fantasy rankings
#' @param injury_model select between "bimodal", "none" - later we'll have separate
#' @param owner_efficiency pass in a named list with average and sd
#' @param verbose print progress messages for debugging
#'
#' @examples \dontrun{
#'
#' conn <- mfl_connect(2021, 54040)
#'
#' auto <- ff_simulate(conn)
#'
#' reprex <- ff_simulate(conn = conn, seed = 613)
#'
#' basic <- ff_simulate(conn = conn, n_seasons = 100, weeks_per_season = 17, best_ball = FALSE)
#'
#' custom <- ff_simulate(
#'   conn = conn,
#'   n_seasons = 100,
#'   weeks_per_season = 17,
#'   custom_rankings = df_rankings,
#'   seed = 613,
#'   best_ball = FALSE,
#'   injury_model = c("bimodal", "separate", "none"),
#'   owner_efficiency = list(average = 0.75, sd = 0.025),
#'   verbose = FALSE
#' )
#' }
#'
#' @export

ff_simulate <- function(conn,
                        n_seasons = 100,
                        weeks_per_season = 14,
                        best_ball = TRUE,
                        seed = NULL,
                        custom_rankings = NULL,
                        injury_model = c("simple", "none"),
                        owner_efficiency = NULL,
                        base_seasons = 2016:2020,
                        parallel = FALSE,
                        verbose = TRUE
                        ){

  #### ASSERTIONS ####

  if(!class(conn) %in% c("mfl_conn","sleeper_conn","flea_conn","espn_conn")) {
    stop("conn should be a connection object created by `ff_connect()` and friends!",
         call. = FALSE)
  }

  injury_model <- match.arg(injury_model)
  checkmate::assert_numeric(base_seasons)
  checkmate::assert_int(n_seasons, lower = 1)
  checkmate::assert_int(weeks_per_season, lower = 1)
  checkmate::assert_int(seed, null.ok = TRUE)
  checkmate::assert_flag(best_ball)
  if(!is.null(seed)) set.seed(seed)

  # checkmate::assert_flag(verbose)
  if(!is.null(custom_rankings)) {
    checkmate::assert_data_frame(custom_rankings)
    ## ADD ASSERTIONS FOR CORRECT RANKINGS COLUMNS
    }
  if(!is.null(owner_efficiency)) checkmate::assert_list(owner_efficiency, names = c("average","sd"))


  #### DOWNLOAD SCORING HISTORY ####

  scoring_history <- ffscrapr::ff_scoringhistory(conn, base_seasons)

  #### CREATE ADP OUTCOMES ####

  adp_outcomes <- .ff_adp_outcomes(scoring_history = scoring_history,
                                   injury_model = injury_model)

  #### DOWNLOAD LATEST FANTASYPROS RANKINGS ####

  latest_rankings <- .ff_latest_rankings()

  #### DOWNLOAD ROSTERS ####

  rosters <- ffscrapr::ff_rosters(conn)
  lineup_constraints <- ffscrapr::ff_starter_positions(conn)

  #### JOIN DATA ####

  preprocessed_data <- .ff_join_data(conn, rosters, latest_rankings, adp_outcomes)

  #### GENERATE PREDICTIONS ####

  n_weeks <- n_seasons * weeks_per_season

  projected_scores <- .ff_generate_projections(preprocessed_data, n_weeks)

  #### OPTIMIZE LINEUPS ####

  optimal_scores <- .ff_optimize_lineups(projected_scores,
                                         lineup_constraints,
                                         best_ball,
                                         parallel)

  #### GENERATE SCHEDULES ####

  schedules <- .ff_build_schedules(n_teams = length(unique(rosters$franchise_id)),
                                  n_seasons = n_seasons,
                                  n_weeks = weeks_per_season)

  #### SUMMARISE SEASON ####

  season_summary <- .ff_summarise_season(optimal_scores, schedules)

  .ff_summarise_season <- function(optimal_scores, schedules){

    scores <- optimal_scores %>%
      dplyr::group_by(.data$n) %>%
      dplyr::mutate(
        all_play_wins = rank(-.data$actual_score),
        all_play_games = n()
      ) %>%
      dplyr::ungroup()

    matchups <- schedules %>%
      dplyr::group_by(.data$team) %>%
      dplyr::mutate(n = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(scores %>%
                  dplyr::mutate(franchise_id = as.integer(franchise_id)) %>%
                  dplyr::rename(team_score = actual_score),
                by = c("team"="franchise_id", "n")) %>%
      dplyr::left_join(scores %>%
                  dplyr::mutate(franchise_id = as.integer(franchise_id)) %>%
                  dplyr::select("opponent_score" = "actual_score",
                                "franchise_id",
                                "opponent_name" = "franchise_name",
                                "n"),
                by = c("opponent"="franchise_id","n")
      ) %>%
      dplyr::mutate(
        result = dplyr::case_when(
          team_score > opponent_score ~ "W",
          team_score < opponent_score ~ "L",
          team_score == opponent_score ~ "T",
          TRUE ~ NA_character_
        )
      )

    # TODO

  }

  #### BUILD AND RETURN ####

  # list(
  #   summary = season,
  #   matchups = matchups,
  #   lineups = lineups,
  #   latest_rankings = rankings,
  #   adp_outcomes = adp_outcomes,
  #   metadata = params
  # )

}

