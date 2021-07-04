#' Simulate Fantasy Seasons
#'
#' The main function of the package
#'
#' @param conn an connection to a league made with `ff_connect()` and friends (required)
#' @param n_seasons number of seasons to simulate, default = 100
#' @param weeks_per_season number of weeks per season, default = 14
#' @param best_ball logical: are weekly wins based on optimal lineups?
#' @param seed an integer to control reproducibility
#' @param injury_model select between "simple", "none"
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
#' basic <- ff_simulate(conn = conn, n_seasons = 100, weeks_per_season = 14, best_ball = FALSE)
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
                        best_ball = FALSE,
                        seed = NULL,
                        injury_model = c("simple", "none"),
                        base_seasons = 2012:2020,
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

  adp_outcomes <- ffs_adp_outcomes(scoring_history = scoring_history,
                                   injury_model = injury_model)

  #### DOWNLOAD LATEST FANTASYPROS RANKINGS ####

  latest_rankings <- ffs_latest_rankings()

  #### DOWNLOAD ROSTERS ####

  rosters <- ffscrapr::ff_rosters(conn)
  lineup_constraints <- ffscrapr::ff_starter_positions(conn)

  #### JOIN DATA ####

  preprocessed_data <- ffs_preprocess_data(conn, rosters, latest_rankings, adp_outcomes)

  #### GENERATE PREDICTIONS ####

  n_weeks <- n_seasons * weeks_per_season

  projected_scores <- ffs_generate_predictions(preprocessed_data, n_weeks)

  #### OPTIMIZE LINEUPS ####

  optimal_scores <- ffs_optimize_lineups(
    projected_scores = projected_scores,
    lineup_constraints = lineup_constraints,
    best_ball = best_ball,
    parallel = parallel)

  #### GENERATE SCHEDULES ####

  schedules <- ffs_build_schedules(n_teams = length(unique(rosters$franchise_id)),
                                  n_seasons = n_seasons,
                                  n_weeks = weeks_per_season)

  #### SUMMARISE SEASON ####

  summary_week <- ffs_summarise_week(optimal_scores, schedules)
  summary_season <- ffs_summarise_season(summary_week)
  summary_simulation <- ffs_summarise_simulation(summary_season)

  #### BUILD AND RETURN ####

  out <- list(
    summary_simulation = summary_simulation,
    summary_season = summary_season,
    summary_week = summary_week,
    latest_rankings = latest_rankings,
    raw_data = preprocessed_data
  )

  return(out)
}

