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
#' conn <- mfl_connect(2020, 54040)
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

ff_simulate <- function(conn,
                        n_seasons = 100,
                        weeks_per_season = 17,
                        best_ball = FALSE,
                        seed = NULL,
                        custom_rankings = NULL,
                        injury_model = c("simple", "none"),
                        owner_efficiency = NULL,
                        base_seasons = 2016:2020#,
                        # verbose = TRUE
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
  # checkmate::assert_flag(verbose)
  if(!is.null(custom_rankings)) {
    checkmate::assert_data_frame(custom_rankings)
    ## ADD ASSERTIONS FOR CORRECT RANKINGS COLUMNS
    }
  if(!is.null(owner_efficiency)) checkmate::assert_list(owner_efficiency, names = c("average","sd"))


  #### Set Seed ####

  if(!is.null(seed)) set.seed(seed)

  #### DOWNLOAD SCORING HISTORY ####

  scoring_history <- ffscrapr::ff_scoringhistory(conn, base_seasons)

  #### CREATE ADP OUTCOMES ####

  adp_outcomes <- .ff_adp_outcomes(scoring_history)

  #### DOWNLOAD LATEST FANTASYPROS RANKINGS ####

  historical_rankings <- .ff_latest_rankings()

  #### DOWNLOAD ROSTERS ####

  rosters <- ffscrapr::ff_rosters()

  ####

}

#' Connects ff_scoringhistory to past rankings
.ff_adp_outcomes <- function(scoring_history){

  adp_outcomes <- ffsimulator::fp_rankings_history %>%
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
      games_played = dplyr::n()
    ) %>%
    dplyr::group_by(.data$pos, .data$rank) %>%
    dplyr::summarise(
      week_outcomes = list(c(unlist(.data$week_outcomes))),
      games_played = sum(.data$games_played, na.rm = TRUE),
      player_name = list(.data$player_name),
      fantasypros_id = list(.data$fantasypros_id)
    ) %>%
    dplyr::ungroup()


  return(adp_outcomes)

}
