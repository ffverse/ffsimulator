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

  #### JOIN DATA ####

  preprocessed_data <- .ff_join_data(conn, rosters, latest_rankings, adp_outcomes)

  #### GENERATE PREDICTIONS ####

}

#' Connects ff_scoringhistory to past rankings
#'
#' @return dataframe with position, rank, probability of games played, all week score outcomes
.ff_adp_outcomes <- function(scoring_history, injury_model){

  adp_outcomes <- ffsimulator::fp_rankings_history %>%
    dplyr::select(-"page_pos") %>%
    dplyr::left_join(
      ffscrapr::dp_playerids() %>%
        dplyr::select("fantasypros_id","gsis_id"),
      by = "fantasypros_id"
    ) %>%
    dplyr::filter(!is.na(.data$gsis_id), pos %in% c("QB","RB","WR","TE")) %>%
    .ff_apply_injury_model(injury_model) %>%
    dplyr::left_join(
      scoring_history %>%
        dplyr::filter(!is.na(.data$gsis_id), .data$week <= 17) %>%
        dplyr::select("season","gsis_id", "team", "points")
      , by = c("season","gsis_id")
    ) %>%
    dplyr::group_by(.data$season,
                    .data$pos,
                    .data$rank,
                    .data$prob_gp,
                    .data$fantasypros_id,
                    .data$player_name
                    ) %>%
    dplyr::summarise(
      week_outcomes = list(points),
      games_played = dplyr::n()
    ) %>%
    dplyr::group_by(.data$pos, .data$rank, .data$prob_gp) %>%
    dplyr::summarise(
      week_outcomes = list(c(unlist(.data$week_outcomes))),
      games_played = round(sum(.data$games_played, na.rm = TRUE)/length(unique(season)),2),
      games_missed = 16 - games_played,
      player_name = list(.data$player_name),
      fantasypros_id = list(.data$fantasypros_id)
    ) %>%
    dplyr::ungroup()

  return(adp_outcomes)
}

#' Applies various injury models to adp outcomes
#'
#' @keywords internal
.ff_apply_injury_model <- function(adp_outcomes, model_type){

  if(model_type == "none") {adp_outcomes$prob_gp <- 1}

  if(model_type == "simple") {
    adp_outcomes <- adp_outcomes %>%
      dplyr::left_join(ffsimulator::fp_injury_table, by = c("pos","rank"))
    }

  adp_outcomes
}

#' Download latest rankings from DynastyProcess GitHub
#'
#' Fetches a copy of FP data from DynastyProcess's data repository.
#'
#' If you have any issues with the output of this data, please open an issue in
#' the DynastyProcess data repository.
#'
#' @seealso <https://github.com/dynastyprocess/data>
#'
#' @export
.ff_latest_rankings <- function() {
  url_query <- "https://github.com/dynastyprocess/data/raw/master/files/db_fpecr_latest.rds"

  response <- httr::RETRY("GET", url_query)

  if (httr::http_error(response)) {
    stop(glue::glue("GitHub request failed with error: <{httr::status_code(response)}> \n
                    while calling <{url_query}>"), call. = FALSE)
  }

  fp_latest <- response %>%
    httr::content(as = "raw") %>%
    parse_raw_rds()

  fp_cleaned <- fp_latest %>%
    dplyr::filter(.data$ecr_type == "rp") %>%
    dplyr::select(
      "player",
      "fantasypros_id"="id",
      "pos",
      "team" = "tm",
      "ecr",
      "sd",
      "sportradar_id"="sportsdata_id",
      "scrape_date"
    )

  return(fp_cleaned)
}

#' Join all the data together ... maybe S3
#'
.ff_join_data <- function(conn, rosters, latest_rankings, adp_outcomes){
  UseMethod(".ff_join_data")
}

.ff_join_data.mfl_conn <- function(conn, rosters, latest_rankings, adp_outcomes){



}
.ff_join_data.sleeper_conn <- function(conn, rosters, latest_rankings, adp_outcomes){}
.ff_join_data.espn_conn <- function(conn, rosters, latest_rankings, adp_outcomes){}
.ff_join_data.flea_conn <- function(conn, rosters, latest_rankings, adp_outcomes){}

#' Optimize Lineups
#'
#' Optimizes lineups for one franchise week at a time. Use purrr or loop to do more franchises/weeks/seasons
#'
#' @param franchise_scores a data frame of scores for one week and one franchise
#' @param lineup_constraints a data frame as created by `ffscrapr::ff_starter_positions()`
#' @param roi_engine see ROI docs
#'
#' @return a list including the optimal_score and the optimal_lineup tibble.
#'
#' @export
.ff_optimize_lineups <- function(franchise_scores, lineup_constraints, roi_engine = "glpk"){

  if(roi_engine == "glpk") suppressMessages(requireNamespace("ROI.plugin.glpk"))
  if(roi_engine == "lpsolve") suppressMessages(requireNamespace("ROI.plugin.lpsolve"))

  player_ids <- franchise_scores %>%
    select(player_id,pos) %>%
    group_by(pos) %>%
    mutate(row_id = row_number()) %>%
    ungroup() %>%
    pivot_wider(names_from = pos, values_from = player_id) %>%
    select(-row_id) %>%
    as.matrix()

  player_scores <- franchise_scores %>%
    select(proj_score,pos) %>%
    group_by(pos) %>%
    mutate(row_id = row_number()) %>%
    ungroup() %>%
    pivot_wider(names_from = pos, values_from = proj_score) %>%
    select(-row_id) %>%
    mutate_all(replace_na,0) %>%
    as.matrix()

  constraints <- list(
    qb_min = lineup_constraints$min[lineup_constraints$pos == "QB"],
    rb_min = lineup_constraints$min[lineup_constraints$pos == "RB"],
    wr_min = lineup_constraints$min[lineup_constraints$pos == "WR"],
    te_min = lineup_constraints$min[lineup_constraints$pos == "TE"],
    qb_max = lineup_constraints$max[lineup_constraints$pos == "QB"],
    rb_max = lineup_constraints$max[lineup_constraints$pos == "RB"],
    wr_max = lineup_constraints$max[lineup_constraints$pos == "WR"],
    te_max = lineup_constraints$max[lineup_constraints$pos == "TE"],
    total_offense = lineup_constraints$offense_starters[[1]]
  )

  model_result <- MIPModel() %>%
    add_variable(x[i, pos],
                 i = seq_len(nrow(player_scores)),
                 pos = seq_len(ncol(player_scores)), type = "binary") %>%
    set_objective(
      sum_expr(
        player_scores[i, pos] * x[i, pos],
        i = seq_len(nrow(player_scores)),
        pos = seq_len(ncol(player_scores)))) %>%
    add_constraint(
      sum_expr(x[i, pos], i = seq_len(nrow(player_scores))) >= constraints$qb_min,
      pos = 1
    ) %>%
    add_constraint(
      sum_expr(x[i, pos], i = seq_len(nrow(player_scores))) >= constraints$rb_min,
      pos = 2
    ) %>%
    add_constraint(
      sum_expr(x[i, pos], i = seq_len(nrow(player_scores))) >= constraints$wr_min,
      pos = 3
    ) %>%
    add_constraint(
      sum_expr(x[i, pos], i = seq_len(nrow(player_scores))) >= constraints$te_min,
      pos = 4
    ) %>%
    add_constraint(
      sum_expr(x[i, pos], i = seq_len(nrow(player_scores))) <= constraints$qb_max,
      pos = 1
    ) %>%
    add_constraint(
      sum_expr(x[i, pos], i = seq_len(nrow(player_scores))) <= constraints$rb_max,
      pos = 2
    ) %>%
    add_constraint(
      sum_expr(x[i, pos], i = seq_len(nrow(player_scores))) <= constraints$wr_max,
      pos = 3
    ) %>%
    add_constraint(
      sum_expr(x[i, pos], i = seq_len(nrow(player_scores))) <= constraints$te_max,
      pos = 4
    ) %>%
    add_constraint(
      sum_expr(x[i, pos],
               i = seq_len(nrow(player_scores)),
               pos = seq_len(4)) <= constraints$total_offense
    ) %>%
    solve_model(with_ROI(roi_engine))

  optimal_lineup <- get_solution(model_result, x[i, j]) %>%
    dplyr::filter(value == 1) %>%
    dplyr::transmute(
      player_id = map2_chr(i, j, ~player_ids[.x,.y]),
      player_score = map2_dbl(i, j, ~player_scores[.x,.y])
    ) %>%
    dplyr::inner_join(x = franchise_scores %>% dplyr::select("player_id","player_name","pos"),
                      by = c("player_id"))

  return(
    list(
      optimal_score = sum(optimal_lineup$player_score, na.rm = TRUE),
      optimal_lineup = optimal_lineup
    )
  )
}
