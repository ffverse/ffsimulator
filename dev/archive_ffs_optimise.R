
ffs_score_rosters <- function(projected_scores, rosters) {
  checkmate::assert_data_frame(projected_scores)
  checkmate::assert_data_frame(rosters)

  assert_columns(
    projected_scores,
    c("fantasypros_id", "ecr", "rank", "projection",
      "injury_model", "season", "week",
      "projected_score", "scrape_date")
  )

  assert_columns(
    rosters,
    c("fantasypros_id", "league_id", "franchise_id", "pos")
  )

  roster_scores <- rosters %>%
    dplyr::inner_join(
      projected_scores %>%
        dplyr::select(
          "fantasypros_id", "ecr", "rank", "projection", "injury_model",
          "season", "week", "projected_score", "scrape_date"
        ),
      by = "fantasypros_id"
    ) %>%
    dplyr::arrange(-.data$projected_score) %>%
    dplyr::group_by(.data$league_id, .data$franchise_id, .data$pos, .data$season, .data$week) %>%
    dplyr::mutate(pos_rank = dplyr::row_number()) %>%
    dplyr::ungroup()

  return(roster_scores)
}

ffs_optimise_lineups_dplyr <- function(roster_scores,
                                       lineup_constraints,
                                       lineup_efficiency_mean = 0.775,
                                       lineup_efficiency_sd = 0.05,
                                       best_ball = FALSE,
                                       parallel = FALSE,
                                       pos_filter = c("QB","RB","WR","TE"),
                                       verbose = TRUE
) {
  checkmate::assert_number(lineup_efficiency_mean, lower = 0, upper = 1)
  checkmate::assert_number(lineup_efficiency_sd, lower = 0, upper = 0.25)
  checkmate::assert_flag(best_ball)
  checkmate::assert_flag(parallel)

  checkmate::assert_data_frame(roster_scores)
  assert_columns(roster_scores,
                 c("pos", "pos_rank", "league_id", "franchise_id",
                   "franchise_name", "season", "week", "projected_score"))

  checkmate::assert_data_frame(lineup_constraints, any.missing = FALSE)
  assert_columns(lineup_constraints, c("pos", "min", "max", "offense_starters"))

  if (!parallel) ffs_map <- purrr::map

  if (parallel && !requireNamespace("furrr", quietly = TRUE)) {
    stop("Package {furrr} is required to run `ffs_optimise_lineups()` in parallel.", call. = FALSE)
  }

  if (parallel && inherits(future::plan(), "sequential") ) {
    message("Parallel processing was specified but no future::plan() was found. Continuing sequentially.")

    ffs_map <- purrr::map
  }

  if (parallel && !inherits(future::plan(), "sequential")) {
    ffs_map <- furrr::future_map
  }

  lineup_constraints <- lineup_constraints %>%
    dplyr::filter(.data$pos %in% pos_filter)

  nest_data <- roster_scores %>%
    dplyr::left_join(
      lineup_constraints %>% dplyr::select("pos", "max"),
      by = "pos"
    ) %>%
    dplyr::filter(.data$pos_rank <= .data$max, .data$pos %in% lineup_constraints$pos) %>%
    dplyr::select("league_id","franchise_id","franchise_name","season","week","player_id","pos","projected_score") %>%
    tidyr::nest(data = c("player_id","pos","projected_score"))

  if (best_ball) lineup_efficiency <- 1

  if (!best_ball) {
    lineup_efficiency <- stats::rnorm(nrow(nest_data),
                                      mean = lineup_efficiency_mean,
                                      sd = lineup_efficiency_sd
    )
  }

  progress_function <- function() NULL
  if(verbose && requireNamespace("progressr", quietly = TRUE)) {
    progress_function <- progressr::progressor(steps = nrow(nest_data))
  }

  optimal_scores <- nest_data %>%
    dplyr::mutate(
      optimals = ffs_map(.data$data,
                         .ff_optimise_one_lineup_dplyr,
                         lineup_constraints,
                         progress_function),
      data = NULL
    ) %>%
    tidyr::unnest_wider("optimals") %>%
    dplyr::bind_cols(lineup_efficiency = lineup_efficiency) %>%
    dplyr::mutate(actual_score = .data$optimal_score * .data$lineup_efficiency)

  return(optimal_scores)
}
.ff_optimise_one_lineup_dplyr <- function(franchise_scores,
                                          lineup_constraints,
                                          progress_function
) {
  min_req <- sum(lineup_constraints$min)

  player_ids <- c(franchise_scores$player_id, rep_len(NA_character_, min_req))
  player_scores <- c(franchise_scores$projected_score, rep_len(0, min_req))
  player_scores[is.na(player_scores)] <- 0

  # binary - position identifiers

  pos_ids <- NULL

  for (i in lineup_constraints$pos) pos_ids <- c(pos_ids, as.integer(franchise_scores$pos == i), rep.int(1L, min_req))

  constraints_matrix <- matrix(
    c(
      pos_ids, # pos minimums
      pos_ids, # pos maximums
      as.integer(franchise_scores$pos %in% c("QB", "RB", "WR", "TE")), rep.int(1L, min_req)
    ), # total offensive starters
    nrow = nrow(lineup_constraints) * 2 + 1,
    byrow = TRUE
  )

  constraints_dir <- c(
    rep_len(">=", nrow(lineup_constraints)),
    rep_len("<=", nrow(lineup_constraints)),
    "<="
  )

  constraints_rhs <- c(
    lineup_constraints$min,
    lineup_constraints$max,
    lineup_constraints$offense_starters[[1]]
  )

  solve_lineup <- Rglpk::Rglpk_solve_LP(
    obj = player_scores,
    mat = constraints_matrix,
    dir = constraints_dir,
    rhs = constraints_rhs,
    types = rep("B", length(player_scores)),
    max = TRUE
  )

  optimals <- list(
    optimal_score = sum(player_scores * solve_lineup$solution),
    optimal_lineup = list(
      player_id = player_ids[as.logical(solve_lineup$solution)],
      player_score = player_scores[as.logical(solve_lineup$solution)]
    )
  )

  progress_function()

  return(optimals)
}
