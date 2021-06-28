#' Optimize Lineups
#'
#' Optimizes lineups for all franchises in the dataframe
#'
.ff_optimize_lineups <- function(projected_scores,
                                 lineup_constraints,
                                 best_ball = TRUE,
                                 parallel = FALSE){

  stopifnot(is.logical(parallel))
  stopifnot(is.logical(best_ball))

  if(!parallel) map <- purrr::map

  if(parallel && requireNamespace("furrr")) {
    map <- furrr::future_map

    if(inherits(future::plan(), "sequential")) {
      message("Parallel processing was specified but no future::plan() was found. Continuing sequentially.")
    }
  }

  nest_data <- projected_scores %>%
    dplyr::left_join(
      lineup_constraints %>% dplyr::select("pos", "max"),
      by = "pos") %>%
    dplyr::filter(.data$pos_rank <= .data$max) %>%
    dplyr::group_by(.data$franchise_id, .data$franchise_name, .data$n) %>%
    tidyr::nest() %>%
    dplyr::ungroup()

  if(best_ball) lineup_efficiency <- 1

  if(!best_ball) lineup_efficiency <- rnorm(nrow(nest_data), mean = 0.775, sd = 0.05)

  optimal_scores <- nest_data %>%
    dplyr::mutate(
      optimals = map(.data$data, ffsimulator::.ff_optimize_one_lineup, lineup_constraints),
      data = NULL
    ) %>%
    tidyr::unnest_wider("optimals") %>%
    dplyr::bind_cols(lineup_efficiency = lineup_efficiency) %>%
    dplyr::mutate(actual_score = optimal_score * lineup_efficiency)

  return(optimal_scores)
}

#' Optimize single lineup
#'
#' Optimizes lineups for one franchise week at a time. Use purrr or loop to do more franchises/weeks/seasons
#'
#' @param franchise_scores a data frame of scores for one week and one franchise
#' @param lineup_constraints a data frame as created by `ffscrapr::ff_starter_positions()`
#'
#' @return a list including the optimal_score and the optimal_lineup tibble.
#'
#' @export
.ff_optimize_one_lineup <- function(franchise_scores, lineup_constraints){

  score_obj <- tidyr::replace_na(franchise_scores$projected_score,0)

  # binary - position identifiers

  pos_qb <- as.integer(franchise_scores$pos=="QB")
  pos_rb <- as.integer(franchise_scores$pos=="RB")
  pos_wr <- as.integer(franchise_scores$pos=="WR")
  pos_te <- as.integer(franchise_scores$pos=="TE")
  pos_off <- rep(1,length.out = length(score_obj))

  constraints_matrix <- matrix(
    c(pos_qb,pos_rb,pos_wr,pos_te, #pos minimums
      pos_qb,pos_rb,pos_wr,pos_te, #pos maximums
      pos_off), # total offensive starters
    nrow = 9,
    byrow = TRUE
  )

  constraints_dir <- c(rep(">=",4), #pos minimums
                       rep("<=",4), #pos maximums
                       "<=") # total offensive starters

  constraints_rhs <- c(
    lineup_constraints$min[lineup_constraints$pos == "QB"],
    lineup_constraints$min[lineup_constraints$pos == "RB"],
    lineup_constraints$min[lineup_constraints$pos == "WR"],
    lineup_constraints$min[lineup_constraints$pos == "TE"],
    lineup_constraints$max[lineup_constraints$pos == "QB"],
    lineup_constraints$max[lineup_constraints$pos == "RB"],
    lineup_constraints$max[lineup_constraints$pos == "WR"],
    lineup_constraints$max[lineup_constraints$pos == "TE"],
    lineup_constraints$offense_starters[[1]]
  )

  solve_lineup <- Rglpk::Rglpk_solve_LP(
    obj = score_obj,
    mat = constraints_matrix,
    dir = constraints_dir,
    rhs = constraints_rhs,
    types = rep("B", length(score_obj)),
    max = TRUE
  )

  optimals <- list(
    optimal_score = sum(franchise_scores$projected_score * solve_lineup$solution),
    optimal_lineup = data.frame(
      player_id = franchise_scores$player_id[as.logical(solve_lineup$solution)],
      player_score = franchise_scores$projected_score[as.logical(solve_lineup$solution)]
    ))

  return(optimals)
}
