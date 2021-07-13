
#' Generate Predictions
#'
#' Run the bootstrapped resampling of player week outcomes on preprocessed dataframes for a given number of seasons and weeks per season.
#'
#' Dataframe should contain ecr, prob_gp, week_outcomes, injury_model, franchise_id, pos, and bye
#'
#' @param preprocessed_data a dataframe as created by `ffs_preprocess_data`
#' @param n_seasons number of seasons
#' @param n_weeks weeks per season
#'
#' @return
#' @export
ffs_generate_predictions <- function(preprocessed_data, n_seasons, n_weeks){

  total_weeks <- n_seasons * n_weeks

  projected_score <- preprocessed_data %>%
    dplyr::filter(!is.na(.data$ecr), !is.na(.data$prob_gp)) %>%
    dplyr::mutate(
      projection =
        purrr::map(.data$week_outcomes,
                   ~ sample(.x, size = total_weeks, replace = TRUE)),
      injury_model =
        purrr::map(.data$prob_gp,
                   ~ stats::rbinom(n = total_weeks, size = 1, prob = .x)),
      season = list(sort(rep_len(seq_len(n_seasons),total_weeks))),
      week = list(rep_len(seq_len(n_weeks),total_weeks)),
      prob_gp = NULL,
      week_outcomes = NULL
    ) %>%
    tidyr::unnest(c("projection", "injury_model", "season", "week")) %>%
    dplyr::arrange(.data$season,.data$week, .data$franchise_id, .data$pos, .data$ecr) %>%
    dplyr::mutate(projected_score = .data$projection * .data$injury_model * (.data$week != .data$bye)) %>%
    dplyr::group_by(.data$season, .data$week, .data$franchise_id, .data$pos) %>%
    dplyr::mutate(pos_rank = rank(-.data$projected_score,
                                  ties.method = "random")) %>%
    dplyr::ungroup()

  return(projected_score)
}


