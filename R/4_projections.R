
.ff_generate_projections <- function(preprocessed_data, n_weeks){

  projected_score <- preprocessed_data %>%
    dplyr::filter(!is.na(.data$ecr)) %>%
    dplyr::mutate(
      projection =
        purrr::map(.data$week_outcomes,
                   ~ sample(.x, size = n_weeks, replace = TRUE)),
      injury_model =
        purrr::map(.data$prob_gp,
                   ~ rbinom(n = n_weeks, size = 1, prob = .x)),
      n = purrr::map(n_weeks, seq_len),
      prob_gp = NULL,
      week_outcomes = NULL
    ) %>%
    tidyr::unnest(c("projection", "injury_model", "n")) %>%
    dplyr::arrange(.data$n, .data$franchise_id, .data$pos, .data$ecr) %>%
    dplyr::mutate(projected_score = .data$projection * .data$injury_model) %>%
    dplyr::group_by(.data$n,.data$franchise_id,.data$pos) %>%
    dplyr::mutate(pos_rank = rank(-.data$projected_score,
                                  ties.method = "random")) %>%
    dplyr::ungroup()

  return(projected_score)
}
