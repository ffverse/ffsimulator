
#' Generate Projections
#'
#' Run the bootstrapped resampling of player week outcomes on the latest rankings and rosters for a given number of seasons and weeks per season.
#'
#' Dataframe should contain ecr, prob_gp, week_outcomes, injury_model, franchise_id, pos, and bye
#'
#' @param adp_outcomes a dataframe of adp-based weekly outcomes, as created by `ffs_adp_outcomes()`
#' @param latest_rankings a dataframe of rankings, as created by `ffs_latest_rankings()`
#' @param rosters a dataframe of rosters, as created by `ffs_rosters()` - optional, reduces computation to just rostered players
#' @param n_seasons number of seasons
#' @param n_weeks weeks per season
#'
#' @return
#' @export
ffs_generate_projections <- function(adp_outcomes, latest_rankings, n_seasons, n_weeks, rosters = NULL){

  if(is.null(rosters))  rosters <- latest_rankings %>% dplyr::select("fantasypros_id")

  total_weeks <- n_seasons * n_weeks

  projected_score <- latest_rankings %>%
    dplyr::semi_join(rosters, by = "fantasypros_id") %>%
    dplyr::mutate(
      rank = purrr::map2(.data$ecr,
                        .data$sd,
                        ~ stats::rnorm(n = n_seasons, mean = .x, sd = .y/2) %>%
                          round() %>%
                          .replace_zero()),
      season = list(seq_len(n_seasons))
    ) %>%
    tidyr::unnest(c("rank","season")) %>%
    dplyr::inner_join(
      adp_outcomes %>% dplyr::select("pos","rank","prob_gp","week_outcomes"),
      by = c("pos","rank")) %>%
    dplyr::filter(!is.na(.data$ecr), !is.na(.data$prob_gp)) %>%
    dplyr::mutate(
      projection =
        purrr::map(.data$week_outcomes,
                   ~ sample(.x, size = n_weeks, replace = TRUE)),
      injury_model =
        purrr::map(.data$prob_gp,
                   ~ stats::rbinom(n = n_weeks, size = 1, prob = .x)),
      week = list(seq_len(n_weeks)),
      prob_gp = NULL,
      week_outcomes = NULL
    ) %>%
    tidyr::unnest(c("projection", "injury_model", "week")) %>%
    dplyr::arrange(.data$season,.data$week, .data$pos, .data$ecr) %>%
    dplyr::mutate(projected_score = .data$projection * .data$injury_model * (.data$week != .data$bye))

  return(projected_score)
}

.replace_zero <- function(x){replace(x,x==0,1)}
