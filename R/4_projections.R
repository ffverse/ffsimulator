
#' Generate Projections
#'
#' Runs the bootstrapped resampling of player week outcomes on the latest rankings and rosters for a given number of seasons and weeks per season.
#'
#' @param adp_outcomes a dataframe of adp-based weekly outcomes, as created by `ffs_adp_outcomes()`
#' @param latest_rankings a dataframe of rankings, as created by `ffs_latest_rankings()`
#' @param rosters a dataframe of rosters, as created by `ffs_rosters()` - optional, reduces computation to just rostered players
#' @param n_seasons number of seasons, default is 100
#' @param n_weeks weeks per season, default is 14
#'
#' @examples \donttest{
#' # cached examples
#' adp_outcomes <- .ffs_cache("adp_outcomes.rds")
#' latest_rankings <- .ffs_cache("latest_rankings.rds")
#'
#' ffs_generate_projections(adp_outcomes, latest_rankings)
#' }
#'
#' @seealso vignette("Custom Simulations") for example usage
#'
#' @return a dataframe of weekly scores for each player in the simulation, approximately of length n_seasons x n_weeks x latest_rankings
#' @export
ffs_generate_projections <- function(adp_outcomes, latest_rankings, n_seasons = 100, n_weeks = 14, rosters = NULL) {
  checkmate::assert_number(n_seasons, lower = 1)
  checkmate::assert_number(n_weeks, lower = 1)

  checkmate::assert_data_frame(adp_outcomes)
  assert_columns(adp_outcomes, c("pos", "rank", "prob_gp", "week_outcomes"))
  adp_outcomes <- data.table::as.data.table(adp_outcomes)[, c("pos", "rank", "prob_gp", "week_outcomes")]

  checkmate::assert_data_frame(latest_rankings)
  assert_columns(latest_rankings, c("player", "pos", "team", "ecr", "sd", "bye", "fantasypros_id","scrape_date"))
  latest_rankings <- data.table::as.data.table(latest_rankings)[, c("player", "pos", "team", "ecr", "sd", "bye", "fantasypros_id","scrape_date")]

  if (is.null(rosters)) rosters <- latest_rankings[, "fantasypros_id"]
  checkmate::assert_data_frame(rosters)
  assert_columns(rosters, "fantasypros_id")
  rosters <- data.table::as.data.table(rosters)

  total_weeks <- n_seasons * n_weeks

  rankings <- latest_rankings[latest_rankings$fantasypros_id %in% rosters$fantasypros_id]

  rankings <- rankings[,
    list(
      scrape_date = rep(.SD$scrape_date),
      player = rep(.SD$player),
      pos = rep(.SD$pos),
      team = rep(.SD$team),
      bye = rep(.SD$bye),
      ecr = rep(.SD$ecr),
      sd = rep(.SD$sd),
      season = seq_len(n_seasons),
      rank = stats::rnorm(n = n_seasons, mean = .SD$ecr, sd = .SD$sd / 2) %>%
        round() %>%
        .replace_zero()
    ),
    by = "fantasypros_id"
  ]

  ps <- merge(rankings, adp_outcomes, by = c("pos", "rank"))
  ps <- ps[!is.na(ps$ecr) & !is.na(ps$prob_gp)][
    ,
    list(
      week = seq_len(n_weeks),
      projection = as.numeric(sample(x = .SD$week_outcomes[[1]], size = n_weeks, replace = TRUE)),
      injury_model = stats::rbinom(n = n_weeks, size = 1, prob = .SD$prob_gp)
    ),
    by = c("season", "fantasypros_id", "player", "pos", "team", "bye", "ecr", "sd", "rank","scrape_date"),
    .SDcols = c("week_outcomes", "prob_gp")
  ]

  ps <- ps[
    order(ps$season, ps$week, ps$pos, ps$ecr),
    `:=`(
      projected_score = ps$projection * ps$injury_model * (ps$week != ps$bye)
    )
  ]

  return(ps)
}

.replace_zero <- function(x) {
  replace(x, x == 0, 1)
}
