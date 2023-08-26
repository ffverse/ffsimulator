#' Generate Projections
#'
#' Runs the bootstrapped resampling of player week outcomes on the latest rankings and rosters for a given number of seasons and weeks per season.
#'
#' @param adp_outcomes a dataframe of adp-based weekly outcomes, as created by `ffs_adp_outcomes()`
#' @param latest_rankings a dataframe of rankings, as created by `ffs_latest_rankings()`
#' @param n number of weeks to simulate
#' @param rosters a dataframe of rosters, as created by `ffs_rosters()` - optional, reduces computation to just rostered players
#'
#' @examples \donttest{
#' # cached examples
#' adp_outcomes_week <- .ffs_cache_example("adp_outcomes_week.rds")
#' latest_rankings_week <- .ffs_cache_example("latest_rankings_week.rds")
#'
#' ffs_generate_projections_week(adp_outcomes_week, latest_rankings_week)
#' }
#'
#' @seealso vignette("custom") for example usage
#'
#' @return a dataframe of weekly scores for each player in the simulation, approximately of length n_seasons x n_weeks x latest_rankings
#' @export
ffs_generate_projections_week <- function(adp_outcomes,
                                          latest_rankings,
                                          n = 1000,
                                          rosters = NULL
) {
  checkmate::assert_number(n, lower = 1)

  checkmate::assert_data_frame(adp_outcomes)
  assert_columns(adp_outcomes, c("pos", "rank", "week_outcomes"))
  adp_outcomes <- data.table::as.data.table(adp_outcomes)[, c("pos", "rank", "week_outcomes")]

  checkmate::assert_data_frame(latest_rankings)
  assert_columns(latest_rankings, c("player", "pos", "team",
                                    "ecr", "sd", "fantasypros_id",
                                    "scrape_date"))

  latest_rankings <- data.table::as.data.table(latest_rankings)[, c("player", "pos", "team", "ecr", "sd", "fantasypros_id","scrape_date")]

  if (is.null(rosters)) rosters <- latest_rankings[, "fantasypros_id"]
  checkmate::assert_data_frame(rosters)
  assert_columns(rosters, "fantasypros_id")
  rosters <- data.table::as.data.table(rosters)

  rankings <- latest_rankings[latest_rankings$fantasypros_id %in% rosters$fantasypros_id]

  rankings <- rankings[,
                       list(
                         scrape_date = rep(.SD$scrape_date),
                         player = rep(.SD$player),
                         pos = rep(.SD$pos),
                         team = rep(.SD$team),
                         ecr = rep(.SD$ecr),
                         sd = rep(.SD$sd),
                         week = seq_len(n),
                         rank = stats::rnorm(n = n, mean = .SD$ecr, sd = .SD$sd / 2) %>%
                           round() %>%
                           .replace_zero()
                       ),
                       by = "fantasypros_id"
  ]

  ps <- merge(rankings, adp_outcomes, by = c("pos", "rank"))
  ps <- ps[!is.na(ps$ecr)][
    ,
    list(
      projected_score = as.numeric(sample(x = .SD$week_outcomes[[1]],
                                          size = 1, replace = TRUE))
    ),
    by = c("week", "fantasypros_id", "player", "pos",
           "team", "ecr", "sd", "rank","scrape_date"),
    .SDcols = c("week_outcomes")
  ]
  ps <- ps[,`:=`(
    projection = ps$projected_score,
    gp_model = 1,
    season = 1
  )]

  return(ps)
}

.replace_zero <- function(x) {
  replace(x, x == 0, 1)
}
