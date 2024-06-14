#' Generate Projections
#'
#' Runs the bootstrapped resampling of player week outcomes on the latest rankings and rosters for a given number of seasons and weeks per season.
#'
#' @param adp_outcomes a dataframe of adp-based weekly outcomes, as created by `ffs_adp_outcomes()`
#' @param latest_rankings a dataframe of rankings, as created by `ffs_latest_rankings()`
#' @param rosters a dataframe of rosters, as created by `ffs_rosters()` - optional, reduces computation to just rostered players
#' @param n_seasons number of seasons, default is 100
#' @param weeks a numeric vector of weeks to simulate, defaults to 1:14
#'
#' @examples \donttest{
#' # cached examples
#' adp_outcomes <- .ffs_cache_example("adp_outcomes.rds")
#' latest_rankings <- .ffs_cache_example("latest_rankings.rds")
#'
#' ffs_generate_projections(adp_outcomes, latest_rankings)
#' }
#'
#' @seealso vignette("custom") for example usage
#'
#' @return a dataframe of weekly scores for each player in the simulation, approximately of length n_seasons x n_weeks x latest_rankings
#' @export
ffs_generate_projections <- function(adp_outcomes,
                                     latest_rankings,
                                     n_seasons = 100,
                                     weeks = 1:14,
                                     version = c("v2","v1"),
                                     rosters = NULL
) {
  version <- rlang::arg_match0(version,values = c("v2","v1"))
  checkmate::assert_number(n_seasons, lower = 1)
  checkmate::assert_numeric(weeks, lower = 1, min.len = 1)
  assert_df(adp_outcomes, c("pos", "rank", "prob_gp", "week_outcomes"))
  assert_df(latest_rankings, c("player", "pos", "team", "ecr", "sd", "bye", "fantasypros_id", "scrape_date"))

  weeks <- unique(weeks)
  n_weeks <- length(weeks)

  adp_outcomes <- data.table::as.data.table(adp_outcomes)[
    , c("pos", "rank", "avg_week", "prob_gp", "week_outcomes")
  ]
  latest_rankings <- data.table::as.data.table(latest_rankings)[
    , c("player", "pos", "team", "ecr", "sd", "bye", "fantasypros_id", "scrape_date")
  ]

  if (is.null(rosters)) rosters <- latest_rankings[, "fantasypros_id"]
  assert_df(rosters, "fantasypros_id")
  rosters <- data.table::as.data.table(rosters)

  .ffs_projections <- switch(
    version,
    "v2" = .ffs_projections_v2,
    "v1" = .ffs_projections_v1
  )

  ps <- .ffs_projections(
    adp_outcomes = adp_outcomes,
    latest_rankings = latest_rankings,
    n_seasons = n_seasons,
    weeks = weeks,
    n_weeks = n_weeks,
    rosters = rosters
  )

  return(ps)
}

.ffs_projections_v1 <- function(adp_outcomes, latest_rankings, n_seasons, weeks, n_weeks, rosters){

  rankings <- latest_rankings[
    latest_rankings$fantasypros_id %in% rosters$fantasypros_id
  ][
    ,
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
  ps <- ps[
    !is.na(ps$ecr) & !is.na(ps$prob_gp)
  ][
    , list(
      week = weeks,
      projection = as.numeric(sample(x = .SD$week_outcomes[[1]], size = n_weeks, replace = TRUE)),
      gp_model = stats::rbinom(n = n_weeks, size = 1, prob = .SD$prob_gp)
    ),
    by = c("season", "fantasypros_id", "player", "pos", "team", "bye", "ecr", "sd", "rank", "scrape_date"),
    .SDcols = c("week_outcomes", "prob_gp")
  ]

  ps <- ps[
    ,
    `:=`(
      projected_score = ps$projection * ps$gp_model * (ps$week != ps$bye)
    )
  ]
  return(ps[])
}

.ffs_projections_v2 <- function(adp_outcomes, latest_rankings, n_seasons, weeks, n_weeks, rosters){

  draft_rankings <- latest_rankings[
    latest_rankings$fantasypros_id %in% rosters$fantasypros_id
  ][
    ,
    list(
      scrape_date = rep(.SD$scrape_date),
      player = rep(.SD$player),
      pos = rep(.SD$pos),
      team = rep(.SD$team),
      bye = rep(.SD$bye),
      ecr = rep(.SD$ecr),
      sd = rep(.SD$sd),
      season = seq_len(n_seasons),
      draft_rank = .replace_zero(round(stats::rnorm(n = n_seasons, mean = .SD$ecr, sd = .SD$sd / 2)))
    ),
    by = "fantasypros_id"
  ]

  week_ranks <- merge(
    draft_rankings,
    .ffs_draft_to_week()[,c("pos", "draft_rank", "week_rank")],
    by = c("pos", "draft_rank"),
    all.x = TRUE
  )[
    !is.na(ecr)
  ][
    , list(
      week = weeks,
      week_rank = as.numeric(sample(x = .SD$week_rank[[1]], size = n_weeks, replace = TRUE))
    ),
    by = c("season", "fantasypros_id", "player", "pos", "team", "bye", "ecr", "sd", "draft_rank", "scrape_date")
  ]

  projected_scores <- merge(
    week_ranks,
    adp_outcomes[, list(pos, week_rank = rank, avg_week, prob_gp, week_outcomes)],
    by = c("pos", "week_rank"),
    all.x = TRUE
  )[
    !is.na(prob_gp)
  ][
    , list(
      season, week,
      pos, scrape_date, fantasypros_id, player, team, bye, ecr, sd,
      draft_rank, week_rank, avg_week, prob_gp,
      gp_model = sapply(prob_gp, function(p) stats::rbinom(n = 1, size = 1, p = p)),
      projection = sapply(week_outcomes, function(x) {if(length(x) == 0) return(0) else sample(x = x, size = 1)})
    )
  ][
    ,
    `:=`(
      projected_score = projection * gp_model * (week != bye)
    )
  ][
    order(season, week, pos, ecr)
  ]

  return(projected_scores[])
}

.replace_zero <- function(x) {
  replace(x, x == 0, 1)
}

.ffs_draft_to_week <- function(){

  season <- fantasypros_id <- player_name <- pos <- rank <- ecr <- sd <- NULL
  week <- draft_rank <- draft_ecr <- draft_sd <- week_rank <- week_ecr <- week_sd <- NULL

  draft_rank <- data.table::as.data.table(fp_rankings_history())[
    , list(
      season,
      fantasypros_id,
      player_name,
      pos,
      draft_rank = rank,
      draft_ecr = ecr,
      draft_sd = sd
    )
  ]

  week_rank <- data.table::as.data.table(fp_rankings_history_week())[
    , list(
      season,
      week,
      fantasypros_id,
      player_name,
      pos,
      week_rank = rank,
      week_ecr = ecr,
      week_sd = sd
    )
  ]

  draft_week_rank <- merge(
    draft_rank,
    week_rank,
    by = c("season", "fantasypros_id", "player_name", "pos"),
    all.x = TRUE
  )[
    order(season, pos, draft_rank)
  ][
    , list(week_rank = list(week_rank),
           player = list(player_name))
    , by = list(pos, draft_rank)
  ]

  return(draft_week_rank)
}
