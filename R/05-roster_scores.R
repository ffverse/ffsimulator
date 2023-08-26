#' Join Rosters to Projected Scores
#'
#' Attaches projected scores to rosters (via an inner-join) and creates a positional ranking column.
#'
#' @param projected_scores a dataframe of projected scores, as created by `ffs_generate_projections()`
#' @param rosters a dataframe of rosters, as created by `ffs_rosters()`
#'
#' @examples \donttest{
#' # cached examples
#' projected_scores <- .ffs_cache_example("projected_scores.rds")
#' rosters <- .ffs_cache_example("mfl_rosters.rds")
#'
#' ffs_score_rosters(projected_scores, rosters)
#' }
#'
#' @seealso vignette("custom") for example usage
#'
#' @return A dataframe of roster-level projected scores
#'
#' @export
ffs_score_rosters <- function(projected_scores, rosters) {
  checkmate::assert_data_frame(projected_scores)
  checkmate::assert_data_frame(rosters)

  assert_columns(
    projected_scores,
    c(
      "fantasypros_id", "ecr", "rank", "projection",
      "gp_model", "season", "week",
      "projected_score", "scrape_date"
    )
  )

  assert_columns(
    rosters,
    c("fantasypros_id", "league_id", "franchise_id", "pos")
  )

  projected_scores <- data.table::as.data.table(
    projected_scores[, c(
      "fantasypros_id", "ecr", "rank", "projection", "gp_model",
      "season", "week", "projected_score", "scrape_date"
    )]
  )

  data.table::setDT(rosters)
  data.table::setkeyv(projected_scores, "fantasypros_id")
  data.table::setkeyv(rosters, "fantasypros_id")

  roster_scores <- merge(rosters, projected_scores, by = "fantasypros_id", all = FALSE, allow.cartesian = TRUE)

  roster_scores[order(-roster_scores$projected_score),
    `:=`(pos_rank = seq_len(.N)),
    by = c("league_id", "franchise_id", "pos", "season", "week")
  ]
  return(roster_scores)
}
