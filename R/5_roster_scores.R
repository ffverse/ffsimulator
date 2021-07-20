#' Join Rosters to Projected Scores
#'
#' Attaches projected scores to rosters (via an inner-join) and creates a positional ranking column.
#'
#' @param projected_scores a dataframe of projected scores, as created by `ffs_generate_projections()`
#' @param rosters a dataframe of rosters, as created by `ffs_rosters()`
#'
#' @examples \donttest{
#' # cached examples
#' projected_scores <- .ffs_cache("projected_scores.rds")
#' rosters <- .ffs_cache("mfl_rosters.rds")
#'
#' ffs_score_rosters(projected_scores, rosters)
#' }
#'
#' @seealso vignette("Custom Simulations") for example usage
#'
#' @return A dataframe of roster-level projected scores
#'
#' @export

ffs_score_rosters <- function(projected_scores, rosters) {
  checkmate::assert_data_frame(projected_scores)
  checkmate::assert_data_frame(rosters)

  checkmate::assert_subset(
    c(
      "fantasypros_id", "ecr", "rank", "projection",
      "injury_model", "season", "week",
      "projected_score", "scrape_date"
    ),
    names(projected_scores)
  )
  checkmate::assert_subset(
    c("fantasypros_id", "league_id", "franchise_id", "pos"),
    names(rosters)
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
