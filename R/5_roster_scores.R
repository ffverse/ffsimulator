#' Join Rosters to Projected Scores
#'
#' Attaches projected scores to rosters (via a left-join)
#'
#' @param projected_scores a dataframe of projected scores, as created by `ffs_generate_projections()`
#' @param rosters a dataframe of rosters, as created by `ffs_rosters()` - must contain fantasypros_id
#'
#' @return A dataframe of roster-level projected scores

ffs_score_rosters <- function(projected_scores,rosters){

  roster_scores <- rosters %>%
    dplyr::inner_join(
      projected_scores %>%
        dplyr::select("fantasypros_id","ecr","rank","projection","injury_model",
                      "season","week","projected_score","scrape_date"),
      by = "fantasypros_id") %>%
    dplyr::arrange(-.data$projected_score) %>%
    dplyr::group_by(.data$league_id,.data$franchise_id,.data$pos, .data$season, .data$week) %>%
    dplyr::mutate(pos_rank = dplyr::row_number()) %>%
    dplyr::ungroup()

  return(roster_scores)
}
