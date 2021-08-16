#' Download latest rankings from DynastyProcess GitHub
#'
#' Fetches a copy of the latest FantasyPros redraft positional rankings data from DynastyProcess.com's data repository.
#'
#' If you have any issues with the output of this data, please open an issue in
#' the DynastyProcess data repository.
#'
#' @seealso <https://github.com/dynastyprocess/data>
#' @seealso `vignette("Custom Simulation")` for example usage
#'
#' @examples \donttest{
#'
#' ffs_latest_rankings()
#' }
#'
#' @return a dataframe with a copy of the latest FP rankings from DynastyProcess's data repository
#'
#' @export
ffs_latest_rankings <- function() {
  fp_latest <- nflreadr::load_ff_rankings()

  fp_cleaned <- fp_latest %>%
    dplyr::filter(
      .data$ecr_type == "rp",
      stringr::str_detect(.data$page_type, paste0(tolower(.data$pos), "$"))
    ) %>%
    dplyr::select(
      "player",
      "fantasypros_id" = "id",
      "pos",
      "team" = "tm",
      "bye",
      "ecr",
      "sd",
      "sportradar_id" = "sportsdata_id",
      "scrape_date"
    )

  return(fp_cleaned)
}
