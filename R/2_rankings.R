#' Download latest rankings from DynastyProcess GitHub
#'
#' Fetches a copy of the latest FantasyPros redraft positional rankings data from DynastyProcess.com's data repository.
#'
#' If you have any issues with the output of this data, please open an issue in
#' the DynastyProcess data repository.
#'
#' @param type one of "draft" or "week" - controls whether to pull preseason or inseason rankings.
#'
#' @seealso <https://github.com/dynastyprocess/data>
#' @seealso `vignette("custom")` for example usage
#'
#' @examples \donttest{
#' try({ # try block to prevent CRAN-related issues
#' ffs_latest_rankings()
#' })
#' }
#'
#' @return a dataframe with a copy of the latest FP rankings from DynastyProcess's data repository
#'
#' @export
ffs_latest_rankings <- function(type = c("draft","week")) {

  type <- rlang::arg_match0(type, c("draft","week"))

  if(type == "draft"){
    fp_latest <- nflreadr::load_ff_rankings()

    fp_cleaned <- fp_latest[
      fp_latest$ecr_type == "rp" & mapply(grepl,x = fp_latest$page_type,pattern = paste0(tolower(fp_latest$pos), "$")),
      c("player",
        "fantasypros_id" = "id",
        "pos",
        "team" = "tm",
        "bye",
        "ecr",
        "sd",
        # "sportradar_id" = "sportsdata_id",
        "scrape_date")]

    names(fp_cleaned)[2] <- "fantasypros_id"
    names(fp_cleaned)[4] <- "team"
    # names(fp_cleaned)[8] <- "sportradar_id"

    if(all(is.na(fp_cleaned$bye))) fp_cleaned$bye <- 0

  }

  if(type == "week"){

    fp_latest <- nflreadr::rds_from_url("https://github.com/dynastyprocess/data/raw/master/files/fp_latest_weekly.rds")

    fp_cleaned <- fp_latest[,c("player_name",
                               "fantasypros_id",
                               "pos",
                               "team",
                               "ecr",
                               "sd",
                               #"sportradar_id",
                               "scrape_date")]
    names(fp_cleaned)[1] <- "player"

  }
  return(fp_cleaned)
}
