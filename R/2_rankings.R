#' Download latest rankings from DynastyProcess GitHub
#'
#' Fetches a copy of FP data from DynastyProcess's data repository.
#'
#' If you have any issues with the output of this data, please open an issue in
#' the DynastyProcess data repository.
#'
#' @seealso <https://github.com/dynastyprocess/data>
#'
#' @export
.ff_latest_rankings <- function() {
  url_query <- "https://github.com/dynastyprocess/data/raw/master/files/db_fpecr_latest.rds"

  response <- httr::RETRY("GET", url_query)

  if (httr::http_error(response)) {
    stop(glue::glue("GitHub request failed with error: <{httr::status_code(response)}> \n
                    while calling <{url_query}>"), call. = FALSE)
  }

  fp_latest <- response %>%
    httr::content(as = "raw") %>%
    parse_raw_rds()

  fp_cleaned <- fp_latest %>%
    dplyr::filter(
      .data$ecr_type == "rp",
      stringr::str_detect(.data$page_type, paste0(tolower(.data$pos),"$"))
    ) %>%
    dplyr::select(
      "player",
      "fantasypros_id"="id",
      "pos",
      "team" = "tm",
      "ecr",
      "sd",
      "sportradar_id"="sportsdata_id",
      "scrape_date"
    )

  return(fp_cleaned)
}
