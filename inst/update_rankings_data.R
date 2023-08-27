rlang::check_installed(
  c("ffpros", "ffscrapr","ffsimulator", "nflreadr",
    "dplyr", "purrr", "tidyr", "stringr",
    "furrr", "future", "mgcv"),
  action = function(pkg, ...) {
    install.packages(
      pkg,
      repos = c("https://ffverse.r-universe.dev", getOption("repos")),
      ...
    )
  }
)

library(magrittr)

options(future.rng.onMisuse = "ignore", ffpros.cache = "filesystem")
future::plan(future::multisession)

build_draft_rankings <- function(build_seasons = 2012:nflreadr::most_recent_season(),
                                 directory = ffsimulator::.ffs_cache_dir()) {

  checkmate::check_numeric(
    build_seasons,
    lower = 2012,
    upper = nflreadr::most_recent_season(),
    any.missing = FALSE,
    min.len = 1
  )

  dir.create(directory, recursive = TRUE, showWarnings = FALSE)

  cli::cli_alert_info("Building {length(build_seasons)} seasons of draft rankings into directory {directory}")

  seasons <- intersect(2016:nflreadr::most_recent_season(), build_seasons)
  fp_rankings_history <- tibble::tibble()
  if(length(seasons) > 0 ){

    cli::cli_alert_info("Building: {seasons}")

    pages <- c(
      "qb-cheatsheets",
      "ppr-rb-cheatsheets",
      "ppr-wr-cheatsheets",
      "ppr-te-cheatsheets",
      "k-cheatsheets",
      "dst-cheatsheets",
      "dl-cheatsheets",
      "lb-cheatsheets",
      "db-cheatsheets"
    )

    fp_rankings_history <- tidyr::crossing(pages, seasons) %>%
      dplyr::mutate(
        rankings = furrr::future_map2(pages, seasons,
                                      ~ ffpros::fp_rankings(page = .x, year = .y),
                                      .progress = TRUE)
      ) %>%
      tidyr::unnest(rankings) %>%
      dplyr::transmute(
        page_pos =
          stringr::str_remove_all(pages, "cheatsheets|^ppr|\\-") %>%
          toupper() %>%
          stringr::str_squish(),
        season = seasons,
        fantasypros_id = as.character(fantasypros_id),
        sportradar_id,
        player_name = nflreadr::clean_player_names(player_name),
        pos = dplyr::case_when(
          pos %in% c("CB", "S") ~ "DB",
          pos %in% c("OLB", "LB") ~ "LB",
          pos %in% c("DE", "DT", "NT") ~ "DL",
          TRUE ~ pos
        ),
        team,
        rank,
        ecr,
        sd
      ) %>%
      dplyr::filter(page_pos == pos)
  }

  seasons2 <- intersect(2012:2015, build_seasons)
  fp_rankings_history2 <- tibble::tibble()
  if(length(seasons2) > 0) {

    cli::cli_alert_info("Building {seasons2} (no IDP or PPR available)")
    pages2 <- c(
      "qb-cheatsheets",
      "rb-cheatsheets",
      "wr-cheatsheets",
      "te-cheatsheets",
      "k-cheatsheets",
      "dst-cheatsheets"
    )

    fp_rankings_history2 <- tidyr::crossing(pages2, seasons2) %>%
      dplyr::mutate(
        rankings = furrr::future_map2(pages2, seasons2,
                                      ~ ffpros::fp_rankings(page = .x, year = .y),
                                      .progress = TRUE)
      ) %>%
      tidyr::unnest(rankings) %>%
      dplyr::transmute(
        page_pos =
          stringr::str_remove_all(pages2, "cheatsheets|^ppr|\\-") %>%
          toupper() %>%
          stringr::str_squish(),
        season = seasons2,
        fantasypros_id = as.character(fantasypros_id),
        sportradar_id,
        player_name = nflreadr::clean_player_names(player_name),
        pos,
        team,
        rank,
        ecr,
        sd
      ) %>%
      dplyr::filter(page_pos == pos)
  }

  fp_rankings_history <- bind_rows(fp_rankings_history2, fp_rankings_history)

  if(nrow(fp_rankings_history) == 0) {
    cli::cli_abort("No rows from scrape - something went wrong!")
  }
  saveRDS(fp_rankings_history, file = file.path(directory, "fp_rankings_history.rds"))

  cli::cli_alert_success("Completed draft rankings history")
  invisible(file.path(directory, "fp_rankings_history.rds"))
}

build_weekly_rankings <- function(build_seasons = 2012:nflreadr::most_recent_season(),
                                  directory = ffsimulator::.ffs_cache_dir()){
  checkmate::check_numeric(
    build_seasons,
    lower = 2012,
    upper = nflreadr::most_recent_season(),
    any.missing = FALSE,
    min.len = 1
  )

  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  weeks <- 1:16

  cli::cli_alert_info("Building {length(build_seasons)} seasons of inseason weekly rankings into directory {directory}")

  seasons <- intersect(2012:2015, build_seasons)
  fp_rankings_history <- tibble::tibble()
  if (length(seasons) > 0) {
    cli::cli_alert_info("Building {seasons}")
    pages <- c(
      "qb",
      "rb",
      "wr",
      "te",
      "k",
      "dst",
      "dl",
      "lb",
      "db"
    )
    fp_rankings_history <- tidyr::crossing(pages, seasons, weeks) %>%
      dplyr::mutate(
        rankings = furrr::future_pmap(
          list(pages, seasons, weeks),
          ~ ffpros::fp_rankings(page = ..1, year = ..2, week = ..3),
          .progress = TRUE
        )
      ) %>%
      tidyr::unnest(rankings) %>%
      dplyr::transmute(
        page_pos =
          stringr::str_remove_all(pages, "cheatsheets|^ppr|\\-") %>%
          toupper() %>%
          stringr::str_squish(),
        season = seasons,
        week = weeks,
        fantasypros_id = as.character(fantasypros_id),
        sportradar_id,
        player_name = nflreadr::clean_player_names(player_name),
        pos = dplyr::case_when(
          pos %in% c("CB", "S") ~ "DB",
          pos %in% c("OLB", "LB") ~ "LB",
          pos %in% c("DE", "DT", "NT") ~ "DL",
          TRUE ~ pos
        ),
        team,
        rank,
        ecr,
        sd
      ) %>%
      dplyr::filter(page_pos == pos)
  }

  seasons2 <- intersect(2016:nflreadr::most_recent_season(), build_seasons)
  fp_rankings_history2 <- tibble::tibble()
  if(length(seasons2) > 0) {
    cli::cli_alert_info("Building {seasons2}")
    pages2 <- c(
      "qb",
      "ppr-rb",
      "ppr-wr",
      "ppr-te",
      "k",
      "dst",
      "dl",
      "lb",
      "db"
    )

    fp_rankings_history2 <- tidyr::crossing(pages2, seasons2, weeks) %>%
      dplyr::mutate(
        rankings = furrr::future_pmap(
          list(pages2, seasons2, weeks),
          ~ ffpros::fp_rankings(page = ..1, year = ..2, week = ..3),
          .progress = TRUE
        )
      ) %>%
      tidyr::unnest(rankings) %>%
      dplyr::transmute(
        page_pos =
          stringr::str_remove_all(pages2, "cheatsheets|^ppr|\\-") %>%
          toupper() %>%
          stringr::str_squish(),
        season = seasons2,
        week = weeks,
        fantasypros_id = as.character(fantasypros_id),
        sportradar_id,
        player_name = nflreadr::clean_player_names(player_name),
        pos,
        team,
        rank,
        ecr,
        sd
      ) %>%
      dplyr::filter(page_pos == pos)
  }

  fp_rankings_history_week <- dplyr::bind_rows(fp_rankings_history, fp_rankings_history2)

  saveRDS(fp_rankings_history_week, file.path(directory, "fp_rankings_history_week.rds"))
  cli::cli_alert_success("Completed inseason weekly rankings history")
  invisible(file.path(directory, "fp_rankings_history_week.rds"))
}

build_injury_model <- function(base_seasons = seq(2012, nflreadr::most_recent_season()),
                               directory = ffsimulator::.ffs_cache_dir()) {
  checkmate::check_numeric(
    base_seasons,
    lower = 2012,
    upper = nflreadr::most_recent_season(),
    any.missing = FALSE,
    min.len = 1
  )

  dir.create(directory, recursive = TRUE, showWarnings = FALSE)

  conn <- ffscrapr::mfl_connect(2021, 47747)
  cli::cli_alert_info("Retrieving ")
  scoring_history <- ffscrapr::ff_scoringhistory(conn, base_seasons)
  model_gam <- function(data) mgcv::gam(games_played_rate ~ mgcv::s(rank, bs = "cs"), data = data)

  fp_injury_table <- ffsimulator::fp_rankings_history() %>%
    dplyr::select(-"page_pos") %>%
    dplyr::left_join(
      ffscrapr::dp_playerids() %>%
        dplyr::select("fantasypros_id", "gsis_id"),
      by = "fantasypros_id"
    ) %>%
    dplyr::filter(!is.na(.data$gsis_id), pos %in% c("QB", "RB", "WR", "TE", "K")) %>%
    dplyr::left_join(
      scoring_history %>%
        dplyr::filter(!is.na(.data$gsis_id), .data$week <= 17) %>%
        dplyr::select("season", "gsis_id", "team", "points"),
      by = c("season", "gsis_id")
    ) %>%
    dplyr::group_by(
      .data$season,
      .data$pos,
      .data$rank,
      .data$fantasypros_id,
      .data$player_name
    ) %>%
    dplyr::summarise(
      week_outcomes = list(points),
      games_played = dplyr::n(),
      games_missed = 16 - games_played,
      games_played_rate = games_played / 16
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(pos) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      model = purrr::map(data, model_gam),
      prob_gp = purrr::map2(model, data, ~ as.numeric(stats::predict(.x, new_data = .y))),
      model = NULL
    ) %>%
    dplyr::ungroup() %>%
    tidyr::unnest(c(data, prob_gp)) %>%
    dplyr::distinct(pos, rank, prob_gp) %>%
    dplyr::arrange(pos, rank)

  saveRDS(fp_injury_table, file.path(directory, "fp_injury_table.rds"))
  cli::cli_alert_success("Completed injury model recalculation")
  invisible(file.path(directory, "fp_injury_table.rds"))
}

build_draft_rankings(2012:nflreadr::most_recent_season())
build_weekly_rankings(2012:nflreadr::most_recent_season())
build_injury_model(2012:nflreadr::most_recent_season())
