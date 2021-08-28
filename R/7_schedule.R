#' Generate fantasy schedules
#'
#' This function generates random head to head schedules for a given number of seasons, teams, and weeks.
#'
#' It starts with the [circle method for round robin scheduling](https://en.wikipedia.org/wiki/Round-robin_tournament#Scheduling_algorithm), grows or shrinks the schedule to match the required number of weeks, and then shuffles both the order that teams are assigned in and the order that weeks are generated. This doesn't "guarantee" unique schedules, but there are n_teams! x n_weeks! permutations of the schedule so it's very very likely that the schedules are unique (3x10^18 possible schedules for a 12 team league playing 13 weeks).
#'
#' @param n_teams number of teams in simulation
#' @param n_seasons number of seasons to simulate, default = 100
#' @param n_weeks number of weeks per season, default = 14
#' @param seed an integer to control reproducibility
#'
#' @examples \donttest{
#'   ffs_build_schedules(n_teams = 12, n_seasons = 1, n_weeks = 14)
#' }
#'
#' @return a dataframe of schedules
#'
#' @seealso `vignette("custom")` for example usage
#' @export
ffs_build_schedules <- function(n_teams,
                                n_seasons = 100,
                                n_weeks = 14,
                                seed = NULL) {
  checkmate::assert_number(n_teams)
  checkmate::assert_number(n_seasons)
  checkmate::assert_number(n_weeks)

  if (!is.null(seed)) set.seed(seed)

  #### GENERATE ROUND ROBIN SCHEDULE TEMPLATE ####

  schedule_template <- .ff_roundrobin_build(n_teams) %>%
    .ff_roundrobin_size(n_weeks)

  #### Randomize team order for length n_seasons ####

  team_orders <- purrr::map(
    seq_len(n_seasons),
    ~ sample(seq_len(n_teams), n_teams)
  )

  #### Randomize week order for length n_seasons ####

  week_orders <- purrr::map(
    seq_len(n_seasons),
    ~ sample(seq_len(n_weeks), n_weeks)
  )

  #### Join Template onto Team and Week ####

  schedules <- tibble::tibble(
    season = seq_len(n_seasons),
    schedule = purrr::map2(
      team_orders, week_orders,
      .ff_roundrobin_applytemplate,
      schedule_template
    )
  ) %>%
    tidyr::unnest("schedule")

  return(schedules)
}

#' @keywords internal
.ff_roundrobin_build <- function(n_teams = 14) {
  bye <- FALSE

  if (n_teams %% 2) {
    bye <- TRUE
    n_teams <- n_teams + 1
  }

  all_teams <- seq_len(n_teams)

  half_one <- utils::head(all_teams, n_teams / 2)
  half_two <- rev(utils::tail(all_teams, n_teams / 2))

  schedule <- vector(mode = "list", length = n_teams - 1)

  week_one <- c(half_one, half_two) %>%
    stats::setNames((c(half_two, half_one)))

  schedule[[1]] <- week_one

  for (i in 2:(n_teams - 1)) {
    half_one <- c(1L, utils::head(half_two, 1), utils::tail(half_one, -1))
    half_two <- c(utils::tail(half_two, -1), utils::tail(half_one, 1))
    half_one <- utils::head(half_one, -1)

    schedule[[i]] <- c(half_one, half_two) %>%
      stats::setNames((c(half_two, half_one)))
  }

  df_schedule <- schedule %>%
    purrr::map(tibble::enframe, name = "team", value = "opponent") %>%
    tibble::tibble() %>%
    dplyr::mutate(week = seq_len(n_teams - 1)) %>%
    tidyr::unnest(1) %>%
    dplyr::transmute(
      .data$week,
      team = as.integer(.data$team),
      .data$opponent
    ) %>%
    dplyr::arrange(.data$week, .data$team)

  if (bye) {
    df_schedule <- df_schedule %>%
      dplyr::mutate_at(
        c("team", "opponent"),
        ~dplyr::case_when(
          .x == n_teams ~ NA_integer_,
          TRUE ~ .x
        )
      ) %>%
      dplyr::filter(!is.na(.data$team))
  }

  return(df_schedule)
}

#' @keywords internal
.ff_roundrobin_size <- function(df_schedule, n_weeks) {
  schedule_max <- max(df_schedule$week)

  if (schedule_max == n_weeks) {
    return(df_schedule)
  }

  if (schedule_max < n_weeks) {
    x <- df_schedule %>%
      dplyr::filter(.data$week <= (n_weeks - schedule_max)) %>%
      dplyr::mutate(week = .data$week + schedule_max)

    df_schedule <- dplyr::bind_rows(df_schedule, x)
  }

  if (schedule_max > n_weeks) {
    df_schedule <- df_schedule %>%
      dplyr::filter(.data$week <= n_weeks)
  }

  return(df_schedule)
}

#' @keywords internal
.ff_roundrobin_applytemplate <- function(team_order, week_order, schedule_template) {
  df_schedule <- schedule_template %>%
    dplyr::mutate(
      team = team_order[.data$team],
      opponent = team_order[.data$opponent],
      week = week_order[.data$week]
    ) %>%
    dplyr::arrange(.data$week, .data$team)

  return(df_schedule)
}
