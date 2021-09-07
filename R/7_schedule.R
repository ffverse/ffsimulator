#' Generate fantasy schedules
#'
#' This function generates random head to head schedules for a given number of seasons, teams, and weeks.
#'
#' It starts with the [circle method for round robin scheduling](https://en.wikipedia.org/wiki/Round-robin_tournament#Scheduling_algorithm), grows or shrinks the schedule to match the required number of weeks, and then shuffles both the order that teams are assigned in and the order that weeks are generated. This doesn't "guarantee" unique schedules, but there are n_teams! x n_weeks! permutations of the schedule so it's very very likely that the schedules are unique (3x10^18 possible schedules for a 12 team league playing 13 weeks).
#'
#' @param n_teams number of teams in simulation
#' @param n_seasons number of seasons to simulate, default = 100
#' @param n_weeks number of weeks per season, default = 14
#' @param franchises optional: a dataframe of franchises as created by [`ffs_franchises()`] - overrides the `n_teams` argument and will attach actual franchise IDs to the schedule output.
#' @param seed an integer to control reproducibility
#'
#' @examples \donttest{
#' ffs_build_schedules(n_teams = 12, n_seasons = 1, n_weeks = 14)
#' }
#'
#' @return a dataframe of schedules
#'
#' @seealso `vignette("custom")` for example usage
#' @export
ffs_build_schedules <- function(n_teams = NULL,
                                n_seasons = 100,
                                n_weeks = 14,
                                franchises = NULL,
                                seed = NULL) {
  if (!is.null(franchises)) {
    checkmate::assert_data_frame(franchises)
    assert_columns(franchises, c("league_id", "franchise_id"))
    f <- data.table::as.data.table(franchises)
    f <- f[,c("league_id","franchise_id")]
    n_teams <- nrow(franchises)
  }
  checkmate::assert_number(n_teams)
  checkmate::assert_number(n_seasons)
  checkmate::assert_number(n_weeks)

  if (!is.null(seed)) set.seed(seed)

  #### GENERATE ROUND ROBIN SCHEDULE TEMPLATE ####

  schedule_template <- .ff_roundrobin_size(.ff_roundrobin_build(n_teams), n_weeks)

  #### Randomize team order for length n_seasons ####

  team_order <- lapply(seq_len(n_seasons), function(...) sample(seq_len(n_teams), n_teams))

  #### Randomize week order for length n_seasons ####

  week_order <- lapply(seq_len(n_seasons), function(...) sample(seq_len(n_weeks), n_weeks))

  #### Join Template onto Team and Week ####

  schedules <- data.table::data.table(
    season = seq_len(n_seasons),
    schedule = mapply(.ff_roundrobin_applytemplate,
                      team_order = team_order,
                      week_order = week_order,
                      MoreArgs = list(schedule_template = schedule_template),
                      SIMPLIFY = FALSE
                      )
  ) %>%
    tidytable::unnest.("schedule")

  #### Attach actual franchise IDs, if available ####

  if (is.null(franchises)) {
    names(schedules)[c(3,4)] <- c("franchise_id","opponent_id")
  }

  if (!is.null(franchises)) {
    schedule_id <- NULL
    franchise_id <- NULL
    data.table::setorderv(f,c("league_id","franchise_id"))
    f[,`:=`(schedule_id = seq_len(.N))]
    o <- f[,.(schedule_id,opponent_id=franchise_id)]

    schedules <- schedules[f, on = c("team"="schedule_id")
    ][o, on = c("opponent"="schedule_id")
    ][,c("season", "week", "league_id", "franchise_id", "opponent_id")
    ]
    data.table::setorderv(schedules,c("season","week","franchise_id"))
  }

  return(schedules)
}

#' @keywords internal
.ff_roundrobin_build <- function(n_teams) {
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

  x <- NULL
  week <- NULL
  team <- NULL
  opponent <- NULL
  df_schedule <- data.table::data.table(week = seq_len(n_teams-1),
                              x = lapply(schedule,.ff_enframe))

  df_schedule <- tidytable::unnest.(df_schedule,x)[
    ,list(week,team = as.integer(team),opponent)
  ][order(week,team)]

  if (bye) {
    df_schedule <- df_schedule[team == n_teams,team := NA_integer_]
    df_schedule <- df_schedule[opponent == n_teams, opponent := NA_integer_]
    df_schedule <- df_schedule[!is.na(team)]
  }

  return(df_schedule)
}

#' @keywords internal
.ff_enframe <- function(vec){
  data.table::data.table(
    team = names(vec),
    opponent = vec
  )
}

#' @keywords internal
.ff_roundrobin_size <- function(df_schedule, n_weeks) {
  schedule_max <- max(df_schedule$week)

  if (schedule_max == n_weeks) {
    return(df_schedule)
  }

  week <- NULL

  if (schedule_max < n_weeks) {
    x <- df_schedule[week <=(n_weeks-schedule_max)][,`:=`(week = week + schedule_max)]
    df_schedule <- data.table::rbindlist(list(df_schedule,x))
  }

  if (schedule_max > n_weeks) {
    df_schedule <- df_schedule[week <= n_weeks]
  }

  return(df_schedule)
}

#' @keywords internal
.ff_roundrobin_applytemplate <- function(team_order, week_order, schedule_template) {
  team <- NULL
  opponent <- NULL
  week <- NULL

  x <- schedule_template[,`:=`(team = team_order[team],
                                         opponent = team_order[opponent],
                                         week = week_order[week])]

  return(x)
}

#' Get Schedule
#'
#' This function lightly wraps `ffscrapr::ff_schedule()` and adds league_id, which is a required column for ffsimulator, casts IDs to character, and drops actual games played so as to only simulate unplayed games.
#'
#' @param conn a connection object as created by `ffscrapr::ff_connect()` and friends.
#'
#' @return a dataframe of schedule that includes the league_id column
#'
#' @examples
#' \donttest{
#' # cached examples
#' conn <- .ffs_cache("mfl_conn.rds")
#'
#' ffs_schedule(conn)
#' }
#'
#' @seealso vignette("Custom Simulations") for more detailed example usage
#'
#' @export
ffs_schedule <- function(conn){

  schedule <- ffscrapr::ff_schedule(conn)
  if("spread" %in% names(schedule)){
    schedule$result[(!is.na(schedule$spread)|schedule$spread==0) & schedule$result == "T"] <- NA
  }
  schedule <- schedule[is.na(schedule$result),c("week","franchise_id","opponent_id")]
  schedule$league_id <- as.character(conn$league_id)
  schedule$franchise_id <- as.character(schedule$franchise_id)
  schedule$opponent_id <-  as.character(schedule$opponent_id)


  return(schedule)
}

#' Repeat fantasy schedules
#'
#' This function repeats an actual `ffs_schedule()` by the appropriate number of seasons.
#'
#' @param actual_schedule a schedule retrieved by `ffs_schedule()`
#' @param n_seasons number of seasons to simulate, default = 100
#'
#' @examples \donttest{
#'  # ffs_repeat_schedules(actual_schedule = x, n_seasons = 10)
#' }
#'
#' @return a dataframe of schedules for the simulation
#'
#' @seealso `vignette("Custom Simulations")` for example usage
#' @export
ffs_repeat_schedules <- function(actual_schedule, n_seasons){

  data.table::setDT(actual_schedule)

  merge(
    data.table::data.table(seasons = seq_len(n_seasons),
                           k = 1),
    actual_schedule[,c(k = 1,.SD)],
    allow.cartesian = TRUE)[,`:=`(k = NULL)][]
}

