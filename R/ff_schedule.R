#' Generate fantasy schedules
#'
#' The main function of the package
#'
#' @param n_teams number of teams in simulation, default = 14
#' @param n_seasons number of seasons to simulate, default = 100
#' @param weeks_per_season number of weeks per season, default = 17
#' @param seed an integer to control reproducibility
#'
#' @export

ff_build_schedules <- function(n_teams = 14,
                               n_seasons = 100,
                               n_weeks = 13,
                               seed = NULL){

  if(!is.null(seed)) set.seed(seed)

  #### GENERATE ROUND ROBIN SCHEDULE TEMPLATE ####

  schedule_template <- .ff_roundrobin_build(n_teams) %>%
    .ff_roundrobin_size(n_weeks)

  #### Randomize team order for length n_seasons ####

  team_orders <- purrr::map(seq_len(n_seasons),
                            ~sample(seq_len(n_teams), n_teams))

  #### Randomize week order for length n_seasons ####

  week_orders <- purrr::map(seq_len(n_seasons),
                            ~sample(seq_len(n_weeks), n_weeks))

  #### Join Template onto Team and Week ####

  schedules <- tibble::tibble(
    season = seq_len(n_seasons),
    schedule = purrr::map2(
      team_orders,week_orders,
      .ff_roundrobin_applytemplate,
      schedule_template)
  ) %>%
    tidyr::unnest("schedule")

  return(schedules)
}

#' @keywords internal
.ff_roundrobin_build <- function(n_teams = 14){

  bye <- FALSE

  if(n_teams %%2) {
    bye <- TRUE
    n_teams <- n_teams + 1
  }

  all_teams <- seq_len(n_teams)

  half_one <- head(all_teams, n_teams / 2)
  half_two <- rev(tail(all_teams, n_teams / 2))

  schedule <- vector(mode = "list", length = n_teams-1)

  week_one <- c(half_one,half_two) %>%
    setNames(c(half_two,half_one))

  schedule[[1]] <- week_one

  for(i in 2:(n_teams-1)){

    half_one <- c(1L, head(half_two,1), tail(half_one,-1))
    half_two <- c(tail(half_two,-1), tail(half_one,1))
    half_one <- head(half_one,-1)

    schedule[[i]] <- c(half_one,half_two) %>%
      setNames(c(half_two,half_one))
  }

  df_schedule <- schedule %>%
    purrr::map(tibble::enframe,name = "team",value = "opponent") %>%
    tibble::tibble() %>%
    dplyr::mutate(week = seq_len(n_teams-1)) %>%
    tidyr::unnest(1) %>%
    dplyr::transmute(
      .data$week,
      team = as.integer(.data$team),
      .data$opponent) %>%
    dplyr::arrange(.data$week,.data$team)

  if(bye) {df_schedule <- df_schedule %>%
    dplyr::mutate_at(c("team","opponent"),
                     dplyr::case_when(.x == n_teams ~ NA_integer_,
                                      TRUE ~ .x)) %>%
    dplyr::filter(!is.na(.data$team))
  }

  return(df_schedule)
}

#' @keywords internal
.ff_roundrobin_size <- function(df_schedule, n_weeks){

  schedule_max <- max(df_schedule$week)

  if(schedule_max == n_weeks) return(df_schedule)

  if(schedule_max < n_weeks) {

    x <- df_schedule %>%
      dplyr::filter(week <= (n_weeks-schedule_max)) %>%
      dplyr::mutate(week = week + schedule_max)

    df_schedule <- dplyr::bind_rows(df_schedule, x)
  }

  if(schedule_max > n_weeks){
    df_schedule <- df_schedule %>%
      dplyr::filter(week <= n_weeks)
  }

  return(df_schedule)
}

.ff_roundrobin_applytemplate <- function(team_order,week_order,schedule_template){

  df_schedule <- schedule_template %>%
    dplyr::mutate(
      team = team_order[.data$team],
      opponent = team_order[.data$opponent],
      week = week_order[.data$week]
    ) %>%
    dplyr::arrange(.data$week,.data$team)

  return(df_schedule)
}
