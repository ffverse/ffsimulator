
#' Automatically Plot ff_simulation Object
#'
#' Creates automatic plots for wins, ranks, or points for an `ff_simulation` object as created by `ff_simulate()`.
#'
#' Usage of this function/method requires the ggplot2 package and (for wins and points plots) the ggridges package.
#'
#' @param object a `ff_simulation` object as created by `ff_simulate()`
#' @param type one of "wins", "rank", "points"
#' @param ... unused, required by autoplot generic
#'
#' @examples
#' \donttest{
#'
#' simulation <- .ffs_cache("foureight_sim.rds")
#'
#' ggplot2::autoplot(simulation) # default is type = "wins"
#' ggplot2::autoplot(simulation, type = "rank")
#' ggplot2::autoplot(simulation, type = "points")
#' }
#'
#' @seealso `vignette("basic")` for example usage
#'
#' @return a ggplot object
#' @export
autoplot.ff_simulation <- function(object,
                                   type = c("wins", "rank", "points"),
                                   ...) {
  type <- rlang::arg_match(type)

  rlang::check_installed(c("ggplot2 (>= 3.4.0)", "ggridges"))

  switch(type,
         "wins" = p <- .ffs_plot_wins(object, ...),
         "rank" = p <- .ffs_plot_rank(object, ...),
         "points" = p <- .ffs_plot_points(object, ...)
  )
  p
}

#' @keywords internal
.ffs_plot_wins <- function(object, ...) {

  ss <- object$summary_season
  data.table::setDT(ss)
  h2h_wins <- NULL
  franchise_name <- NULL

  ss_levels <- ss[
    , list(franchise_name,h2h_wins = stats::median(h2h_wins, na.rm = TRUE))
    ,by = "franchise_name"
  ][
    order(h2h_wins)
  ]

  ss$franchise_name <- factor(ss$franchise_name, levels = ss_levels$franchise_name)

  ggplot2::ggplot(
    ss,
    ggplot2::aes(
      x = .data$h2h_wins,
      y = .data$franchise_name,
      fill = .data$franchise_name
    )
  ) +
    ggridges::geom_density_ridges(
      stat = "binline",
      color = "white",
      binwidth = 1,
      scale = 1.3,
      alpha = 0.8,
      show.legend = FALSE
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq.int(0, max(object$summary_season$h2h_wins) + 1, by = 2)
    ) +
    ggplot2::xlab("Season Wins") +
    ggplot2::ylab(NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      plot.title.position = "plot"
    ) +
    ggplot2::labs(
      title = glue::glue("Season Win Totals - {object$simulation_params$n_seasons} Simulated Seasons"),
      subtitle = glue::glue("{object$league_info$league_name}"),
      caption = glue::glue("ffsimulator R pkg | Based on rankings as of {object$simulation_params$scrape_date}")
    )
}

#' @keywords internal
.ffs_plot_rank <- function(object, ...) {

  ss <- object$summary_season
  data.table::setDT(ss)
  h2h_wins <- NULL
  franchise_name <- NULL
  season_rank <- NULL

  ss[
    ,`:=`(season_rank = rank(-h2h_wins, ties.method = "min"))
    , by = "season"
  ]

  ss_levels <- ss[
    , list(franchise_name, season_rank = mean(season_rank, ties.method = "min"))
    , by = "franchise_name"
  ][
    order(season_rank)
  ]

  ss$franchise_name <- factor(ss$franchise_name, levels = ss_levels$franchise_name)
  ss$rank_label <- factor(scales::ordinal(ss$season_rank), scales::ordinal(sort(unique(ss$season_rank))))

  ggplot2::ggplot(
    ss,
    ggplot2::aes(x = .data$franchise_name, color = .data$franchise_name, fill = .data$franchise_name)) +
    ggplot2::geom_bar() +
    ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(position = "none"))+
    ggplot2::facet_wrap(~ .data$rank_label) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab("Number of Seasons") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      plot.title.position = "plot",
      plot.caption.position = "plot"
    ) +
    ggplot2::labs(
      title = glue::glue("Final Season Rank - {object$simulation_params$n_seasons} Simulated Seasons"),
      subtitle = glue::glue("{object$league_info$league_name}"),
      caption = glue::glue("ffsimulator R pkg | Based on rankings as of {object$simulation_params$scrape_date}"),
      fill = "Franchise Name",
      color = "Franchise Name"
    )
}

#' @keywords internal
.ffs_plot_points <- function(object, ...) {

  sw <- object$summary_week
  data.table::setDT(sw)
  team_score <- NULL
  sw_levels <- sw[
    , list(team_score = stats::median(team_score, na.rm = TRUE))
    ,by = c("franchise_name")
    ][
      order(team_score)
    ]
  sw$franchise_name <- factor(sw$franchise_name, levels = sw_levels$franchise_name)

  ggplot2::ggplot(
    sw,
    ggplot2::aes(
      x = .data$team_score,
      y = .data$franchise_name,
      fill = .data$franchise_name
    )) +
    ggridges::geom_density_ridges(
      color = "white",
      quantile_lines = TRUE,
      scale = 1.3,
      alpha = 0.8,
      show.legend = FALSE
    ) +
    ggplot2::scale_x_continuous(n.breaks = 8) +
    ggplot2::xlab("Weekly Score") +
    ggplot2::ylab(NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      plot.title.position = "plot"
    ) +
    ggplot2::labs(
      title = glue::glue("Weekly Scores - {
                         object$simulation_params$n_seasons * object$simulation_params$n_weeks
                         } Simulated Weeks"),
      subtitle = glue::glue("{object$league_info$league_name}"),
      caption = glue::glue("ffsimulator R pkg | Based on rankings as of {object$simulation_params$scrape_date}")
    )
}

#' @rdname autoplot.ff_simulation
#' @param x A `ff_simulation` object.
#' @param y Ignored, required for compatibility with the `plot()` generic.
#' @export
plot.ff_simulation <- function(x, ..., type = c("wins", "rank", "points"), y) {
  rlang::check_installed(c("ggplot2 (>= 3.4.0)", "ggridges"))

  type <- rlang::arg_match(type)

  ggplot2::autoplot(x, type = type, ...)
}
