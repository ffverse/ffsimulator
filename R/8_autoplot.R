
#' Automatically Plot ff_simulation Object
#'
#' Creates some plots for wins, ranks, or points for an `ff_simulation` object as created by `ff_simulate()`
#'
#' @param object a `ff_simulation` object as created by `ff_simulate()`
#' @param type one of "wins", "rank", "points"
#' @param ... unused
#' @examples
#'
#' @return a ggplot object
#' @export
autoplot.ff_simulation <- function(
  object,
  type = c("wins", "rank", "points"),
  ...){

  type <- match.arg(type)

  if(!requireNamespace("ggplot2",quietly = TRUE)) {
    stop("`ggplot2` must be installed to use `autoplot`.", call. = FALSE)
    }

  if (type == "wins" && !requireNamespace("ggridges", quietly = TRUE)) {
    stop("`ggridges` must be installed to use `type = \"wins\"` option.", call. = FALSE)
  }

  switch (type,
    "wins" = p <- .ffs_plot_wins(object, ...),
    "rank" = p <- .ffs_plot_rank(object, ...),
    "points" = p <- .ffs_plot_points(object, ...)
  )

  p
}

.ffs_plot_wins <- function(object,...){

  object$summary_season %>%
    dplyr::mutate(franchise_name = forcats::fct_reorder(.f = .data$franchise_name, .x = .data$h2h_wins)) %>%
    ggplot2::ggplot(
      ggplot2::aes(x = .data$h2h_wins,
                   y = .data$franchise_name,
                   fill = .data$franchise_name)) +
    ggridges::geom_density_ridges(
      stat = "binline",
      color = "white",
      binwidth = 1,
      scale = 1.3,
      alpha = 0.8
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq.int(0, max(object$summary_season$h2h_wins) + 1, by = 2)) +
    ggplot2::xlab("Season Wins") +
    ggplot2::ylab(NULL) +
    ggplot2::theme_minimal()+
    ggplot2::theme(
      legend.position = "none",
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      plot.title.position = "plot"
    ) +
    ggplot2::labs(
      title = glue::glue("Season Win Totals - {object$simulation_params$n_seasons} Simulated Seasons"),
      subtitle = glue::glue("{object$league_info$league_name}"),
      caption = glue::glue("ffsimulator R pkg | FP rankings as of {object$latest_rankings$scrape_date[[1]]}")
    )

}


.ffs_plot_rank <- function(object,...){

  object$summary_season %>%
    dplyr::group_by(.data$season) %>%
    dplyr::mutate(season_rank = rank(-.data$h2h_wins, ties.method = "min")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      franchise_name = forcats::fct_reorder(.f = .data$franchise_name, .x = .data$season_rank),
      rank_label = scales::ordinal(.data$season_rank) %>% forcats::fct_reorder(.data$season_rank),
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$franchise_name, color = .data$franchise_name, fill = .data$franchise_name)) +
    ggplot2::geom_bar() +
    ggplot2::facet_wrap(~.data$rank_label) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab("Number of Seasons") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      plot.title.position = "plot",
      plot.caption.position = "plot"
    ) +
    ggplot2::labs(
      title = glue::glue("Final Season Rank - {object$simulation_params$n_seasons} Simulated Seasons"),
      subtitle = glue::glue("{object$league_info$league_name}"),
      caption = glue::glue("ffsimulator R pkg | FP rankings as of {object$latest_rankings$scrape_date[[1]]}"),
      fill = "Franchise Name",
      color = "Franchise Name"
    )

}

.ffs_plot_points <- function(object,...){

  object$summary_week %>%
    dplyr::mutate(franchise_name = forcats::fct_reorder(.f = .data$franchise_name, .x = .data$team_score)) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$team_score,
               y = .data$franchise_name,
               fill = .data$franchise_name)) +
    ggridges::geom_density_ridges(
      color = "white",
      quantile_lines = TRUE,
      scale = 1.3,
      alpha = 0.8
    ) +
    ggplot2::scale_x_continuous(n.breaks = 8) +
    ggplot2::xlab("Weekly Score") +
    ggplot2::ylab(NULL) +
    ggplot2::theme_minimal()+
    ggplot2::theme(
      legend.position = "none",
      panel.grid.major.y = ggplot2::element_blank(),
      plot.title.position = "plot"
    ) +
    ggplot2::labs(
      title = glue::glue("Weekly Scores - {
                         object$simulation_params$n_seasons * object$simulation_params$weeks_per_season
                         } Simulated Weeks"),
      subtitle = glue::glue("{object$league_info$league_name}"),
      caption = glue::glue("ffsimulator R pkg | FP rankings as of {object$latest_rankings$scrape_date[[1]]}")
    )

}

#' @rdname autoplot.ff_simulation
#' @param x A `ff_simulation` object.
#' @param y Ignored, required for compatibility with the `plot()` generic.
#' @export
plot.ff_simulation <- function(x, ..., type = c("wins", "rank", "points"), y) {
  type <- match.arg(type)
  ggplot2::autoplot(x, type = type, ...)
}
