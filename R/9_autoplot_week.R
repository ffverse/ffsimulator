
#' Automatically Plot ff_simulation Object
#'
#' Creates automatic plots for wins, ranks, or points for an `ff_simulation` object as created by `ff_simulate()`.
#'
#' Usage of this function/method requires the ggplot2 package and (for wins and points plots) the ggridges package.
#'
#' @param object a `ff_simulation` object as created by `ff_simulate()`
#' @param type one of "luck" or "points"
#' @param ... unused, required by autoplot generic
#'
#' @examples
#' \donttest{
#'
#' simulation <- .ffs_cache("foureight_sim_week.rds")
#'
#' ggplot2::autoplot(simulation) # default is type = "luck"
#' ggplot2::autoplot(simulation, type = "points")
#' }
#'
#' @seealso `vignette("basic")` for example usage
#'
#' @return a ggplot object
#' @export
autoplot.ff_simulation_week <- function(object,
                                        type = c("luck","points"),
                                        ...) {
  type <- rlang::arg_match(type)

  if (!requireNamespace("ggplot2", quietly = TRUE) &&
      !requireNamespace("ggridges", quietly = TRUE)) {
    stop("`ggplot2` and `ggridges` must be installed to use `autoplot`.", call. = FALSE)
  }

  switch(type,
         "luck" = p <- .ffs_plot_week_luck(object, ...),
         "points" = p <- .ffs_plot_week_points(object, ...)
  )
  p
}

.ffs_plot_week_luck <- function(object, ...) {

  if(!object$simulation_params$actual_schedule) stop("Schedule luck plot not available if `actual_schedule` is FALSE")

  luck <- object$summary_simulation
  data.table::setDT(luck)
  h2h_winpct <- NULL
  allplay_winpct <- NULL
  luck_pct <- NULL

  luck <- luck[,`:=`(luck_pct = h2h_winpct - allplay_winpct)
  ][,`:=`(
    luck_label = scales::percent(luck_pct, accuracy = 0.1),
    ap_hjust = ifelse(luck_pct > 0, 1.1, -0.1),
    h2h_hjust = ifelse(luck_pct > 0, -0.1, 1.1)
    )
  ][order(h2h_winpct)]

  luck$franchise_name <- factor(luck$franchise_name, levels = luck$franchise_name)

  ggplot2::ggplot(
    luck,
    ggplot2::aes(
      y = .data$franchise_name,
      color = .data$franchise_name
    )
  ) +
    ggplot2::geom_point(ggplot2::aes(x=.data$allplay_winpct), alpha = 0.8, size = 2) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = .data$allplay_winpct,
        xend = .data$h2h_winpct,
        y = .data$franchise_name,
        yend = .data$franchise_name

      ),
      linewidth = 1,
      alpha = 0.75,
      lineend = "round",
      linejoin = "mitre",
      arrow = ggplot2::arrow(angle = 30,length = ggplot2::unit(6, "points"),type = "closed")) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = (.data$allplay_winpct + .data$h2h_winpct)/2,
        label = .data$luck_label),
      hjust = 0.5,
      vjust = -0.75
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = .data$h2h_winpct,
        y = .data$franchise_name,
        hjust = .data$h2h_just
      ),
      label = "H2H Win %",
      hjust = -0.1,
      data = luck[.N]
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = .data$allplay_winpct,
        y = .data$franchise_name,
        hjust = .data$ap_hjust
      ),
      label = "AllPlay Win %",
      data = luck[.N]
    ) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(),limits = c(0, 1))+
    ggplot2::scale_color_discrete(guide = "none")+
    ggplot2::xlab("Win Percentage") +
    ggplot2::ylab(NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      plot.title.position = "plot"
    ) +
    ggplot2::labs(
      title = glue::glue("Schedule Luck"),
      subtitle = glue::glue("{object$league_info$league_name} - {object$simulation_params$n} Simulated Weeks"),
      caption = glue::glue("ffsimulator R pkg | Based on rankings as of {object$simulation_params$scrape_date}")
    )
}

#' @keywords internal
.ffs_plot_week_points <- function(object, ...) {

  sw <- object$summary_week
  data.table::setDT(sw)
  team_score <- NULL
  sw_levels <- sw[,.(team_score = stats::median(team_score, na.rm = TRUE)),by = c("franchise_name")][order(team_score)]
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
      title = glue::glue("Week Scores - {object$simulation_params$n} Simulated Weeks"),
      subtitle = glue::glue("{object$league_info$league_name}"),
      caption = glue::glue("ffsimulator R pkg | Based on rankings as of {object$simulation_params$scrape_date}")
    )
}

#' @rdname autoplot.ff_simulation_week
#' @param x A `ff_simulation_week` object.
#' @param y Ignored, required for compatibility with the `plot()` generic.
#' @export
plot.ff_simulation_week <- function(x, ..., type = c("luck", "points"), y) {
  if (!requireNamespace("ggplot2", quietly = TRUE) &&
      !requireNamespace("ggridges", quietly = TRUE)) {
    stop("`ggplot2` and `ggridges` must be installed to use `plot`.", call. = FALSE)
  }

  type <- rlang::arg_match(type)

  ggplot2::autoplot(x, type = type, ...)
}
