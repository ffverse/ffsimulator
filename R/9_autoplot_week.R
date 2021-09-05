
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
#' simulation <- ff_simulate_week(mfl_connect(2021,54040), actual_schedule = TRUE)
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

  if (!requireNamespace("ggplot2", quietly = TRUE) && !requireNamespace("forcats", quietly = TRUE)) {
    stop("`ggplot2` and `forcats` must be installed to use `autoplot`.", call. = FALSE)
  }

  if (type %in% c("wins", "points") && !requireNamespace("ggridges", quietly = TRUE)) {
    stop("`ggridges` must be installed to use `type=\"points\"` option.", call. = FALSE)
  }

  switch(type,
         "luck" = p <- .ffs_plot_week_scheduleluck(object, ...),
         "points" = p <- .ffs_plot_week_points(object, ...)
  )
  p
}

#' #' @keywords internal
.ffs_plot_week_wins <- function(object, ...) {
  object$summary_week %>%
    dplyr::mutate(franchise_name = forcats::fct_reorder(.f = .data$franchise_name, .x = .data$allplay_wins)) %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$allplay_wins,
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
      breaks = seq.int(0, max(object$summary_season$allplay_wins) + 1, by = 2)
    ) +
    ggplot2::xlab("All-Play Wins") +
    ggplot2::ylab(NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      # legend.position = "none",
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      plot.title.position = "plot"
    ) +
    ggplot2::labs(
      title = glue::glue("All-Play Win Totals - {object$simulation_params$n} Simulated Weeks"),
      subtitle = glue::glue("{object$league_info$league_name}"),
      caption = glue::glue("ffsimulator R pkg | Based on rankings as of {object$simulation_params$scrape_date}")
    )
}

.ffs_plot_week_scheduleluck <- function(object, ...) {

  if(!object$simulation_params$actual_schedule) stop("Schedule Luck plot not available if `actual_schedule` is FALSE")

  luck <- object$summary_simulation %>%
    dplyr::mutate(franchise_name = forcats::fct_reorder(.f = .data$franchise_name,
                                                        .x = .data$h2h_winpct),
                  luck_pct = .data$h2h_winpct - .data$allplay_winpct,
                  luck_label = scales::percent(.data$luck_pct, accuracy = 0.1),
                  ap_hjust = ifelse(.data$luck_pct > 0, 1.1, -0.1),
                  h2h_hjust = ifelse(luck_pct > 0, -0.1, 1.1)
    )

  # dplyr::select("franchise_name", "h2h"="h2h_winpct", "ap"="allplay_winpct") %>%
  luck %>%
    ggplot2::ggplot(
      ggplot2::aes(
        y = .data$franchise_name,
        color = .data$franchise_name
      )
    ) +
    # ggplot2::geom_point(ggplot2::aes(x=.data$h2h_winpct), size = 2, shape = 8) +
    ggplot2::geom_point(ggplot2::aes(x=.data$allplay_winpct), alpha = 0.8, size = 2) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = .data$allplay_winpct,
        xend = .data$h2h_winpct,
        y = .data$franchise_name,
        yend = .data$franchise_name

      ),
      size = 1,
      alpha = 0.75,
      lineend = "round",
      linejoin = "mitre",
      arrow = ggplot2::arrow(angle = 30,length = ggplot2::unit(4,"points"),type = "closed")) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = (.data$allplay_winpct + .data$h2h_winpct)/2,
        # y = .data$franchise_name,
        label = .data$luck_label),
      # inherit.aes = FALSE,
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
      data = luck %>% dplyr::filter(.data$h2h_winpct == max(.data$h2h_winpct))
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = .data$allplay_winpct,
        y = .data$franchise_name,
        hjust = .data$ap_hjust
      ),
      label = "AllPlay Win %",
      data = luck %>% dplyr::filter(.data$h2h_winpct == max(.data$h2h_winpct))
    ) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(),limits = c(0, 1))+
    ggplot2::scale_color_discrete(guide = "none")+
    ggplot2::xlab("Win Percentage") +
    ggplot2::ylab(NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      # legend.position = "bottom",
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
  object$summary_week %>%
    dplyr::mutate(franchise_name = forcats::fct_reorder(.f = .data$franchise_name, .x = .data$team_score)) %>%
    ggplot2::ggplot(ggplot2::aes(
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
      title = glue::glue("Weekly Scores - {object$simulation_params$n} Simulated Weeks"),
      subtitle = glue::glue("{object$league_info$league_name}"),
      caption = glue::glue("ffsimulator R pkg | Based on rankings as of {object$simulation_params$scrape_date}")
    )
}

#' @rdname autoplot.ff_simulation_week
#' @param x A `ff_simulation_week` object.
#' @param y Ignored, required for compatibility with the `plot()` generic.
#' @export
plot.ff_simulation_week <- function(x, ..., type = c("wins", "rank", "points"), y) {
  if (!requireNamespace("ggplot2", quietly = TRUE) && !requireNamespace("forcats", quietly = TRUE)) {
    stop("`ggplot2` and `forcats` must be installed to use `autoplot`.", call. = FALSE)
  }

  type <- rlang::arg_match(type)

  ggplot2::autoplot(x, type = type, ...)
}
