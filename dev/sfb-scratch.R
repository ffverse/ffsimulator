# library(ffsimulator)
pkgload::load_all()
library(ffscrapr)
library(tidyverse)
library(furrr)

plan(multisession)

conn <- mfl_connect(2021, 47747)

league_info <- ffscrapr::ff_league(conn)

scoring_history <- ffscrapr::ff_scoringhistory(conn, 2012:2020)

adp_outcomes <- ffs_adp_outcomes(scoring_history = scoring_history, injury_model = "simple")

latest_rankings <- ffs_latest_rankings()

# projected_scores <- ffs_generate_projections(scoring_history, adp_outcomes, n_season, n_weeks)

rosters <- ffs_rosters(conn) %>%
  left_join(
    ff_franchises(conn) %>% select(franchise_id,division_name),
    by = "franchise_id"
  )

lineup_constraints <- ffscrapr::ff_starter_positions(conn)

#### GENERATE PREDICTIONS ####
n_seasons <- 100
n_weeks <- 13

projected_scores <- ffs_generate_projections(adp_outcomes = adp_outcomes,
                                             latest_rankings = latest_rankings,
                                             n_seasons = n_seasons,
                                             n_weeks = n_weeks,
                                             rosters = rosters)

roster_scores <- ffs_score_rosters(rosters, projected_scores)

#### OPTIMIZE LINEUPS ####

optimal_scores <- ffs_optimize_lineups(
  roster_scores = roster_scores,
  lineup_constraints = lineup_constraints,
  best_ball = FALSE,
  parallel = TRUE)

#### GENERATE SCHEDULES ####

schedules <- ffs_build_schedules(n_teams = length(unique(rosters$franchise_id)),
                                 n_seasons = n_seasons,
                                 n_weeks = n_weeks)

#### SUMMARISE SEASON ####

summary_week <- ffs_summarise_week(optimal_scores, schedules)
summary_season <- ffs_summarise_season(summary_week)
summary_simulation <- ffs_summarise_simulation(summary_season)

out <-
  structure(
    list(
      summary_simulation = summary_simulation,
      summary_season = summary_season,
      summary_week = summary_week,
      projected_scores = roster_scores,
      latest_rankings = latest_rankings,
      league_info = league_info,
      simulation_params = list(
        n_seasons = n_seasons,
        weeks_per_season = n_weeks,
        best_ball = FALSE,
        seed = NULL,
        injury_model = "simple",
        base_seasons = 2012:2020
      )
    ),
    class = "ff_simulation"
  )

autoplot(out)
autoplot(out, type = "rank")
autoplot(out, type = "points")

out$summary_season %>%
  dplyr::mutate(franchise_name = forcats::fct_reorder(.f = .data$franchise_name, .x = .data$points_for)) %>%
  ggplot2::ggplot(
    ggplot2::aes(x = .data$points_for,
                 y = .data$franchise_name,
                 fill = .data$franchise_name)) +
  ggridges::geom_density_ridges(
    color = "white",
    scale = 1.3,
    alpha = 0.8,
    quantile_lines = TRUE
  ) +
  ggplot2::xlab("Season Points") +
  ggplot2::ylab(NULL) +
  ggplot2::theme_minimal()+
  ggplot2::theme(
    legend.position = "none",
    panel.grid.major.y = ggplot2::element_blank(),
    plot.title.position = "plot"
  ) +
  ggplot2::labs(
    title = glue::glue("Season Points - {out$simulation_params$n_seasons} Simulated Seasons"),
    subtitle = glue::glue("{out$league_info$league_name}"),
    caption = glue::glue("ffsimulator R pkg | FP rankings as of {out$latest_rankings$scrape_date[[1]]}")
  )
