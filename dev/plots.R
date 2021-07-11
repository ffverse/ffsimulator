library(ggplot2)
library(ggridges)
library()

#### Basic Season Wins ####

sims$summary_season %>%
  mutate(franchise_name = fct_reorder(.f = franchise_name, .x = h2h_wins)) %>%
  ggplot(aes(x = h2h_wins,
             y = franchise_name,
             fill = franchise_name)) +
  geom_density_ridges(
    stat = "binline",
    color = "white",
    binwidth = 1,
    scale = 1.3,
    alpha = 0.8
    ) +
  scale_x_continuous(breaks = seq.int(0, max(sims$summary_season$h2h_wins)+1, by = 2)) +
  xlab("Season Wins") +
  ylab(NULL) +
  theme_minimal()+
  theme(
    legend.position = "none",
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor.x = ggplot2::element_blank(),
    plot.title.position = "plot"
  ) +
  labs(
    title = glue::glue("Season Win Totals - {sims$simulation_params$n_seasons} Simulated Seasons"),
    subtitle = glue::glue("{sims$league_info$league_name}"),
    caption = glue::glue("ffsimulator R pkg | FP rankings as of {sims$latest_rankings$scrape_date[[1]]}")
  )

#### Season Rankings ####

sims$summary_season %>%
  group_by(season) %>%
  mutate(season_rank = rank(-h2h_wins,ties.method = "min")) %>%
  ungroup() %>%
  mutate(
    franchise_name = fct_reorder(.f = franchise_name, .x = season_rank),
    rank_label = scales::ordinal(season_rank) %>% fct_reorder(season_rank),
         ) %>%
  ggplot(aes(x = franchise_name,color = franchise_name, fill = franchise_name)) +
  geom_bar() +
  facet_wrap(~rank_label) +
  xlab(NULL) +
  ylab("Number of Seasons") +
  theme_minimal() +
  theme(
    axis.text.x = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  ) +
  labs(
    title = glue::glue("Final Season Rank - {sims$simulation_params$n_seasons} Simulated Seasons"),
    subtitle = glue::glue("{sims$league_info$league_name}"),
    caption = glue::glue("ffsimulator R pkg | FP rankings as of {sims$latest_rankings$scrape_date[[1]]}"),
    fill = "Franchise Name",
    color = "Franchise Name"
  )

#### Season Points ####

sims$summary_week %>%
  mutate(franchise_name = fct_reorder(.f = franchise_name, .x = team_score)) %>%
  ggplot(aes(x = team_score,
             y = franchise_name,
             fill = franchise_name)) +
  geom_density_ridges(
    color = "white",
    quantile_lines = TRUE,
    # bandwidth = 1,
    scale = 1.3,
    alpha = 0.8
  ) +
  scale_x_continuous(n.breaks = 8) +
  xlab("Weekly Score") +
  ylab(NULL) +
  theme_minimal()+
  theme(
    legend.position = "none",
    panel.grid.major.y = ggplot2::element_blank(),
    plot.title.position = "plot"
  ) +
  labs(
    title = glue::glue("Weekly Scores - {sims$simulation_params$n_seasons * sims$simulation_params$weeks_per_season} Simulated Weeks"),
    subtitle = glue::glue("{sims$league_info$league_name}"),
    caption = glue::glue("ffsimulator R pkg | FP rankings as of {sims$latest_rankings$scrape_date[[1]]}")
  )

