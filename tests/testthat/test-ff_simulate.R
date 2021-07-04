with_mock_api({

  test_that("MFL simulation works", {

    fog <- mfl_connect(2021,22627)
    fog_sim <- ff_simulate(fog, n_seasons = 2)

    checkmate::expect_list(fog_sim, len = 5)
    checkmate::expect_tibble(fog_sim$summary_simulation, nrows = 12, any.missing = FALSE)
    checkmate::expect_tibble(fog_sim$summary_season, nrows = 24, any.missing = FALSE)
    checkmate::expect_tibble(fog_sim$summary_week, nrows = 336, any.missing = FALSE)
    checkmate::expect_tibble(fog_sim$latest_rankings, min.rows = 300)
    checkmate::expect_tibble(fog_sim$raw_data, min.rows = 300)

  })

  test_that("Sleeper simulation works", {
    jml <- ff_connect(platform = "sleeper", league_id = "652718526494253056", season = 2021)
    jml_sim <- ff_simulate(jml, n_seasons = 2)

    checkmate::expect_list(jml_sim, len = 5)
    checkmate::expect_tibble(jml_sim$summary_simulation, nrows = 12, any.missing = FALSE)
    checkmate::expect_tibble(jml_sim$summary_season, nrows = 24, any.missing = FALSE)
    checkmate::expect_tibble(jml_sim$summary_week, nrows = 336, any.missing = FALSE)
    checkmate::expect_tibble(jml_sim$latest_rankings, min.rows = 300)
    checkmate::expect_tibble(jml_sim$raw_data, min.rows = 300)
  })

  test_that("Fleaflicker simulation works", {

    got <- fleaflicker_connect(2020, 206154)
    got_sim <- ff_simulate(got, n_seasons = 2)

    checkmate::expect_list(got_sim, len = 5)
    checkmate::expect_tibble(got_sim$summary_simulation, nrows = 16, any.missing = FALSE)
    checkmate::expect_tibble(got_sim$summary_season, nrows = 32, any.missing = FALSE)
    checkmate::expect_tibble(got_sim$summary_week, nrows = 448, any.missing = FALSE)
    checkmate::expect_tibble(got_sim$latest_rankings, min.rows = 300)
    checkmate::expect_tibble(got_sim$raw_data, min.rows = 300)

  })

  test_that("ESPN simulation works", {

    tony <- espn_connect(season = 2020, league_id = 899513)
    tony_sim <- ff_simulate(tony, n_seasons = 2)

    checkmate::expect_list(tony_sim, len = 5)
    checkmate::expect_tibble(tony_sim$summary_simulation, nrows = 10, any.missing = FALSE)
    checkmate::expect_tibble(tony_sim$summary_season, nrows = 20, any.missing = FALSE)
    checkmate::expect_tibble(tony_sim$summary_week, nrows = 280, any.missing = FALSE)
    checkmate::expect_tibble(tony_sim$latest_rankings, min.rows = 300)
    checkmate::expect_tibble(tony_sim$raw_data, min.rows = 185)

  })

})
