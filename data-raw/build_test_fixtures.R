library(tidyverse)
library(ffscrapr)

mfl_conn <- ffscrapr::mfl_connect(season = 2021, league_id = 22627)

mfl_scoring_history <- ffscrapr::ff_scoringhistory(mfl_conn, 2015:2020) %>%
  select(season, week, gsis_id, sportradar_id, mfl_id, player_name, pos, team, points)

mfl_rosters <- ffscrapr::ff_rosters(mfl_conn) %>%
  select(-salary, -contract_years, -roster_status, -draft_year, -draft_round)

mfl_lineup_constraints <- ffscrapr::ff_starter_positions(mfl_conn)

saveRDS(mfl_conn, "tests/testthat/cache/mfl_conn.rds")
saveRDS(mfl_scoring_history, "tests/testthat/cache/mfl_scoring_history.rds")
saveRDS(mfl_rosters, "tests/testthat/cache/mfl_rosters.rds")
saveRDS(mfl_lineup_constraints, "tests/testthat/cache/mfl_lineup_constraints.rds")

sleeper_conn <- ff_connect(platform = "sleeper", league_id = "652718526494253056", season = 2021)

sleeper_scoring_history <- ffscrapr::ff_scoringhistory(sleeper_conn, 2015:2020) %>%
  select(season, week, gsis_id, sportradar_id, sleeper_id, player_name, pos, team, points)

sleeper_rosters <- ffscrapr::ff_rosters(sleeper_conn)

sleeper_lineup_constraints <- ffscrapr::ff_starter_positions(sleeper_conn)

saveRDS(sleeper_conn, "tests/testthat/cache/sleeper_conn.rds")
saveRDS(sleeper_scoring_history, "tests/testthat/cache/sleeper_scoring_history.rds")
saveRDS(sleeper_rosters, "tests/testthat/cache/sleeper_rosters.rds")
saveRDS(sleeper_lineup_constraints, "tests/testthat/cache/sleeper_lineup_constraints.rds")

fleaflicker_conn <- fleaflicker_connect(2020, 206154)

fleaflicker_scoring_history <- ffscrapr::ff_scoringhistory(fleaflicker_conn, 2015:2020) %>%
  select(season, week, gsis_id, sportradar_id, player_name, pos, team, points)

fleaflicker_rosters <- ffscrapr::ff_rosters(fleaflicker_conn)

fleaflicker_lineup_constraints <- ffscrapr::ff_starter_positions(fleaflicker_conn)

saveRDS(fleaflicker_conn, "tests/testthat/cache/fleaflicker_conn.rds")
saveRDS(fleaflicker_scoring_history, "tests/testthat/cache/fleaflicker_scoring_history.rds")
saveRDS(fleaflicker_rosters, "tests/testthat/cache/fleaflicker_rosters.rds")
saveRDS(fleaflicker_lineup_constraints, "tests/testthat/cache/fleaflicker_lineup_constraints.rds")

espn_conn <- espn_connect(season = 2020, league_id = 899513)

espn_scoring_history <- ffscrapr::ff_scoringhistory(espn_conn, 2015:2020) %>%
  select(season, week, gsis_id, sportradar_id, player_name, pos, team, points)

espn_rosters <- ffscrapr::ff_rosters(espn_conn)

espn_lineup_constraints <- ffscrapr::ff_starter_positions(espn_conn)

saveRDS(espn_conn, "tests/testthat/cache/espn_conn.rds")
saveRDS(espn_scoring_history, "tests/testthat/cache/espn_scoring_history.rds")
saveRDS(espn_rosters, "tests/testthat/cache/espn_rosters.rds")
saveRDS(espn_lineup_constraints, "tests/testthat/cache/espn_lineup_constraints.rds")

latest_rankings <- ffs_latest_rankings()
saveRDS(latest_rankings, "tests/testthat/cache/latest_rankings.rds")
