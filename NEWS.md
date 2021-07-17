# ffsimulator 1.0.0

The `ffsimulator` package uses bootstrap resampling to run fantasy football season simulations supported by historical rankings and nflfastR data, calculating optimal lineups, and returning aggregated results.

This initial release introduces two major functions:

-   `ff_simulate()` (runs the main simulation)
-   `autoplot()` (visualizes the output of the simulation)

and also introduces a suite of subfunctions that power the main simulation, mostly prefixed with `ffs_`

-   `ffs_latest_rankings()`
-   `ffs_rosters()`
-   `ffs_adp_outcomes()`
-   `ffs_generate_projections()`
-   `ffs_score_rosters()`
-   `ffs_optimise_lineups()`
-   `ffs_build_schedules()`
-   `ffs_summarise_week()`
-   `ffs_summarise_season()`
-   `ffs_summarise_simulation()`

Finally, this first version includes two dataframes:

-   `fp_rankings_history` provides historical expert consensus rankings from 2012-2020
-   `fp_injury_table` calculates the odds of a player playing in any given game

Other features developed for initial release include:

-   `ffs_adp_outcomes()` bins together nearby ranks to broaden the bootstrapping sample
-   Bye week handling
-   Invalid lineups in `ffs_optimise_lineups()`
-   Unit and integration testing

Features postponed to subsequent versions:

-   Progress bar for verbose output
-   Investigating speedup packages for higher volumes
-   Using real ff_schedules rather than simulated schedules
-   Support for kicker scoring
