# ffsimulator (development version)

## New features
- `ff_simulate()` gains a a new `verbose` argument that allows for progress tracking, and is set ON by default. This can also be controlled by `options(ffsimulator.verbose = TRUE)` (or FALSE). Resolves #9. (v1.0.0.01)
- `ffs_copy_template()` copies a template for custom simulations to the desired filepath, making it easier to run a simulation from the component level. Resolves #12 (v1.0.0.02)
- Column assertions cleaned up and should be much clearer on each error message, resolves #16. (v1.0.0.03)
- Refactored `ffs_optimise_lineups()` and `ffs_score_rosters()` to use data.table, ***deprecates*** parallel options (v1.0.0.04)
- Refactored `ffs_build_schedules()` column names to match syntax from `ff_schedule()`- `team` is now `franchise_id` and `opponent` is now `opponent_id`. ***BREAKING, for custom sims***
- `ffs_build_schedules()` is now responsible for joining franchises to the schedule, not `ffs_summarise_week()`.
- `ffs_build_schedules()` now takes a franchises dataframe as created by `ffs_franchises()` (v1.0.0.05)
- `autoplot()` refactored to hide legend and axis titles via the geom/scale - this allows easier theme adjustments.
- `ffs_repeat_schedules()` takes in a schedule as created by and expands it by `n_seasons`. This wraps up the actual_schedule feature. (v1.0.0.06)

## Minor changes

---

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
