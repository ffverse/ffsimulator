# ffsimulator (development version)

## Fixes
- Patch if all weeks are bye weeks as zero

# ffsimulator 1.2.0

This release of ffsimulator adds an experimental wins added calculation with `ff_wins_added()`, some cleanups, and try-handling to examples because apparently donttest doesn't exist.

## New features
- `ff_wins_added()` runs the simulation as desired once, then calculates the net wins added for each player over a replacement level player.
- `ffs_add_replacement_level()` adds replacement level players to every team that are essentially the lowest player at each position that is not currently rostered. 
- `ff_simulate()` and `ff_simulate_week()` gains a "return" parameter that controls elements to return - this provides more data so that wins_added can be calculated efficiently.  

## Fixes
- `verbose` argument primarily refers to the package option now. 
- Fix bye week handling for summarising actual schedule weeks by using inner join.

Immensely grateful to everyone who shared and used `ffsimulator` this year, and especially to [@JoeSydlowski](https://github.com/joesydlowski) for helping to build out the logic for wins added. 

---

# ffsimulator 1.1.0

This release of ffsimulator adds new features and refactors a lot of the backend for improved calculation efficiency.

## New features

- `ff_simulate_week()` is a new function (and supporting internal function family) that simulates upcoming inseason weeks with daily-updated upcoming week ranks.
- `ff_simulate()` gains an `actual_schedule` argument to simulate actual schedule + unplayed games.
- `ff_simulate()` and `ff_simulate_week()` gain a `verbose` argument that is set ON by default and can also be turned off with `options(ffsimulator.verbose = FALSE)`
- `ffs_copy_template()` helps build custom simulations by copying a template of starter code to a desired filepath.
- Simulations now support kickers (you can thank SFB for that!) and gain a `pos_filter` argument.

## Backend changes

Many backend changes - some are breaking (***grimaces and points at the experimental badge***).

- Parallel options removed in favour of data.table options. ***BREAKING*** - parallel is gone.
- Column assertions should be clearer on each error message.
- Many functions, including `ffs_optimise_lineups()`, `ffs_score_rosters()`, and more are now written in data.table - this helps improve speed for all sizes of simulations. 
- `injury_model` argument is now renamed to `gp_model`. ***BREAKING*** - argument name changed.
- `autoplot()` refactored to hide legend and axis titles via geom/scale and not via theme - this allows for much easier theming (since you can apply a whole new theme via `+`).
- `ffs_build_schedules()` now matches syntax from `ffscrapr::ff_schedule()` - `team` is now `franchise_id` and `opponent` is now `opponent_id` - ***BREAKING*** - output column names changed.
- `ffs_build_schedules()` is now responsible for joining franchises to the schedule, and not `ffs_summarise_week()`, and now takes a franchises dataframe as created by `ffs_franchises()` ***BREAKING***
- `ffs_repeat_schedules()` supports the actual_schedule feature by repeating it by `n_seasons`.
- Print methods cleaned up.

Immensely grateful to everyone who used and shared the first release of this package - it's been very motivating! Special thanks to [@JoeSydlowski](https://github.com/joesydlowski), [@topfunky](https://github.com/topfunky), [@mrcaseb](https://github.com/mrcaseb) for their contributions and feedback. 

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
