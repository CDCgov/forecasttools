# forecasttools 0.1.3
* BREAKING CHANGE: `target_end_dates_from_horizons()` and `get_hubverse_table()` now work with both daily and weekly horizons. To restore functionality of existing code, add the argument `horizon_timescale = "weeks"` to these functions.

# forecasttools 0.1.2
* Adds optional epidate annotation to `daily_to_epiweekly()`
* Increases testing coverage
* Fixes bug in `pull_nhsn()` so that ordering on multiple columns works as expected.

# forecasttools 0.1.1
* Makes target data path configurable in `hub_to_scorable_quantiles()`
* Fixes small bugs.

# forecasttools 0.1.0

* Initial development release.
