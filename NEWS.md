# forecasttools 0.1.5
* BREAKING CHANGE: `get_hubverse_table()` has been removed and replace by `get_hubverse_quantile_table()`, `get_flusight_hub_table()` and `get_covid_hub_table()`.
* `horizons_from_target_end_dates` is now vectorized
* New function: `hub_quantiles_to_median_qi()`

# forecasttools 0.1.4
* BREAKING CHANGE: Functions to prepare hubs for scoring (`hubverse_table_with_obs()` and `quantile_table_to_scorable()`) now no longer assume that forecast tables contain a single target. This will break existing scoring workflows that use truth data without a target column. It can be addressed by setting the `id_cols` argument `hubverse_table_with_obs()` or `quantile_table_to_scorable()` explicitly.
* New function for calculating forecast horizons from target end dates: `horizons_from_target_end_dates()`
* New epiweek rounding functions: `floor_mmwr_epiweek`, `ceiling_mmwr_epiweek()`, `floor_isoweek()`, and `ceiling_isoweek()`.
* Smaller-than-memory `parquet` file I/O now handled via [`nanoparquet`](https://nanoparquet.r-lib.org/) (previously handled via [`arrow`](https://arrow.apache.org/docs/r/), reducing `forecasttools` package weight and installation time.

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
