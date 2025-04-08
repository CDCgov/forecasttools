quantile_forecasts <- example_daily_forecast_flu |>
  trajectories_to_quantiles(
    id_cols = "location",
    timepoint_col = "date",
    value_col = "hosp"
  ) |>
  dplyr::rename(target_end_date = date)

test_that(
  paste0(
    "get_epiweekly_hubverse_table errors if reference date ",
    "is the wrong day of the week"
  ),
  {
    expect_error(
      forecasttools::get_flusight_hub_table(
        tibble::tibble(),
        "2025-01-01"
      ),
      "4"
    )
    expect_error(
      forecasttools::get_covid_hub_table(
        tibble::tibble(),
        "2025-01-01"
      ),
      "4"
    )
    expect_error(
      forecasttools::get_epiweekly_hubverse_table(
        tibble::tibble(),
        "2025-01-01",
        week_start = 3,
        reference_dow = 7
      ),
      "1"
    )
    expect_error(
      forecasttools::get_epiweekly_hubverse_table(
        tibble::tibble(),
        "2025-01-01",
        week_start = 7,
        reference_dow = 3
      ),
      "3"
    )
  }
)

test_that(
  "get_hubverse_quantile_table handles different horizon_timescales correctly",
  {
    expect_setequal(
      get_hubverse_quantile_table(quantile_forecasts,
        reference_date = "2023-10-21",
        horizon_timescale = "days",
        target_name = "my_target",
        timepoint_col = "target_end_date",
        horizons = -1:3
      ) |>
        dplyr::distinct(target_end_date) |>
        dplyr::pull(),
      as.Date(c("2023-10-21", "2023-10-22", "2023-10-23", "2023-10-24"))
    )
    expect_setequal(
      get_hubverse_quantile_table(quantile_forecasts,
        reference_date = "2023-10-21",
        horizon_timescale = "weeks",
        target_name = "my_target",
        timepoint_col = "target_end_date",
        horizons = -1:3
      ) |>
        dplyr::distinct(target_end_date) |>
        dplyr::pull(),
      as.Date(c("2023-10-21", "2023-10-28", "2023-11-04", "2023-11-11"))
    )
    expect_error(get_hubverse_quantile_table(quantile_forecasts,
      reference_date = "2023-10-21",
      horizon_timescale = "seconds",
      target_name = "my_target",
      timepoint_col = "target_end_date"
    ))
  }
)



test_that(
  "get_hubverse_quantile_table handles improper timepoint_col correctly",
  {
    expect_error(get_hubverse_quantile_table(quantile_forecasts,
      reference_date = "2023-10-21",
      horizon_timescale = "days",
      target_name = "my_target",
      timepoint_col = "blah"
    ))
    expect_error(get_hubverse_quantile_table(quantile_forecasts,
      reference_date = "2023-10-21",
      horizon_timescale = "days",
      target_name = "my_target",
      timepoint_col = "quantile_value"
    ))
  }
)


reference_date <- lubridate::ymd("2025-01-01")
n <- 5
test_that("horizons_from_target_end_dates works correctly", {
  expect_equal(
    horizons_from_target_end_dates(
      reference_date,
      seq(reference_date, by = "days", length.out = n),
      horizon_timescale = "days"
    ),
    0:(n - 1)
  )
  expect_equal(
    horizons_from_target_end_dates(
      reference_date,
      seq(reference_date, by = "weeks", length.out = n),
      horizon_timescale = "weeks"
    ),
    0:(n - 1)
  )
  expect_error(
    horizons_from_target_end_dates(
      reference_date,
      seq(reference_date, by = "days", length.out = n),
      horizon_timescale = "weeks"
    )
  )
  expect_equal(
    horizons_from_target_end_dates(
      reference_date = lubridate::ymd("2000-01-01"),
      target_end_dates = c(
        lubridate::ymd("2000-01-01") + lubridate::ddays(1),
        lubridate::ymd("2000-01-01") + lubridate::dweeks(1)
      ),
      horizon_timescale = c("days", "weeks")
    ),
    c(1, 1)
  )
})

test_that("target_end_dates_from_horizons works correctly", {
  expect_equal(
    target_end_dates_from_horizons(
      reference_date,
      0:(n - 1),
      horizon_timescale = "days"
    ),
    seq(reference_date, by = "days", length.out = n)
  )
  expect_equal(
    target_end_dates_from_horizons(
      reference_date,
      0:(n - 1),
      horizon_timescale = "weeks"
    ),
    seq(reference_date, by = "weeks", length.out = n)
  )
  expect_error(
    target_end_dates_from_horizons(
      reference_date,
      0:(n - 1),
      horizon_timescale = "seconds"
    )
  )
})
