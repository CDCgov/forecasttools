quantile_forecasts <- example_daily_forecast_flu |>
  trajectories_to_quantiles(
    id_cols = "location",
    timepoint_cols = "date",
    value_col = "hosp"
  ) |>
  dplyr::rename(target_end_date = date)

test_that(
  paste0(
    "get_hubverse_table errors if reference date ",
    "is the wrong day of the week"
  ),
  {
    expect_error(
      forecasttools::get_hubverse_table(
        tibble::tibble(),
        "2025-01-01"
      ),
      "4"
    )
    expect_error(
      forecasttools::get_hubverse_table(
        tibble::tibble(),
        "2025-01-01",
        week_start = 3
      ),
      "1"
    )
    expect_error(
      forecasttools::get_hubverse_table(
        tibble::tibble(),
        "2025-01-01",
        reference_dow = 3
      ),
      "3"
    )
  }
)

test_that("get_hubverse_table handles different horizon_timescales correctly", {
  expect_setequal(
    get_hubverse_table(quantile_forecasts,
      reference_date = "2023-10-21",
      horizon_timescale = "days",
      target_name = "my_target",
      timepoint_cols = "target_end_date"
    ) |>
      dplyr::distinct(target_end_date) |>
      dplyr::pull(),
    as.Date(c("2023-10-21", "2023-10-22", "2023-10-23", "2023-10-24"))
  )
  expect_setequal(
    get_hubverse_table(quantile_forecasts,
      reference_date = "2023-10-21",
      horizon_timescale = "weeks",
      target_name = "my_target",
      timepoint_cols = "target_end_date"
    ) |>
      dplyr::distinct(target_end_date) |>
      dplyr::pull(),
    as.Date(c("2023-10-21", "2023-10-28", "2023-11-04", "2023-11-11"))
  )
  expect_error(get_hubverse_table(quantile_forecasts,
    reference_date = "2023-10-21",
    horizon_timescale = "seconds",
    target_name = "my_target",
    timepoint_cols = "target_end_date"
  ))
})



test_that("get_hubverse_table handles improper timepoint_cols correctly", {
  expect_error(get_hubverse_table(quantile_forecasts,
    reference_date = "2023-10-21",
    horizon_timescale = "days",
    target_name = "my_target",
    timepoint_cols = "blah"
  ))
  expect_error(get_hubverse_table(quantile_forecasts,
    reference_date = "2023-10-21",
    horizon_timescale = "days",
    target_name = "my_target",
    timepoint_cols = "quantile_value"
  ))
})
