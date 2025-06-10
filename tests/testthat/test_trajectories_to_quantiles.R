test_that("trajectories_to_quantiles works as expected with default args", {
  dat <- forecasttools::example_daily_forecast_flu |>
    dplyr::rename(
      value = "hosp",
      timepoint = "date"
    ) |>
    dplyr::filter(location == "NM")

  n_quants <- dplyr::n_distinct(c(
    0.01,
    0.025,
    seq(0.05, 0.95, 0.05),
    0.975,
    0.99
  ))
  n_locs <- dplyr::n_distinct(dat$location)
  n_dates <- dplyr::n_distinct(dat$timepoint)

  quants <- trajectories_to_quantiles(
    dat
  )

  expect_equal(nrow(quants), n_quants * n_locs * n_dates)
  checkmate::expect_names(
    names(quants),
    identical.to = c(
      "timepoint",
      "quantile_value",
      "quantile_level"
    )
  )
})

test_that("trajectories_to_quantiles works as expected with custom args", {
  dat <- forecasttools::example_daily_forecast_flu
  quants_to_use <- c(0.2, 0.752, 0.3)
  n_quants <- dplyr::n_distinct(quants_to_use)
  n_locs <- dplyr::n_distinct(dat$location)
  n_dates <- dplyr::n_distinct(dat$date)

  quants <- trajectories_to_quantiles(
    dat,
    timepoint_col = "date",
    value_col = "hosp",
    id_cols = "location",
    quantiles = quants_to_use,
    quantile_value_name = "x_value",
    quantile_level_name = "y_level"
  )

  expect_equal(nrow(quants), n_quants * n_locs * n_dates)
  checkmate::expect_names(
    names(quants),
    identical.to = c(
      "date",
      "location",
      "x_value",
      "y_level"
    )
  )
})

test_that("trajectories_to_quantiles works with missing values", {
  timepoint_to_na <- as.Date("2023-10-21")
  draw_to_na <- 18
  dat <- forecasttools::example_daily_forecast_flu |>
    dplyr::rename(
      value = "hosp",
      timepoint = "date"
    ) |>
    dplyr::filter(location == "NM") |>
    dplyr::mutate(
      value = dplyr::if_else(
        timepoint == timepoint_to_na & .draw == draw_to_na,
        NaN,
        value
      )
    )

  quants <- trajectories_to_quantiles(dat)

  present_timepoints <- unique(quants$timepoint)
  expected_timepoints <- setdiff(unique(dat$timepoint), timepoint_to_na)
  expect_true(setequal(present_timepoints, expected_timepoints))
})
