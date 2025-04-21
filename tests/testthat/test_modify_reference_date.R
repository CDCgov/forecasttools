reference_date <- as.Date("2023-10-20")

ex_quantiles <- example_daily_forecast_flu |>
  trajectories_to_quantiles(
    id_cols = "location",
    timepoint_col = "date",
    value_col = "hosp"
  ) |>
  dplyr::rename(target_end_date = date)

original_hub_tbl_days <-
  get_hubverse_quantile_table(
    quantile_forecasts = ex_quantiles,
    reference_date = reference_date,
    horizon_timescale = "days",
    target_name = "my_target",
    timepoint_col = "target_end_date",
    horizons = seq(7, 28, by = 7)
  )

original_hub_tbl_weeks <-
  get_hubverse_quantile_table(
    quantile_forecasts = ex_quantiles,
    reference_date = reference_date,
    horizon_timescale = "weeks",
    target_name = "my_target",
    timepoint_col = "target_end_date",
    horizons = 1:4
  )

test_that("modify_reference_date converts daily to weekly horizons", {
  modified_tbl <- modify_reference_date(
    original_hub_tbl = original_hub_tbl_days,
    horizon_timescale = "weeks"
  )
  expect_equal(modified_tbl$horizon, original_hub_tbl_days$horizon / 7)
})

test_that("modify_reference_date converts weekly to daily horizons", {
  modified_tbl <- modify_reference_date(
    original_hub_tbl = original_hub_tbl_weeks,
    horizon_timescale = "days"
  )
  expect_equal(modified_tbl$horizon, original_hub_tbl_weeks$horizon * 7)
})

test_that("modify_reference_date warns when optional columns are missing", {
  incomplete_tbl <- original_hub_tbl_days %>%
    dplyr::select(-horizon, -horizon_timescale)

  expect_warning(
    modify_reference_date(
      original_hub_tbl = incomplete_tbl,
      horizon_timescale = "weeks"
    ),
    "will be appended"
  )
})

test_that("modify_reference_date applies reference_date_transform correctly", {
  modified_tbl <- modify_reference_date(
    original_hub_tbl = original_hub_tbl_days,
    reference_date_transform = function(x) as.Date(x) + 1,
    horizon_timescale = "days"
  )

  expect_equal(
    modified_tbl$reference_date,
    as.Date(original_hub_tbl_days$reference_date) + 1
  )
  expect_equal(
    modified_tbl$horizon,
    as.Date(original_hub_tbl_days$horizon) - 1
  )
})

test_that("modify_reference_date handles mixed timescales correctly", {
  original_mixed_timescale_tbl <- tibble::tibble(
    reference_date = rep(as.Date("2025-01-01"), 2),
    target_end_date = reference_date + lubridate::days(c(1, 7)),
    horizon_timescale = c("days", "weeks")
  ) |>
    dplyr::mutate(
      horizon = horizons_from_target_end_dates(
        horizon_timescale = horizon_timescale,
        target_end_dates = target_end_date,
        reference_date = reference_date
      )
    )
  result <- modify_reference_date(
    original_mixed_timescale_tbl,
    reference_date_transform = \(x) as.Date(x) - lubridate::days(7)
  )
  expect_equal(
    result$horizon_timescale,
    original_mixed_timescale_tbl$horizon_timescale
  )
  expect_equal(result$horizon, c(8, 2))
})
