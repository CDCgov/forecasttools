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
