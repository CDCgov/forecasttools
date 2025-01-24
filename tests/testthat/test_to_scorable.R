create_hubverse_table <- function(
    date_cols, horizon, location, output_type, output_type_id) {
  data <- tidyr::expand_grid(
    reference_date = date_cols,
    horizon = horizon,
    output_type_id = output_type_id,
    location = location
  ) |>
    dplyr::group_by(reference_date, horizon, location) |>
    dplyr::mutate(
      value = sort(
        sample(1:100, dplyr::n(), replace = TRUE),
        decreasing = FALSE
      ),
      target = "wk inc covid prop ed visits",
      output_type = "quantile",
      target_end_date = reference_date + 7 * horizon
    ) |>
    dplyr::ungroup()

  return(data)
}


create_observation_data <- function(
    date_cols, location_cols) {
  data <- tidyr::expand_grid(
    reference_date = date_cols,
    location = location_cols
  ) |>
    dplyr::mutate(value = sample(1:100, dplyr::n(), replace = TRUE))
  return(data)
}


testthat::test_that(paste0(
  "quantile_table_to_scorable works as expected ",
  "with valid inputs"
), {
  forecast <- create_hubverse_table(
    date_cols = seq(
      lubridate::ymd("2023-11-01"), lubridate::ymd("2024-01-29"),
      by = "day"
    ),
    horizon = c(0, 1, 2),
    location = c("loc1", "loc2"),
    output_type = "quantile",
    output_type_id = c(0.01, 0.025, 1:19 / 20, 0.975, 0.99)
  )

  observed <- create_observation_data(
    date_cols = seq(
      lubridate::ymd("2023-11-01"), lubridate::ymd("2024-01-29"),
      by = "day"
    ),
    location = c("loc1", "loc2")
  )

  scorable <- quantile_table_to_scorable(
    forecast,
    observed,
    obs_date_column = "reference_date"
  )
  expect_true(scoringutils::is_forecast_quantile(scorable))
  expect_setequal(forecast$location, scorable$location)
})


testthat::test_that("score_hubverse handles missing location data", {
  forecast <- create_hubverse_table(
    date_cols = seq(
      lubridate::ymd("2024-11-01"), lubridate::ymd("2024-11-29"),
      by = "day"
    ),
    horizon = c(0, 1),
    location = c("loc1", "loc2"),
    output_type = "quantile",
    output_type_id = 1:19 / 20
  )

  observed <- create_observation_data(
    date_cols = seq(
      lubridate::ymd("2024-11-01"), lubridate::ymd("2024-11-29"),
      by = "day"
    ),
    location = c("loc1")
  )

  result <- quantile_table_to_scorable(
    forecast, observed,
    obs_date_column = "reference_date"
  )
  expect_false("loc2" %in% result$location)
  expect_setequal(observed$location, result$location)
})


testthat::test_that(paste0(
  "quantile_table_to_scorable ",
  "handles zero length forecast table"
), {
  forecast <- tibble::tibble(
    reference_date = as.Date(character(0)),
    horizon = integer(0),
    output_type_id = numeric(0),
    location = character(0),
    value = numeric(0),
    target = character(0),
    output_type = character(0),
    target_end_date = as.Date(character(0))
  )

  observed <- create_observation_data(
    date_cols = seq(
      lubridate::ymd("2024-11-01"), lubridate::ymd("2024-11-02"),
      by = "day"
    ),
    location = c("loc1")
  )

  expect_error(
    result <- quantile_table_to_scorable(
      forecast, observed,
      obs_date_column = "reference_date"
    ),
    "Assertion on 'data' failed: Must have at least 1 rows, but has 0 rows."
  )
})
