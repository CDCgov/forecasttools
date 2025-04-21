create_hubverse_table <- function(
  date_cols,
  horizon,
  location,
  output_type,
  output_type_id,
  target
) {
  data <- tidyr::expand_grid(
    reference_date = date_cols,
    horizon = horizon,
    output_type_id = output_type_id,
    location = location,
    target = target
  ) |>
    dplyr::group_by(reference_date, horizon, location, target) |>
    dplyr::mutate(
      value = sort(
        sample(1:100, dplyr::n(), replace = TRUE),
        decreasing = FALSE
      ),
      output_type = "quantile",
      target_end_date = reference_date + 7 * horizon
    ) |>
    dplyr::ungroup()

  return(data)
}


create_observation_data <- function(
  date,
  location,
  target
) {
  data <- tidyr::expand_grid(
    reference_date = date,
    location = location,
    target = target
  ) |>
    dplyr::mutate(value = sample(1:100, dplyr::n(), replace = TRUE))
  return(data)
}

forecast <- create_hubverse_table(
  date_cols = seq(
    lubridate::ymd("2023-11-01"),
    lubridate::ymd("2024-01-29"),
    by = "day"
  ),
  horizon = c(0, 1, 2),
  location = c("loc1", "loc2"),
  target = c(
    "wk inc pathogen ed visits",
    "daily prev pathogen admissions"
  ),
  output_type = "quantile",
  output_type_id = c(0.01, 0.025, 1:19 / 20, 0.975, 0.99)
)

observed <- create_observation_data(
  date = seq(
    lubridate::ymd("2023-11-01"),
    lubridate::ymd("2024-01-29"),
    by = "day"
  ),
  location = c("loc1", "loc2"),
  target = c(
    "wk inc pathogen ed visits",
    "daily prev pathogen admissions"
  )
)


testthat::test_that(
  paste0(
    "hubverse_table_with_obs errors ",
    "when needed columns are missing"
  ),
  {
    expect_error(
      hubverse_table_with_obs(
        forecast,
        observed,
        obs_value_column = "wrong"
      ),
      "wrong"
    )
    expect_error(
      hubverse_table_with_obs(
        forecast,
        observed,
        obs_date_column = "not_in_table"
      ),
      "not_in_table"
    )
    expect_error(
      hubverse_table_with_obs(
        forecast,
        observed,
        id_cols = "missing_id_col"
      ),
      "missing_id_col"
    )
  }
)

testthat::test_that(
  paste0(
    "quantile_table_to_scorable works as expected ",
    "with valid default inputs"
  ),
  {
    scorable <- quantile_table_to_scorable(
      forecast,
      observed,
      obs_date_column = "reference_date"
    )
    expect_true(scoringutils::is_forecast_quantile(scorable))
    expect_setequal(forecast$location, scorable$location)
    expect_setequal(observed$value, scorable$observed)
    expect_setequal(forecast$target, scorable$target)
  }
)


testthat::test_that(
  paste0(
    "quantile_table_to_scorable respects non-default inputs ",
    "with valid default inputs"
  ),
  {
    scorable <- quantile_table_to_scorable(
      forecast |> dplyr::rename(loc = "location"),
      observed |>
        dplyr::rename(
          different_value = "value",
          loc = "location"
        ),
      obs_date_column = "reference_date",
      obs_value_column = "different_value",
      id_cols = c("loc", "target")
    )
    expect_true(scoringutils::is_forecast_quantile(scorable))
    expect_setequal(forecast$location, scorable$loc)
    expect_setequal(observed$value, scorable$observed)
    expect_setequal(forecast$target, scorable$target)
  }
)


testthat::test_that(
  paste0(
    "quantile_table_to_scorable handles ",
    "missing location or target data"
  ),
  {
    forecast <- create_hubverse_table(
      date = seq(
        lubridate::ymd("2024-11-01"),
        lubridate::ymd("2024-11-29"),
        by = "day"
      ),
      horizon = c(0, 1),
      location = c("loc1", "loc2"),
      output_type = "quantile",
      target = c(
        "wk inc pathogen ed visits",
        "daily prev pathogen admissions"
      ),
      output_type_id = 1:19 / 20
    )

    observed <- create_observation_data(
      date = seq(
        lubridate::ymd("2024-11-01"),
        lubridate::ymd("2024-11-29"),
        by = "day"
      ),
      location = c("loc1"),
      target = c("wk inc pathogen ed visits")
    )

    result <- quantile_table_to_scorable(
      forecast,
      observed,
      obs_date_column = "reference_date"
    )
    expect_false("loc2" %in% result$location)
    expect_false("daily prev pathogen admissions" %in% result$target)
    expect_setequal(observed$location, result$location)
    expect_setequal(
      result$observed,
      observed$value
    )
    expect_setequal(observed$target, result$target)
  }
)


testthat::test_that(
  paste0(
    "quantile_table_to_scorable ",
    "handles zero length forecast table"
  ),
  {
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
      date = seq(
        lubridate::ymd("2024-11-01"),
        lubridate::ymd("2024-11-02"),
        by = "day"
      ),
      location = c("loc1"),
      target = "null target"
    )

    expect_error(
      result <- quantile_table_to_scorable(
        forecast,
        observed,
        obs_date_column = "reference_date"
      ),
      "Assertion on 'data' failed: Must have at least 1 rows, but has 0 rows."
    )
  }
)
