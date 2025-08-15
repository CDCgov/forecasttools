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

test_that(
  paste0(
    "with_hubverse_oracle_output() raises errors ",
    "for incompatible column names or types"
  ),
  {
    # Test case: oracle_value column present in hubverse_table
    hubverse_table <- tibble::tibble(
      location = "US",
      target_end_date = as.Date("2023-01-01"),
      target = "wk inc flu hosp",
      output_type = "quantile",
      output_type_id = "0.5",
      value = 100,
    )

    oracle_output_table <- tibble::tibble(
      location = "US",
      target_end_date = as.Date("2023-01-01"),
      target = "wk inc flu hosp",
      output_type = "quantile",
      output_type_id = "0.5",
      oracle_value = 95
    )

    result <- with_hubverse_oracle_output(hubverse_table, oracle_output_table)
    expect_equal(result, hubverse_table |> dplyr::mutate(oracle_value = 95))

    expect_error(
      with_hubverse_oracle_output(
        hubverse_table |> dplyr::mutate(oracle_value = 503),
        oracle_output_table
      ),
      "oracle_value"
    )
    expect_error(
      with_hubverse_oracle_output(
        hubverse_table,
        oracle_output_table |> dplyr::mutate(extra_column = "not in table")
      ),
      "extra_column"
    )
    expect_error(
      with_hubverse_oracle_output(
        hubverse_table,
        oracle_output_table |>
          dplyr::mutate(target_end_date = as.character(.data$target_end_date))
      ),
      "incompatible types"
    )
  }
)

test_that("with_hubverse_oracle_output handles mixed output types correctly", {
  # hubverse table with mixed output types and out of order levels
  hubverse_table <- tibble::tibble(
    location = rep("US", 7),
    target_end_date = rep(as.Date("2023-01-01"), 7),
    target = c(
      "wk inc flu ed visits",
      "wk inc flu hosp",
      "wk inc flu hosp",
      "wk flu burden level",
      "wk inc flu hosp",
      "wk flu burden level",
      "wk inc flu ed visits"
    ),
    output_type = c(
      "cdf",
      "quantile",
      "quantile",
      "pmf",
      "quantile",
      "pmf",
      "cdf"
    ),
    output_type_id = c("200", "0.5", "0.75", "high", "0.25", "low", "105"),
    value = c(0.95, 100, 120, 0.7, 80, 0.3, 0.8)
  )

  oracle_output <- tibble::tibble(
    location = c("US", "US", "US"),
    target_end_date = c(
      as.Date("2023-01-01"),
      as.Date("2023-01-01"),
      as.Date("2023-01-01")
    ),
    target = c(
      "wk inc flu hosp",
      "wk flu burden level",
      "wk inc flu ed visits"
    ),
    output_type = c("quantile", "pmf", "cdf"),
    output_type_id = c(NA_character_, "low", 105),
    # NA for quantile, specific IDs for pmf/cdf
    oracle_value = c(95, 0.4, 0.8)
  )

  result <- with_hubverse_oracle_output(hubverse_table, oracle_output)

  # all original rows preserved, oracle_value column added
  expect_true("oracle_value" %in% names(result))
  expect_equal(nrow(result), 7)

  ## non-"need_id" rows (here "quantile") are handled correctly
  ## despite the NA output_type_ids in the oracle table
  quantile_rows <- dplyr::filter(result, .data$output_type == "quantile")
  expect_equal(quantile_rows$oracle_value, rep(95, 3))
  expect_true(all(!is.na(quantile_rows$output_type_id)))

  ## Check that pmf row with matching output_type_id gets correct oracle_value
  pmf_low_row <- dplyr::filter(
    result,
    .data$output_type == "pmf",
    .data$output_type_id == "low"
  )
  expect_equal(pmf_low_row$oracle_value, 0.4)

  ## Check while pmf row with non-matching output_type_id gets NA oracle_value

  pmf_high_row <- dplyr::filter(
    result,
    .data$output_type == "pmf",
    .data$output_type_id == "high"
  )
  expect_true(is.na(pmf_high_row$oracle_value))

  ## cdf row with matching output_type_id gets correct oracle_value
  cdf_row_match <- dplyr::filter(
    result,
    .data$output_type == "cdf",
    .data$output_type_id == "105"
  )
  expect_equal(cdf_row_match$oracle_value, 0.8)

  ## Check that cdf row with unmatched output_type_id gets NA
  cdf_row_match <- dplyr::filter(
    result,
    .data$output_type == "cdf",
    .data$output_type_id == "200"
  )
  expect_true(is.na(cdf_row_match$oracle_value))
})

test_that("with_hubverse_oracle_output preserves all hubverse table rows", {
  hubverse_table <- tibble::tibble(
    location = "US",
    target_end_date = as.Date(c("2023-01-01", "2023-01-01", "2023-02-02")),
    target = "wk inc flu hosp",
    output_type = "quantile",
    output_type_id = c("0.25", "0.5", "0.75"),
    value = c(80, 100, 120)
  )

  # Oracle output only covers some dates
  oracle_output <- tibble::tibble(
    location = "US",
    target_end_date = as.Date("2023-01-01"),
    target = "wk inc flu hosp",
    output_type = "quantile",
    output_type_id = NA_character_,
    oracle_value = 95
  )

  result <- with_hubverse_oracle_output(hubverse_table, oracle_output)

  # All original rows should be preserved, including those that do not match
  expect_equal(nrow(result), 3)
  expect_equal(result$oracle_value, c(95, 95, NA))
})

test_that("with_hubverse_oracle_output handles empty oracle output", {
  hubverse_table <- tibble::tibble(
    location = "US",
    target_end_date = as.Date("2023-01-01"),
    target = "wk inc flu hosp",
    output_type = "quantile",
    output_type_id = "0.5",
    value = 100
  )

  oracle_output <- tibble::tibble(
    location = character(0),
    target_end_date = as.Date(character(0)),
    target = character(0),
    output_type = character(0),
    output_type_id = character(0),
    oracle_value = numeric(0)
  )

  result <- with_hubverse_oracle_output(hubverse_table, oracle_output)

  expect_equal(nrow(result), 1)
  expect_true(is.na(result$oracle_value))
})
