dat <- forecasttools::example_daily_forecast_flu

test_that(
  paste0(
    "daily_to_epiweekly works identically to a ",
    "on the example data (unit test based on the ",
    "example in the hubverse formatting vignette)."
  ),
  {
    expected <- dat |>
      dplyr::mutate(
        epiweek = lubridate::epiweek(date),
        epiyear = lubridate::epiyear(date)
      ) |>
      dplyr::group_by(
        epiweek,
        epiyear,
        .draw,
        location
      ) |>
      dplyr::filter(dplyr::n() == 7) |>
      dplyr::summarise(
        weekly_hosp = sum(hosp),
        .groups = "drop"
      )

    result <- daily_to_epiweekly(
      dat,
      value_col = "hosp",
      id_cols = c(".draw", "location"),
      weekly_value_name = "weekly_hosp"
    )

    expect_equal(result, expected)

    ## should be different with partial weeks
    ## included
    expected_with_partial <- dat |>
      dplyr::mutate(
        epiweek = lubridate::epiweek(date),
        epiyear = lubridate::epiyear(date)
      ) |>
      dplyr::group_by(
        epiweek,
        epiyear,
        .draw,
        location
      ) |>
      dplyr::summarise(
        weekly_hosp = sum(hosp),
        .groups = "drop"
      )

    expect_true(nrow(result) < nrow(expected_with_partial))

    result_with_partial <- daily_to_epiweekly(
      dat,
      value_col = "hosp",
      id_cols = c(".draw", "location"),
      weekly_value_name = "weekly_hosp",
      strict = FALSE
    )

    expect_equal(result_with_partial, expected_with_partial)
  }
)

test_that(paste0(
  "daily_to_epiweekly() errors by default if more than ",
  "seven entries for a given epiweekly trajectory"
), {
  dat_duplicate_row <- dat |>
    dplyr::filter(
      date == as.Date("2023-10-30"),
      location == "KS",
      .draw == 18
    )
  expect_equal(nrow(dat_duplicate_row), 1)

  ## duplicate only one row, otherwise data
  ## still valid. Error should still occur.
  dat_duplicated <- dplyr::bind_rows(dat, dat_duplicate_row)

  expect_error(
    daily_to_epiweekly(
      dat_duplicated,
      value_col = "hosp",
      id_cols = c(".draw", "location"),
      weekly_value_name = "weekly_hosp"
    ),
    regexp = "repeated values"
  )
})
