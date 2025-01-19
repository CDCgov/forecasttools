dat <- forecasttools::example_daily_forecast_flu

mini_dat <- dat |>
  dplyr::filter(
    .data$date <= lubridate::ymd("2023-10-28"),
    .data$location %in% c("NM", "SD")
  )

test_that(
  paste0(
    "daily_to_epiweekly works identically to a manual approach",
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

test_that(paste0(
  "daily_to_epiweekly() optionally annotates ",
  "results with epiweek start and/or end dates"
), {
  result <- daily_to_epiweekly(
    mini_dat,
    value_col = "hosp",
    id_cols = c(".draw", "location"),
    weekly_value_name = "weekly_hosp",
    with_epiweek_start_date = FALSE,
    with_epiweek_end_date = FALSE
  )
  expect_gt(nrow(result), 1)
  checkmate::expect_names(names(result),
    disjunct.from = c("epiweek_start_date", "epiweek_end_date")
  )

  result <- daily_to_epiweekly(
    mini_dat,
    value_col = "hosp",
    id_cols = c(".draw", "location"),
    weekly_value_name = "weekly_hosp",
    with_epiweek_start_date = TRUE,
    with_epiweek_end_date = TRUE
  )
  expect_gt(nrow(result), 1)

  checkmate::expect_names(names(result),
    must.include = c("epiweek_start_date", "epiweek_end_date")
  )
  expect_equal(
    lubridate::wday(result$epiweek_start_date, week_start = 7),
    rep(1, length(result$epiweek_start_date))
  )
  expect_equal(
    lubridate::wday(result$epiweek_end_date, week_start = 7),
    rep(7, length(result$epiweek_start_date))
  )

  result <- daily_to_epiweekly(
    mini_dat,
    value_col = "hosp",
    id_cols = c(".draw", "location"),
    weekly_value_name = "weekly_hosp",
    with_epiweek_start_date = FALSE,
    with_epiweek_end_date = TRUE
  )
  expect_gt(nrow(result), 1)
  checkmate::expect_names(names(result),
    must.include = "epiweek_end_date",
    disjunct.from = "epiweek_start_date"
  )
  result <- daily_to_epiweekly(
    mini_dat,
    value_col = "hosp",
    id_cols = c(".draw", "location"),
    weekly_value_name = "weekly_hosp",
    with_epiweek_start_date = TRUE,
    with_epiweek_end_date = FALSE,
  )
  expect_gt(nrow(result), 1)
  checkmate::expect_names(names(result),
    must.include = "epiweek_start_date",
    disjunct.from = "epiweek_end_date"
  )

  ## names should be configurable
  result <- daily_to_epiweekly(
    mini_dat,
    value_col = "hosp",
    id_cols = c(".draw", "location"),
    weekly_value_name = "weekly_hosp",
    with_epiweek_start_date = TRUE,
    with_epiweek_end_date = TRUE,
    epiweek_end_date_name = "enddate",
    epiweek_start_date_name = "startdate"
  )
  expect_gt(nrow(result), 1)
  checkmate::expect_names(names(result),
    must.include = c("startdate", "enddate"),
    disjunct.from = c("epiweek_start_date", "epiweek_end_date")
  )
})
