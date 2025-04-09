some_epiweeks <- tidyr::crossing(
  epiweek = 1:52,
  epiyear = 1800:2200
)

ew_dow <- tidyr::crossing(
  some_epiweeks,
  day_of_week = 1:7
)

test_that(paste0(
  "epiweek_to_date() satisfies assert_date_in_epiweek() ",
  "every USA/MMWR epiweek in 1:52 for every epiyear ",
  "in 1800:2200, for every day of the epiweek"
), {
  expect_no_warning(
    assert_date_in_epiweek(
      epiweek_to_date(
        ew_dow$epiweek,
        ew_dow$epiyear,
        epiweek_standard = "USA",
        day_of_week = ew_dow$day_of_week
      ),
      ew_dow$epiweek,
      ew_dow$epiyear,
      epiweek_standard = "MMWR"
    )
  )
})

test_that(paste0(
  "epiweek_to_date() satisfies assert_date_in_epiweek() ",
  "every ISO epiweek in 1:52 for every epiyear ",
  "in 1800:2200, for every day of the epiweek"
), {
  expect_no_warning(
    assert_date_in_epiweek(
      epiweek_to_date(
        ew_dow$epiweek,
        ew_dow$epiyear,
        epiweek_standard = "ISO",
        day_of_week = ew_dow$day_of_week
      ),
      ew_dow$epiweek,
      ew_dow$epiyear,
      epiweek_standard = "iso"
    )
  )
})

test_that(paste0(
  "epiweek_to_date() validates the `epiweek_standard`"
), {
  # must be a scalar
  expect_error(
    epiweek_to_date(
      ew_dow$epiweek,
      ew_dow$epiyear,
      epiweek_standard = c("ISO", "ISO"),
      day_of_week = ew_dow$day_of_week
    ),
    "Must have length 1."
  )
  ## must be a known standard
  expect_error(
    epiweek_to_date(
      ew_dow$epiweek,
      ew_dow$epiyear,
      epiweek_standard = c("ISOA"),
      day_of_week = ew_dow$day_of_week
    ),
    "must be a subset of"
  )
})


test_that(paste0(
  "epiweek_to_date() uses vector recycling but errors ",
  "given incompatible shapes"
), {
  expect_no_warning(
    epiweek_to_date(
      1,
      c(2020, 2027),
      day_of_week = 1:2
    )
  )
  expect_no_warning(
    epiweek_to_date(
      c(1, 5),
      2025,
      day_of_week = 1:2
    )
  )
  expect_no_warning(
    epiweek_to_date(
      1,
      2025,
      day_of_week = c(1, 7)
    )
  )

  expect_error(
    epiweek_to_date(
      1,
      2025:2028,
      day_of_week = c(1, 7)
    ),
    "Can't recycle"
  )

  expect_error(
    epiweek_to_date(
      rep(1, 5),
      rep(2025, 5),
      day_of_week = c(1, 7)
    ),
    "Can't recycle"
  )
})


test_that(paste0(
  "epiyear_n_days and epiyear_n_days results correspond ",
  "to manual expectation"
), {
  years <- 2020:2032
  expected_weeks_mmwr <- c(
    53, 52, 52, 52, 52, 53, 52,
    52, 52, 52, 52, 53, 52
  )
  expected_weeks_iso <- c(
    53, 52, 52, 52, 52, 52, 53, 52,
    52, 52, 52, 52, 53
  )
  expect_equal(
    epiyear_n_days(years),
    7L * expected_weeks_mmwr
  )
  expect_equal(
    epiyear_n_weeks(years),
    expected_weeks_mmwr
  )
  expect_equal(
    epiyear_n_weeks(years, epiweek_standard = "mMwR"),
    expected_weeks_mmwr
  )
  expect_equal(
    epiyear_n_weeks(years, epiweek_standard = "USa"),
    expected_weeks_mmwr
  )
  expect_equal(
    epiyear_n_weeks(years, epiweek_standard = "Iso"),
    expected_weeks_iso
  )
  expect_equal(
    epiyear_n_days(years, epiweek_standard = "isO"),
    7L * expected_weeks_iso
  )
})

test_that(paste0(
  "epiweek_to_date() errors if you try to get ",
  "a date from an epiweek that doesn't exist ",
  "but not if you request a valid epiweek"
), {
  ## 2020 had an epiweek 53
  expect_no_error(
    epiweek_to_date(
      53,
      2020,
      epiweek_standard = "USA"
    )
  )

  ## 2021 did not
  expect_error(
    epiweek_to_date(
      53,
      2021,
      epiweek_standard = "USA"
    ),
    "epiweek 53 of epiyear 2021"
  )

  # non-integer epiweeks are not valid
  expect_error(
    epiweek_to_date(
      50.5,
      2021,
      epiweek_standard = "USA"
    ),
    "integer"
  )

  # non-integer epiyears are not valid
  expect_error(
    epiweek_to_date(
      50,
      2026.5,
      epiweek_standard = "USA"
    ),
    "integer"
  )

  # epiweeks less than 1 are not valid
  expect_error(
    epiweek_to_date(
      0,
      2021,
      epiweek_standard = "USA"
    ),
    ">= 1"
  )


  ## a single failure should raise an error
  expect_error(
    epiweek_to_date(
      53,
      c(2020, 2021),
      epiweek_standard = "USA"
    ),
    "epiweek 53 of epiyear 2021"
  )

  ## but the validation should be vectorized
  ## and succeed accordingly
  expect_no_error(
    epiweek_to_date(
      c(53, 52),
      c(2020, 2021),
      epiweek_standard = "USA"
    )
  )
})


test_that(
  paste0(
    "with_epidate() behaves identically to manual use of ",
    "epiweek_to_date() in dplyr::mutate()"
  ),
  {
    result <- some_epiweeks |> with_epidate()
    expected <- some_epiweeks |>
      dplyr::mutate(
        epidate = epiweek_to_date(
          .data$epiweek,
          .data$epiyear,
          day_of_week = 1,
          epiweek_standard = "USA"
        )
      )
    expect_equal(result, expected)

    result <- some_epiweeks |> with_epidate(day_of_week = 5)
    expected <- some_epiweeks |>
      dplyr::mutate(
        epidate = epiweek_to_date(
          .data$epiweek,
          .data$epiyear,
          day_of_week = 5,
          epiweek_standard = "USA"
        )
      )
    expect_equal(result, expected)

    result <- some_epiweeks |> with_epidate(
      day_of_week = 7,
      epidate_name = "epiweek_end_date"
    )
    expected <- some_epiweeks |>
      dplyr::mutate(
        epiweek_end_date = epiweek_to_date(
          .data$epiweek,
          .data$epiyear,
          day_of_week = 7,
          epiweek_standard = "USA"
        )
      )
    expect_equal(result, expected)
  }
)
