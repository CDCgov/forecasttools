dates <- seq(as.Date("1900-01-01"), as.Date("2060-01-01"), by = "day")
n_dates <- length(dates)

test_that(paste0(
  "iso and mmwr epiweek rounding functions agree ",
  "with manual expectation"
), {
  expect_equal(
    ceiling_mmwr_epiweek(as.Date("2025-03-13")),
    as.Date("2025-03-15")
  )
  expect_equal(
    ceiling_mmwr_epiweek(as.Date("2025-03-15")),
    as.Date("2025-03-15")
  )
  expect_equal(
    ceiling_mmwr_epiweek(as.Date("2025-03-16")),
    as.Date("2025-03-22")
  )
  expect_equal(
    ceiling_isoweek(as.Date("2025-03-13")),
    as.Date("2025-03-16")
  )
  expect_equal(
    ceiling_isoweek(as.Date("2025-03-16")),
    as.Date("2025-03-16")
  )
  expect_equal(
    ceiling_isoweek(as.Date("2025-03-17")),
    as.Date("2025-03-23")
  )
})

test_that(
  paste0(
    "iso and mmwr epiweek rounding functions always ",
    "output the expected day of the week"
  ),
  {
    expect_equal(
      lubridate::wday(ceiling_mmwr_epiweek(dates), week_start = 1),
      rep(6, n_dates)
    )
    expect_equal(
      lubridate::wday(ceiling_isoweek(dates), week_start = 1),
      rep(7, n_dates)
    )
    expect_equal(
      lubridate::wday(floor_mmwr_epiweek(dates), week_start = 1),
      rep(7, n_dates)
    )
    expect_equal(
      lubridate::wday(floor_isoweek(dates), week_start = 1),
      rep(1, n_dates)
    )
  }
)

test_that(
  paste0(
    "iso and mmwr epiweek rounding functions always output ",
    "a day within the same epiweek and epiyear"
  ),
  {
    expect_equal(
      lubridate::epiweek(ceiling_mmwr_epiweek(dates)),
      lubridate::epiweek(dates)
    )
    expect_equal(
      lubridate::epiyear(ceiling_mmwr_epiweek(dates)),
      lubridate::epiyear(dates)
    )
    expect_equal(
      lubridate::isoweek(ceiling_isoweek(dates)),
      lubridate::isoweek(dates)
    )
    expect_equal(
      lubridate::isoyear(ceiling_isoweek(dates)),
      lubridate::isoyear(dates)
    )
    expect_equal(
      lubridate::epiweek(floor_mmwr_epiweek(dates)),
      lubridate::epiweek(dates)
    )
    expect_equal(
      lubridate::epiyear(floor_mmwr_epiweek(dates)),
      lubridate::epiyear(dates)
    )
    expect_equal(
      lubridate::isoweek(floor_isoweek(dates)),
      lubridate::isoweek(dates)
    )
    expect_equal(
      lubridate::isoyear(floor_isoweek(dates)),
      lubridate::isoyear(dates)
    )
  }
)
