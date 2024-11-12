schema <- tidyr::crossing(
  epiweek = 1:52,
  epiyear = 1800:2200,
  day_of_week = 1:7
)

testthat::test_that(paste0(
  "epiweek_to_date() function's internal ",
  "validation passes for USA epiweeks for ",
  "every epiweek in 1:52 for every epiyear ",
  "in 1800:2200, for every day of the epiweek"
), {
  testthat::expect_no_error(
    with_usa_dates <- schema |>
      dplyr::mutate(
        date = forecasttools::epiweek_to_date(
          epiweek,
          epiyear,
          epiweek_standard = "USA",
          day_of_week = day_of_week,
          validate = TRUE
        )
      )
  )
})

testthat::test_that(paste0(
  "epiweek_to_date() function's internal ",
  "validation passes for ISO (epi)weeks for ",
  "every epiweek in 1:52 for every epiyear ",
  "in 1800:2200, for every day of the epiweek"
), {
  testthat::expect_no_error(
    with_iso_dates <- schema |>
      dplyr::mutate(
        date = forecasttools::epiweek_to_date(
          epiweek,
          epiyear,
          epiweek_standard = "ISO",
          day_of_week = day_of_week,
          validate = TRUE
        )
      )
  )
})


testthat::test_that(paste0(
  "epiweek_to_date() function's internal ",
  "validation fails if you try to get the ",
  "start of an epiweek that doesn't exist ",
  "but passes if you get a valid epiweek"
), {
  ## 2020 had an epiweek 53
  testthat::expect_no_error(
    forecasttools::epiweek_to_date(
      53,
      2020,
      epiweek_standard = "USA",
      validate = TRUE
    )
  )

  ## 2021 did not
  testthat::expect_error(
    forecasttools::epiweek_to_date(
      53,
      2021,
      epiweek_standard = "USA",
      validate = TRUE
    )
  )

  ## a single failure should raise an error
  testthat::expect_error(
    forecasttools::epiweek_to_date(
      53,
      c(2020, 2021),
      epiweek_standard = "USA",
      validate = TRUE
    )
  )

  ## but the validation should be vectorized
  ## and succeed accordingly
  testthat::expect_no_error(
    forecasttools::epiweek_to_date(
      c(53, 52),
      c(2020, 2021),
      epiweek_standard = "USA",
      validate = TRUE
    )
  )
})
