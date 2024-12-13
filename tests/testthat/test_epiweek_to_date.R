schema <- tidyr::crossing(
  epiweek = 1:52,
  epiyear = 1800:2200,
  day_of_week = 1:7
)

test_that(paste0(
  "epiweek_to_date() function's internal ",
  "validation passes for USA epiweeks for ",
  "every epiweek in 1:52 for every epiyear ",
  "in 1800:2200, for every day of the epiweek"
), {
  expect_no_warning(
    epiweek_to_date(
      schema$epiweek,
      schema$epiyear,
      epiweek_standard = "USA",
      day_of_week = schema$day_of_week,
      validate = TRUE
    )
  )
})

test_that(paste0(
  "epiweek_to_date() function's internal ",
  "validation passes for ISO (epi)weeks for ",
  "every epiweek in 1:52 for every epiyear ",
  "in 1800:2200, for every day of the epiweek"
), {
  expect_no_warning(
    epiweek_to_date(
      schema$epiweek,
      schema$epiyear,
      epiweek_standard = "ISO",
      day_of_week = schema$day_of_week,
      validate = TRUE
    )
  )
})


test_that(paste0(
  "epiweek_to_date() function's internal ",
  "validation fails if you try to get the ",
  "start of an epiweek that doesn't exist ",
  "but passes if you get a valid epiweek"
), {
  ## 2020 had an epiweek 53
  expect_no_error(
    epiweek_to_date(
      53,
      2020,
      epiweek_standard = "USA",
      validate = TRUE
    )
  )

  ## 2021 did not
  expect_error(
    epiweek_to_date(
      53,
      2021,
      epiweek_standard = "USA",
      validate = TRUE
    )
  )

  ## a single failure should raise an error
  expect_error(
    epiweek_to_date(
      53,
      c(2020, 2021),
      epiweek_standard = "USA",
      validate = TRUE
    )
  )

  ## but the validation should be vectorized
  ## and succeed accordingly
  expect_no_error(
    epiweek_to_date(
      c(53, 52),
      c(2020, 2021),
      epiweek_standard = "USA",
      validate = TRUE
    )
  )
})
