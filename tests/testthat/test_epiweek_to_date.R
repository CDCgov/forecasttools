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

some_epiweeks <- tidyr::crossing(
  epiweek = 1:52,
  epiyear = 1800:2200
)

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
