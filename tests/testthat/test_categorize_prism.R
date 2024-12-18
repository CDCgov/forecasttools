test_that(paste0(
  "get_prism_cutpoints() works identically ",
  "to a manual read from the table "
), {
  locations <- names(forecasttools::prism_thresholds[, 1, 1])
  diseases <- names(forecasttools::prism_thresholds[1, , 1])
  locs_diseases <- tidyr::expand_grid(
    locations,
    diseases
  )

  purrr::pmap(
    locs_diseases,
    \(locations, diseases) {
      result <- get_prism_cutpoints(locations, diseases)
      expected <- list(forecasttools::prism_thresholds[
        locations, diseases,
      ])
      expect_equal(result, expected)
    }
  )
})

test_that(paste0(
  "get_prism_cutpoints() gives the expected ",
  "values for particular examples"
), {
  test_cases <- list(
    list(
      diseases = c(
        "Influenza"
      ),
      locations = c("US", "MA"),
      expected = list(
        c(
          prop_lower_bound = 0,
          prop_low = 0.00339654,
          prop_moderate = 0.019232328,
          prop_high = 0.035068116,
          prop_very_high = 0.050903904,
          prop_upper_bound = 1
        ),
        c(
          prop_lower_bound = 0,
          prop_low = 0.002146614,
          prop_moderate = 0.020711687,
          prop_high = 0.03927676,
          prop_very_high = 0.057841833,
          prop_upper_bound = 1
        )
      ),
      list(
        diseases = "RSV",
        locations = "UT",
        expected = list(c(
          prop_lower_bound = 0,
          prop_low = 0.0004531255,
          prop_moderate = 0.0088554939,
          prop_high = 0.0172578623,
          prop_very_high = 0.0256602307,
          prop_upper_bound = 1
        ))
      )
    )
  )

  purrr::map(test_cases, \(x) {
    result <- get_prism_cutpoints(x$locations, x$diseases)
    expect_equal(result, x$expected)
  })
})
