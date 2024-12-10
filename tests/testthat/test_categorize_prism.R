testthat::test_that(paste0(
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
      result <- get_prism_cutpoints(diseases, locations)
      expected <- forecasttools::prism_thresholds[
        locations, diseases,
      ]
      testthat::expect_equal(result, expected)
    }
  )
})

testthat::test_that(paste0(
  "get_prism_cutpoints() gives the expected ",
  "values for particular examples"
), {
  test_cases <- list(
    list(
      diseases = c(
        "Influenza"
      ),
      locations = c("US", "MA"),
      expected = array(
        c(
          0, 0,
          0.00339654, 0.002146614,
          0.01923233, 0.02071169,
          0.03506812, 0.03927676,
          0.05090390, 0.05784183,
          1, 1
        ),
        dim = c(2, 6),
        dimnames = list(
          location = c("US", "MA"),
          breaks = c(
            "prop_lower_bound",
            "prop_low",
            "prop_moderate",
            "prop_high",
            "prop_very_high",
            "prop_upper_bound"
          )
        )
      ),
      list(
        diseases = "RSV",
        locations = "UT",
        expected = c(
          prop_lower_bound = 0,
          prop_low = 0.0004531255,
          prop_moderate = 0.0088554939,
          prop_high = 0.0172578623,
          prop_very_high = 0.0256602307,
          prop_upper_bound = 1
        )
      )
    )
  )

  purrr::map(test_cases, \(x) {
    result <- get_prism_cutpoints(x$diseases, x$locations)
    testthat::expect_equal(result, x$expected)
  })
})
