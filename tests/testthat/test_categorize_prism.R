get_binnable_values <- function(set_of_cutpoints) {
  return(c(
    set_of_cutpoints[1] - 1.5,
    mean(c(set_of_cutpoints[1], set_of_cutpoints[2])),
    mean(c(set_of_cutpoints[2], set_of_cutpoints[3])),
    mean(c(set_of_cutpoints[3], set_of_cutpoints[4])),
    set_of_cutpoints[4] + 1.5
  ))
}

default_labels <- c("Very Low", "Low", "Moderate", "High", "Very High")


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


test_that(paste0(
  "Categorization works as expected for all bins, ",
  "all locations, and all diseases"
), {
  locations <- dimnames(forecasttools::prism_thresholds)$location
  diseases <- dimnames(forecasttools::prism_thresholds)$disease

  params <- tidyr::crossing(location = locations, disease = diseases)

  categorize_and_compare <- function(location, disease) {
    cutpoints <- get_prism_cutpoints(location, disease)
    values <- get_binnable_values(cutpoints[[1]])
    expected_categories <- categorize_vector(values,
      break_sets = cutpoints,
      label_sets = list(default_labels)
    )

    result <- categorize_prism(values, location, disease)
    checkmate::expect_factor(result, ordered = TRUE)
    expect_equal(result, expected_categories)
  }

  purrr::pmap(params, categorize_and_compare)
})

test_that("vectors can be categorized with custom bin names", {
  locations <- dimnames(forecasttools::prism_thresholds)$location
  diseases <- dimnames(forecasttools::prism_thresholds)$disease
  custom_bin_names <- c("Bin1", "Bin2", "Bin3", "Bin4", "Bin5")

  params <- tidyr::crossing(location = locations, disease = diseases)

  categorize_with_custom_bins <- function(location, disease) {
    cutpoints <- get_prism_cutpoints(location, disease)
    values <- get_binnable_values(cutpoints[[1]])
    expected_categories <- categorize_vector(values,
      break_sets = cutpoints,
      label_sets = list(custom_bin_names)
    )

    result <- categorize_prism(values,
      location,
      disease,
      prism_bin_names = custom_bin_names
    )
    checkmate::expect_factor(result, ordered = TRUE)
    expect_equal(result, expected_categories)
  }

  purrr::pmap(params, categorize_with_custom_bins)
})
