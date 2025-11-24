get_binnable_values <- function(set_of_cutpoints) {
  return(c(
    set_of_cutpoints[1] - 1.5,
    mean(c(set_of_cutpoints[1], set_of_cutpoints[2])),
    mean(c(set_of_cutpoints[2], set_of_cutpoints[3])),
    mean(c(set_of_cutpoints[3], set_of_cutpoints[4])),
    set_of_cutpoints[4] + 1.5
  ))
}

default_labels <- default_prism_bin_names


test_that(
  paste0(
    "get_prism_cutpoints() works identically ",
    "to a manual read from the table "
  ),
  {
    vars <- dimnames(forecasttools::prism_thresholds)
    vars$breaks <- NULL

    tidyr::expand_grid(!!!vars) |>
      dplyr::sample_n(100) |>
      purrr::pmap(
        \(disease, location, as_of) {
          result <- get_prism_cutpoints(location, disease, as_of)
          expected <- list(forecasttools::prism_thresholds[,
            disease,
            location,
            as_of
          ])
          expect_equal(result, expected)
        }
      )
  }
)


test_that(
  paste0(
    "Categorization works as expected for all bins, ",
    "all locations, and all diseases"
  ),
  {
    locations <- dimnames(forecasttools::prism_thresholds)$location
    diseases <- dimnames(forecasttools::prism_thresholds)$disease

    params <- tidyr::expand_grid(location = locations, disease = diseases)

    categorize_and_compare <- function(location, disease) {
      cutpoints <- get_prism_cutpoints(location, disease)
      values <- get_binnable_values(cutpoints[[1]])
      expected_categories <- categorize_vector(
        values,
        break_sets = cutpoints,
        label_sets = list(default_labels)
      )

      result <- categorize_prism(values, location, disease)
      checkmate::expect_factor(result, ordered = TRUE)
      expect_equal(result, expected_categories)
    }

    purrr::pmap(params, categorize_and_compare)
  }
)

test_that("vectors can be categorized with custom bin names", {
  locations <- dimnames(forecasttools::prism_thresholds)$location
  diseases <- dimnames(forecasttools::prism_thresholds)$disease
  custom_bin_names <- c("Bin1", "Bin2", "Bin3", "Bin4", "Bin5")

  params <- tidyr::crossing(location = locations, disease = diseases)

  categorize_with_custom_bins <- function(location, disease) {
    cutpoints <- get_prism_cutpoints(location, disease)
    values <- get_binnable_values(cutpoints[[1]])
    expected_categories <- categorize_vector(
      values,
      break_sets = cutpoints,
      label_sets = list(custom_bin_names)
    )

    result <- categorize_prism(
      values,
      location,
      disease,
      prism_bin_names = custom_bin_names
    )
    checkmate::expect_factor(result, ordered = TRUE)
    expect_equal(result, expected_categories)
  }

  purrr::pmap(params, categorize_with_custom_bins)
})


test_that("flexible capitalization of locations and diseases works", {
  distinct_cuts <- tidyr::expand_grid(
    location = c("CA", "ca", "Ca", "cA"),
    disease = c(
      "Influenza",
      "influenza",
      "INFLUENZA",
      "iNFlUeNzA",
      "influEnza"
    )
  ) |>
    dplyr::mutate(cuts = get_prism_cutpoints(location, disease)) |>
    dplyr::pull(cuts) |>
    dplyr::n_distinct()

  expect_equal(distinct_cuts, 1)
})

test_that("error is thrown for invalid as_of", {
  expect_error(
    get_prism_cutpoints("WA", "Influenza", as_of = "1900-01-01"),
    regexp = "No available PRISM cutpoints"
  )
})
