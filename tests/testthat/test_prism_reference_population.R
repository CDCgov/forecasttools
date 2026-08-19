test_that("get_prism_reference_population matches the table it reads", {
  latest_vintage <- max(prism_rate_reference_populations$as_of)

  expected <- prism_rate_reference_populations |>
    dplyr::filter(.data$as_of == !!latest_vintage) |>
    dplyr::arrange(.data$location)

  actual <- get_prism_reference_population(
    expected$location,
    as_of = latest_vintage
  )

  expect_identical(actual, as.double(expected$population))
})


test_that("get_prism_reference_population is vectorized over location", {
  locations <- c("wa", "ca", "wa", "al")
  actual <- get_prism_reference_population(locations)

  expect_length(actual, length(locations))
  expect_type(actual, "double")

  expect_identical(
    actual,
    purrr::map_dbl(locations, \(x) get_prism_reference_population(x))
  )
  expect_identical(actual[[1]], actual[[3]])
})


test_that("get_prism_reference_population accepts any case", {
  expect_identical(
    get_prism_reference_population("WA"),
    get_prism_reference_population("wa")
  )
})


test_that("get_prism_reference_population uses the vintage in effect", {
  vintages <- sort(unique(prism_rate_reference_populations$as_of))
  first_vintage <- vintages[[1]]

  expect_identical(
    get_prism_reference_population("wa", as_of = first_vintage),
    get_prism_reference_population("wa", as_of = first_vintage + 365)
  )

  expect_error(
    get_prism_reference_population("wa", as_of = first_vintage - 1),
    "No PRISM reference population"
  )
})


test_that("get_prism_reference_population errors without coverage", {
  uncovered <- c("us", "mo", "wy", "pr")

  expect_length(
    intersect(uncovered, prism_rate_reference_populations$location),
    0
  )

  purrr::walk(uncovered, \(location) {
    expect_error(
      get_prism_reference_population(location),
      "No PRISM reference population"
    )
  })
})


test_that("get_prism_reference_population rejects a vectorized as_of", {
  expect_error(
    get_prism_reference_population(
      "wa",
      as_of = as.Date(c("2026-08-13", "2026-08-14"))
    ),
    "as_of"
  )
})


test_that("reference populations cover the rate-scale threshold locations", {
  purrr::walk(unique(prism_rate_reference_populations$as_of), \(vintage) {
    population_locations <- prism_rate_reference_populations |>
      dplyr::filter(.data$as_of == !!vintage) |>
      dplyr::pull("location")

    threshold_locations <- forecasttools::prism_thresholds |>
      dplyr::filter(
        .data$as_of == !!vintage,
        .data$signal == "nhsn"
      ) |>
      dplyr::pull("location") |>
      unique()

    expect_setequal(population_locations, threshold_locations)
  })
})
