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

break_names <- c(
  "very_low",
  "low",
  "moderate",
  "high",
  "very_high",
  "upper_bound"
)

prism_signals <- unique(forecasttools::prism_thresholds$signal)

as_ofs_for_signal <- function(signal) {
  forecasttools::prism_thresholds |>
    dplyr::filter(.data$signal == !!signal) |>
    dplyr::pull("as_of") |>
    unique()
}

latest_as_of_for_signal <- function(signal) {
  max(as_ofs_for_signal(signal))
}

query_date_for <- function(signal, vintage) {
  vintages <- sort(as_ofs_for_signal(signal))
  later_vintages <- vintages[vintages > vintage]

  offset <- if (length(later_vintages) == 0) {
    45
  } else {
    floor(as.numeric(min(later_vintages) - vintage) / 2)
  }

  return(vintage + offset)
}

prism_rows <- forecasttools::prism_thresholds |>
  dplyr::mutate(
    query_date = purrr::map2_vec(
      .data$signal,
      .data$as_of,
      query_date_for
    )
  )

prism_params <- prism_rows |>
  dplyr::filter(
    .data$as_of == latest_as_of_for_signal(.data$signal),
    .by = "signal"
  ) |>
  dplyr::select("signal", "location", "disease")


test_that(
  paste0(
    "get_prism_cutpoints() resolves a date to its vintage and ",
    "then reads identically to a manual read from the table "
  ),
  {
    prism_rows |>
      dplyr::sample_n(100) |>
      purrr::pmap(
        \(as_of, signal, disease, location, values, query_date) {
          result <- get_prism_cutpoints(
            location,
            disease,
            query_date,
            signal = signal
          )
          expect_equal(result, list(values))
        }
      )
  }
)


test_that(
  paste0(
    "Categorization works as expected for all bins, ",
    "all signals, all locations, and all diseases"
  ),
  {
    categorize_and_compare <- function(signal, location, disease) {
      cutpoints <- get_prism_cutpoints(location, disease, signal = signal)
      values <- get_binnable_values(cutpoints[[1]])
      expected_categories <- categorize_vector(
        values,
        break_sets = cutpoints,
        label_sets = list(default_labels)
      )

      result <- categorize_prism(values, location, disease, signal = signal)
      checkmate::expect_factor(result, ordered = TRUE)
      expect_equal(result, expected_categories)
    }

    purrr::pmap(prism_params, categorize_and_compare)
  }
)

test_that("vectors can be categorized with custom bin names", {
  custom_bin_names <- c("Bin1", "Bin2", "Bin3", "Bin4", "Bin5")

  categorize_with_custom_bins <- function(signal, location, disease) {
    cutpoints <- get_prism_cutpoints(location, disease, signal = signal)
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
      prism_bin_names = custom_bin_names,
      signal = signal
    )
    checkmate::expect_factor(result, ordered = TRUE)
    expect_equal(result, expected_categories)
  }

  purrr::pmap(prism_params, categorize_with_custom_bins)
})


test_that("NSSP and NHSN thresholds are on their documented scales", {
  nssp <- get_prism_cutpoints("CA", "Influenza", signal = "NSSP")[[1]]
  nhsn <- get_prism_cutpoints("CA", "Influenza", signal = "NHSN")[[1]]

  expect_named(nssp, break_names)
  expect_named(nhsn, break_names)

  checkmate::expect_numeric(nssp, upper = 1)

  expect_equal(nssp[["upper_bound"]], 1)
  expect_equal(nhsn[["upper_bound"]], Inf)
})


test_that("flexible capitalization of signals, locations, and diseases works", {
  distinct_cuts <- tidyr::expand_grid(
    signal = c("NHSN", "nhsn", "Nhsn", "nHsN"),
    location = c("CA", "ca", "Ca", "cA"),
    disease = c(
      "Influenza",
      "influenza",
      "INFLUENZA",
      "iNFlUeNzA",
      "influEnza"
    )
  ) |>
    dplyr::mutate(
      cuts = get_prism_cutpoints(
        .data$location,
        .data$disease,
        signal = .data$signal
      )
    ) |>
    dplyr::pull("cuts") |>
    dplyr::n_distinct()

  expect_equal(distinct_cuts, 1)
})

test_that("error is thrown for invalid as_of", {
  purrr::walk(prism_signals, \(signal) {
    expect_error(
      get_prism_cutpoints(
        "WA",
        "Influenza",
        as_of = "1900-01-01",
        signal = signal
      ) |>
        suppressWarnings(),
      regexp = "No PRISM"
    )
  })
})

test_that("error is thrown for an unknown signal", {
  expect_error(
    get_prism_cutpoints("WA", "Influenza", signal = "NREVSS"),
    regexp = "signal"
  )
})


test_that("omitting signal is deprecated and falls back to NSSP", {
  lifecycle::expect_deprecated(
    get_prism_cutpoints("CA", "Influenza")
  )
  lifecycle::expect_deprecated(
    categorize_prism(0.05, "CA", "Influenza")
  )

  expect_equal(
    suppressWarnings(get_prism_cutpoints("CA", "Influenza")),
    get_prism_cutpoints("CA", "Influenza", signal = "NSSP")
  )
})
