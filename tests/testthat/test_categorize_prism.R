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

prism_as_ofs <- names(forecasttools::prism_thresholds)

prism_signals <- forecasttools::prism_thresholds |>
  purrr::map(\(x) dimnames(x)$signal) |>
  unlist(use.names = FALSE) |>
  unique()

as_ofs_for_signal <- function(signal) {
  forecasttools::prism_thresholds |>
    purrr::keep(\(x) signal %in% dimnames(x)$signal) |>
    names()
}

latest_as_of_for_signal <- function(signal) {
  max(lubridate::as_date(as_ofs_for_signal(signal))) |> as.character()
}

query_date_for <- function(vintage) {
  vintages <- prism_as_ofs |> lubridate::as_date() |> sort()
  vintage <- lubridate::as_date(vintage)
  later_vintages <- vintages[vintages > vintage]

  offset <- if (length(later_vintages) == 0) {
    45
  } else {
    floor(as.numeric(min(later_vintages) - vintage) / 2)
  }

  return(vintage + offset)
}

covered_combinations <- function(vintage) {
  thresholds <- forecasttools::prism_thresholds[[vintage]]
  dims <- dimnames(thresholds)
  vintage_query_date <- query_date_for(vintage)

  tidyr::expand_grid(
    as_of = vintage,
    signal = dims$signal,
    location = dims$location,
    disease = dims$disease
  ) |>
    dplyr::filter(purrr::pmap_lgl(
      list(.data$disease, .data$location, .data$signal),
      \(disease, location, signal) {
        !anyNA(thresholds[, disease, location, signal])
      }
    )) |>
    dplyr::mutate(query_date = vintage_query_date)
}

prism_params <- prism_signals |>
  purrr::map(\(signal) {
    covered_combinations(latest_as_of_for_signal(signal)) |>
      dplyr::filter(.data$signal == !!signal) |>
      dplyr::select("signal", "location", "disease")
  }) |>
  purrr::list_rbind()


test_that(
  paste0(
    "get_prism_cutpoints() resolves a date to its vintage and ",
    "then reads identically to a manual read from the table "
  ),
  {
    prism_as_ofs |>
      purrr::map(covered_combinations) |>
      purrr::list_rbind() |>
      dplyr::sample_n(100) |>
      purrr::pmap(
        \(as_of, signal, location, disease, query_date) {
          result <- get_prism_cutpoints(
            location,
            disease,
            query_date,
            signal = signal
          )
          expected <- forecasttools::prism_thresholds[[as_of]][,
            disease,
            location,
            signal
          ]
          expect_equal(result, list(expected))
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

  expect_equal(unname(nssp[["upper_bound"]]), 1)
  expect_equal(unname(nhsn[["upper_bound"]]), Inf)
  expect_true(all(nssp[1:5] <= 1))
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
      cuts = purrr::pmap(
        list(.data$location, .data$disease, .data$signal),
        \(location, disease, signal) {
          get_prism_cutpoints(location, disease, signal = signal)
        }
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
      regexp = "No available PRISM cutpoints"
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
