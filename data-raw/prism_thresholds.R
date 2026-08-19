prism_signal_specs <- list(
  "nssp" = list(upper_bound = 1),
  "nhsn" = list(upper_bound = Inf)
)


normalize_thresholds <- function(dat, signal) {
  if (signal == "nssp") {
    dat <- dat |>
      dplyr::mutate(dplyr::across(
        dplyr::starts_with("perc_level_"),
        \(x) x / 100
      )) |>
      dplyr::rename_with(
        \(x) stringr::str_remove(x, "^perc_"),
        dplyr::starts_with("perc_level_")
      )
  }

  if (signal == "nhsn") {
    dat <- dat |>
      dplyr::filter(.data$unit == "rate") |>
      dplyr::select(-"unit", -"total_population", -"hhs_region")
  }

  dat |>
    dplyr::rename("location" = "state_abb") |>
    dplyr::mutate("level_very_low" = 0, .before = "level_low") |>
    dplyr::mutate(
      "level_upper_bound" = prism_signal_specs[[signal]]$upper_bound
    )
}


prism_files <-
  tibble::tibble(
    "file_path" = fs::path("inst", "extdata", "prism_thresholds") |>
      fs::dir_ls(recurse = TRUE, glob = "*.tsv")
  ) |>
  dplyr::mutate(
    signal = .data$file_path |>
      fs::path_dir() |>
      fs::path_file(),
    as_of = .data$file_path |>
      fs::path_file() |>
      fs::path_ext_remove() |>
      lubridate::ymd(),
    dat = purrr::map(.data$file_path, \(x) {
      readr::read_tsv(x, show_col_types = FALSE) |>
        dplyr::arrange(.data$disease, .data$state_abb)
    })
  ) |>
  dplyr::select(-"file_path")

prism_files$as_of |>
  checkmate::assert_date(any.missing = FALSE, .var.name = "as_of")


prism_files$signal |>
  unique() |>
  testthat::expect_setequal(names(prism_signal_specs))


long_thresholds <-
  prism_files |>
  dplyr::mutate(
    dat = purrr::map2(.data$dat, .data$signal, normalize_thresholds)
  ) |>
  tidyr::unnest("dat") |>
  dplyr::mutate(dplyr::across(
    dplyr::where(is.character),
    stringr::str_to_lower
  )) |>
  tidyr::pivot_longer(
    cols = dplyr::starts_with("level_"),
    names_to = "breaks",
    values_to = "value"
  ) |>
  dplyr::mutate(
    breaks = .data$breaks |>
      stringr::str_remove("^level_") |>
      forcats::fct_inorder()
  )

prism_thresholds <-
  long_thresholds |>
  dplyr::arrange(
    .data$as_of,
    .data$signal,
    .data$disease,
    .data$location,
    .data$breaks
  ) |>
  dplyr::summarise(
    values = list(purrr::set_names(.data$value, .data$breaks)),
    .by = c("as_of", "signal", "disease", "location")
  )

prism_thresholds |>
  dplyr::count(
    .data$as_of,
    .data$signal,
    .data$disease,
    .data$location
  ) |>
  dplyr::pull("n") |>
  checkmate::assert_set_equal(1)

prism_thresholds$values |>
  purrr::walk(\(x) {
    checkmate::assert_numeric(x, any.missing = FALSE, names = "unique")
  })

expected_bin_names <- c(
  "very_low",
  "low",
  "moderate",
  "high",
  "very_high",
  "upper_bound"
)
prism_thresholds$values |>
  purrr::walk(\(x) {
    checkmate::assert_names(names(x), identical.to = expected_bin_names)
  })

purrr::pwalk(prism_thresholds, \(as_of, signal, disease, location, values) {
  expected <- long_thresholds |>
    dplyr::filter(
      .data$as_of == !!as_of,
      .data$signal == !!signal,
      .data$disease == !!disease,
      .data$location == !!location
    ) |>
    dplyr::pull("value")

  testthat::expect_identical(unname(values), expected)
})


nhsn_population_rows <-
  prism_files |>
  dplyr::filter(.data$signal == "nhsn") |>
  tidyr::unnest("dat") |>
  dplyr::filter(.data$unit == "rate") |>
  dplyr::select(
    location = "state_abb",
    "as_of",
    population = "total_population"
  ) |>
  dplyr::mutate(location = stringr::str_to_lower(.data$location))

nhsn_population_rows |>
  dplyr::summarise(
    n_populations = dplyr::n_distinct(.data$population),
    .by = c("as_of", "location")
  ) |>
  dplyr::pull("n_populations") |>
  checkmate::assert_set_equal(1)

prism_rate_reference_populations <-
  nhsn_population_rows |>
  dplyr::summarise(
    population = unique(.data$population),
    .by = c("as_of", "location")
  ) |>
  dplyr::select("location", "as_of", "population") |>
  dplyr::arrange(.data$as_of, .data$location)

prism_rate_reference_populations |>
  dplyr::count(.data$location, .data$as_of) |>
  dplyr::pull("n") |>
  checkmate::assert_set_equal(1)

prism_rate_reference_populations$population |>
  checkmate::assert_integerish(lower = 1, any.missing = FALSE)

purrr::walk(unique(prism_rate_reference_populations$as_of), \(vintage) {
  population_locations <- prism_rate_reference_populations |>
    dplyr::filter(.data$as_of == !!vintage) |>
    dplyr::pull("location")

  threshold_locations <- prism_thresholds |>
    dplyr::filter(
      .data$as_of == !!vintage,
      .data$signal == "nhsn"
    ) |>
    dplyr::pull("location") |>
    unique()

  testthat::expect_setequal(population_locations, threshold_locations)
})

usethis::use_data(
  prism_thresholds,
  prism_rate_reference_populations,
  overwrite = TRUE
)
