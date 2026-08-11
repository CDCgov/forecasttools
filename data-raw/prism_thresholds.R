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


prism_array_dims <- c("breaks", "disease", "location", "signal")

thresholds_to_array <- function(dat) {
  completeness <- dat |>
    dplyr::summarise(
      n = dplyr::n(),
      expected = dplyr::n_distinct(.data$breaks) *
        dplyr::n_distinct(.data$disease) *
        dplyr::n_distinct(.data$location),
      .by = "signal"
    )
  checkmate::assert_true(all(completeness$n == completeness$expected))

  labels <- dat |>
    dplyr::select(dplyr::all_of(prism_array_dims)) |>
    purrr::map(\(x) as.character(sort(unique(x))))

  cells <- dat |>
    dplyr::select(dplyr::all_of(prism_array_dims)) |>
    purrr::map2(labels, \(x, lab) match(as.character(x), lab)) |>
    purrr::reduce(cbind)

  thresholds <- array(NA_real_, dim = lengths(labels), dimnames = labels)
  thresholds[cells] <- dat$value

  return(thresholds)
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
  dplyr::arrange(.data$as_of) |>
  tidyr::nest(.by = "as_of", .key = "thresholds") |>
  dplyr::mutate(
    thresholds = purrr::map(.data$thresholds, thresholds_to_array)
  ) |>
  tibble::deframe()

purrr::iwalk(prism_thresholds, \(thresholds, as_of_name) {
  dat <- dplyr::filter(long_thresholds, .data$as_of == as.Date(as_of_name))
  dims <- dimnames(thresholds)

  testthat::expect_equal(sum(!is.na(thresholds)), nrow(dat))

  purrr::walk(1:1000, \(i) {
    tmp_sample <- dims |> purrr::map_chr(\(x) sample(x, 1))

    actual <- thresholds[,
      tmp_sample[["disease"]],
      tmp_sample[["location"]],
      tmp_sample[["signal"]]
    ] |>
      unname()

    expected <- dat |>
      dplyr::filter(
        .data$signal == tmp_sample[["signal"]],
        .data$disease == tmp_sample[["disease"]],
        .data$location == tmp_sample[["location"]]
      ) |>
      dplyr::pull("value")

    if (length(expected) == 0) {
      testthat::expect_true(all(is.na(actual)))
    } else {
      testthat::expect_identical(actual, expected)
    }
  })
})

usethis::use_data(prism_thresholds, overwrite = TRUE)
