prism_signal_specs <- list(
  "nssp" = list(
    dim_cols = c("breaks", "disease", "location", "as_of"),
    break_prefix = "prop_"
  ),
  "nhsn" = list(
    dim_cols = c("breaks", "disease", "location", "as_of", "unit"),
    break_prefix = ""
  )
)


prism_upper_bound <- function(unit) {
  dplyr::if_else(unit == "prop", 1, Inf)
}


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
      ) |>
      dplyr::mutate("unit" = "prop")
  }

  dat |>
    dplyr::rename("location" = "state_abb") |>
    dplyr::mutate("level_very_low" = 0, .before = "level_low") |>
    dplyr::mutate("level_upper_bound" = prism_upper_bound(.data$unit))
}


prism_signal_long <- function(dat, signal_name) {
  dat |>
    dplyr::filter(.data$signal == signal_name) |>
    dplyr::mutate(
      breaks = forcats::fct_relabel(
        .data$breaks,
        \(x) paste0(prism_signal_specs[[signal_name]]$break_prefix, x)
      )
    )
}


array_slice <- function(thresholds, slice_dims) {
  return(unname(
    do.call("[", c(list(thresholds), list(TRUE), as.list(slice_dims)))
  ))
}


long_slice <- function(dat, slice_dims) {
  purrr::reduce(
    names(slice_dims),
    \(acc, dim_name) {
      acc[as.character(acc[[dim_name]]) == slice_dims[[dim_name]], ]
    },
    .init = dat
  ) |>
    dplyr::arrange(.data$breaks) |>
    dplyr::pull("value")
}


thresholds_to_array <- function(dat, dim_cols) {
  sorted <- dat |>
    dplyr::select(dplyr::all_of(rev(dim_cols)), "value") |>
    dplyr::arrange(dplyr::across(dplyr::all_of(rev(dim_cols))))

  dims <- sorted |>
    dplyr::select(-"value") |>
    purrr::map(unique) |>
    purrr::map(as.character) |>
    rev()

  return(array(
    data = sorted$value,
    dim = lengths(dims),
    dimnames = dims
  ))
}

prism_files <-
  tibble::tibble(
    "file_path" = fs::path("inst", "extdata") |>
      fs::dir_ls()
  ) |>
  dplyr::filter(
    file_path |>
      fs::path_file() |>
      stringr::str_detect(
        "^prism_thresholds_[a-z]+_\\d{4}-\\d{2}-\\d{2}\\.tsv$"
      )
  ) |>
  dplyr::mutate(
    signal = file_path |>
      fs::path_file() |>
      stringr::str_extract("(?<=^prism_thresholds_)[a-z]+"),
    as_of = file_path |>
      fs::path_file() |>
      stringr::str_extract("\\d{4}-\\d{2}-\\d{2}") |>
      as.Date(),
    dat = purrr::map(file_path, \(x) {
      readr::read_tsv(x, show_col_types = FALSE) |>
        dplyr::arrange(.data$disease, .data$state_abb)
    })
  ) |>
  dplyr::select(-"file_path")


prism_files$signal |>
  unique() |>
  testthat::expect_setequal(names(prism_signal_specs))


prism_files |>
  dplyr::group_by(.data$signal) |>
  dplyr::group_walk(\(x, ...) {
    x$dat |>
      purrr::map(\(y) {
        dplyr::mutate(
          y,
          dplyr::across(dplyr::matches("level_"), \(z) NA)
        )
      }) |>
      dplyr::n_distinct() |>
      testthat::expect_equal(1)
  })

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
  names(prism_signal_specs) |>
  rlang::set_names() |>
  purrr::map(\(signal_name) {
    long_thresholds |>
      prism_signal_long(signal_name) |>
      thresholds_to_array(prism_signal_specs[[signal_name]]$dim_cols)
  })

prism_thresholds |>
  purrr::walk(\(x) testthat::expect_false(anyNA(x)))

# test that array construction is correct
purrr::iwalk(prism_thresholds, \(thresholds, signal_name) {
  dat <- prism_signal_long(long_thresholds, signal_name)

  purrr::walk(1:1000, \(i) {
    tmp_sample <- dimnames(thresholds) |> purrr::map_chr(\(x) sample(x, 1))
    slice_dims <- tmp_sample[names(tmp_sample) != "breaks"]

    testthat::expect_identical(
      array_slice(thresholds, slice_dims),
      long_slice(dat, slice_dims)
    )
  })
})

usethis::use_data(prism_thresholds, overwrite = TRUE)
