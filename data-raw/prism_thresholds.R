prism_dim_cols <- c("breaks", "disease", "location", "as_of")

prism_signal_specs <- list(
  "nssp" = list(break_prefix = "prop_", upper_bound = 1),
  "nhsn" = list(break_prefix = "", upper_bound = Inf)
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
      thresholds_to_array(prism_dim_cols)
  })

prism_thresholds |>
  purrr::walk(\(x) testthat::expect_false(anyNA(x)))

# test that array construction is correct
purrr::iwalk(prism_thresholds, \(thresholds, signal_name) {
  dat <- prism_signal_long(long_thresholds, signal_name)
  dims <- dimnames(thresholds)

  purrr::walk(1:1000, \(i) {
    tmp_sample <- dims |> purrr::map_chr(\(x) sample(x, 1))

    testthat::expect_identical(
      thresholds[,
        tmp_sample[["disease"]],
        tmp_sample[["location"]],
        tmp_sample[["as_of"]]
      ] |>
        unname(),

      dat |>
        dplyr::filter(
          .data$as_of == as.Date(tmp_sample[["as_of"]]),
          .data$disease == tmp_sample[["disease"]],
          .data$location == tmp_sample[["location"]]
        ) |>
        dplyr::pull("value")
    )
  })
})

usethis::use_data(prism_thresholds, overwrite = TRUE)
