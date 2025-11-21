#' When there is an appropriate online endpoint for these
#' thresholds, this this script will be updated to point there.
#' For now, it reads from bundled .tsv's in inst/extdata
prop_thresholds <-
  tibble::tibble(
    "file_path" = fs::path("inst", "extdata") |>
      fs::dir_ls()
  ) |>
  dplyr::filter(
    file_path |>
      fs::path_file() |>
      stringr::str_detect(
        "^prism_thresholds_\\d{4}-\\d{2}-\\d{2}\\.tsv$"
      )
  ) |>
  dplyr::mutate(
    as_of = file_path |>
      fs::path_file() |>
      stringr::str_extract("\\d{4}-\\d{2}-\\d{2}") |>
      as.Date()
  ) |>
  dplyr::mutate(
    dat = purrr::map(file_path, readr::read_tsv, show_col_types = FALSE)
  ) |>
  dplyr::select(-file_path) |>
  tidyr::unnest("dat") |>
  dplyr::mutate(dplyr::across(
    dplyr::where(is.character),
    stringr::str_to_lower
  )) |>
  dplyr::mutate(dplyr::across(
    dplyr::starts_with("perc_level_"),
    \(x) x / 100
  )) |>
  dplyr::rename_with(
    \(x) stringr::str_replace(x, "perc_level_", "prop_"),
    dplyr::starts_with("perc_level_")
  ) |>
  dplyr::rename("location" = "state_abb") |>
  dplyr::mutate("prop_very_low" = 0, .after = "location") |>
  dplyr::mutate("prop_upper_bound" = 1) |>
  tidyr::pivot_longer(
    cols = dplyr::starts_with("prop_"),
    names_to = "breaks",
    values_to = "value"
  ) |>
  dplyr::mutate(breaks = forcats::fct_inorder(breaks)) |>
  dplyr::arrange(as_of, location, disease, breaks) |>
  dplyr::select("as_of", "location", "disease", "breaks", dplyr::everything())

# convert to array
dims <- prop_thresholds |>
  dplyr::select(-value) |>
  purrr::map(unique) |>
  purrr::map(as.character) |>
  rev()

prism_thresholds <- array(
  data = prop_thresholds$value,
  dim = lengths(dims),
  dimnames = dims
)

usethis::use_data(prism_thresholds, overwrite = TRUE)
