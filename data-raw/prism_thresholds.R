#' When there is an appropriate online endpoint for these
#' thresholds, this this script will be updated to point there.
#' For now, it reads from a bundled .tsv in inst/extdata

thresholds <- readr::read_tsv(
  fs::path(
    "inst",
    "extdata",
    "prism_thresholds",
    ext = "tsv"
  ),
  show_col_types = FALSE
)

## transform thresholds from percentage to proprotion
## and store as ordered named vectors in named 3D array

prism_thresholds <- thresholds |>
  dplyr::transmute(
    disease,
    location = state_abb,
    prop_lower_bound = 0,
    prop_low = perc_level_low / 100,
    prop_moderate = perc_level_moderate / 100,
    prop_high = perc_level_high / 100,
    prop_very_high = perc_level_very_high / 100,
    prop_upper_bound = 1
  ) |>
  tidyr::nest(breaks = dplyr::starts_with("prop_")) |>
  dplyr::mutate(
    breaks =
      purrr::map(
        breaks,
        \(x) unlist(x, use.names = TRUE)
      )
  ) |>
  tidyr::nest(loc_breaks = c(location, breaks)) |>
  tibble::deframe() |>
  purrr::map(deframe) |>
  simplify2array(higher = FALSE) |>
  apply(c(1, 2), unlist) |>
  aperm(c(2, 3, 1))

names(dimnames(prism_thresholds)) <- c("location", "disease", "breaks")


usethis::use_data(prism_thresholds,
  overwrite = TRUE
)
