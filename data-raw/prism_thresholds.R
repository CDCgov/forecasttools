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

## Transform thresholds from percentage to proprotion
prop_thresholds <- thresholds |>
  dplyr::transmute(
    disease,
    location = state_abb,
    prop_lower_bound = 0,
    prop_low = perc_level_low / 100,
    prop_moderate = perc_level_moderate / 100,
    prop_high = perc_level_high / 100,
    prop_very_high = perc_level_very_high / 100,
    prop_upper_bound = 1
  )

#' Transform thresholds from flat table to
#' multi-dimensional array, via a nested list.
#'
#' Method for conversion to multi-dim array:
#' 1. Transform the long-form tabular data to a
#' nested named list (via nest() and deframe())
#' 2. Transform the nested named list to a multi-dimensional
#' array with dimension names (via simplify2array() and unlist())
#' 3. Order and name the dimensions of that array.

thresholds_nested_list <- prop_thresholds |>
  tidyr::nest(breaks = dplyr::starts_with("prop_")) |>
  tidyr::nest(loc_breaks = c(location, breaks)) |>
  tibble::deframe() |>
  purrr::map(deframe) # yields a nested list of tibbles

prism_thresholds <- thresholds_nested_list |>
  simplify2array() |> # yields a 2D array of length-1 tibbles
  apply(1:2, unlist) |> # yields a 3D array
  aperm(c(2, 3, 1)) # orders 3D array dimensions as desired

names(dimnames(prism_thresholds)) <- c("location", "disease", "breaks")

usethis::use_data(prism_thresholds, overwrite = TRUE)
