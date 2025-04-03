#' Get a table of quantile intervals ("qi") that can be computed
#' from a given set of absolute quantile levels.
#'
#' These intervals are also called "equal-tailed intervals", and can
#' be computed whenever both the `x` quantile and the `1 - x` are present.
#' (yielding an interval of width `1 - (min(x, 1 - x) / 2)`.
#'
#' Checks that all quantile_levels are between 0 and 1, inclusive.
#'
#' @param quantile_levels Vector of quantile levels from which
#' to get available quantile intervals.
#' @param quantile_tol Round quantile level values to this many
#' decimal places, to avoid problems with floating point number
#' equality comparisons. Passed as the `digits` argument to
#' [base::round()]. Default 10.
#' @return A table of available intervals, as a
#' [`tibble`][tibble::tibble()], with columns
#' `.lower_quantile`, `.upper_quantile`, and `.width`,
#' that give the bounds and the widths of the available
#' intervals.
#'
#' @export
#' @examples
#' get_available_qi(c(0.25, 0.3, 0.5, 0.75, 0.99))
#' get_available_qi(c(0.1, 0.2, 0.5, 0.8, 0.9))
get_available_qi <- function(quantile_levels,
                             quantile_tol = 10) {
  quantile_levels <- as.numeric(quantile_levels) |>
    round(digits = quantile_tol) |>
    unique()
  checkmate::assert_numeric(
    quantile_levels,
    lower = 0,
    upper = 1
  )
  lowers <- quantile_levels[quantile_levels < 0.5]
  uppers <- round(1 - lowers, digits = quantile_tol)

  valid <- uppers %in% quantile_levels

  result <- tibble::tibble(
    .lower_quantile = lowers[valid],
    .upper_quantile = uppers[valid]
  ) |>
    dplyr::arrange(.data$.lower_quantile) |>
    dplyr::mutate(
      .width = .data$.upper_quantile - .data$.lower_quantile
    )

  return(result)
}

#' Get a table of quantile intervals ("qi") corresponding to a
#' vector of interval widths.
#'
#' These intervals are also called "equal-tailed intervals", and can
#' be computed whenever both the `x` quantile and the `1 - x` are present.
#' (yielding an interval of width `1 - (min(x, 1 - x) / 2)`.
#'
#' @param widths Vector of interval widths. All entries
#' must be between 0 and 1, inclusive. Duplicate entries
#' are ignored.
#' @param quantile_tol Round quantile level values to this many
#' decimal places, to avoid problems with floating point number
#' equality comparisons. Passed as the `digits` argument to
#' [base::round()]. Default 10.
#' @return A table of available intervals, as a
#' [`tibble`][tibble::tibble()], with columns
#' `.lower_quantile`, `.upper_quantile`, and `.width`,
#' that give the bounds and the widths of the requested
#' intervals.
widths_to_qi_table <- function(widths,
                               quantile_tol = 10) {
  widths <- as.numeric(widths) |>
    round(digits = quantile_tol) |>
    unique()
  checkmate::assert_numeric(widths,
    lower = 0,
    upper = 1
  )
  qi_table <- tibble::tibble(
    .lower_quantile = 0.5 - widths / 2,
    .upper_quantile = 0.5 + widths / 2,
    .width = widths
  ) |>
    dplyr::arrange(.data$.lower_quantile)

  return(qi_table)
}
