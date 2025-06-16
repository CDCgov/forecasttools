#' Filter a set of forecasts or forecast scores to
#' include only forecasts present for both halves
#' of a pairwise comparison.
#'
#' Exposes the internal filtering logic of
#' [scoringutils:::compare_forecasts()] to
#' the user. Code adapted from that function, which is
#' MIT licensed.
#'
#' @param forecasts Table of forecasts or scores to filter,
#' as a valid input to [scoringutils::get_forecast_unit()].
#' @param name_comparator1 Character, name of the first comparator.
#' @param name_comparator2 Character, name of the comparator to compare
#' against.
#' @param compare Name of the column containing the comparator values.
#' Default `"model"`.
#' @return Table filtered to contain only forecasts from the two
#' comparators that both share.
#' @examples
#'
#' filter_to_shared_forecasts(scoringutils::example_quantile,
#'                            "EuroCOVIDhub-ensemble",
#'                            "UMass-MechBayes")
#'
#' scoringutils::example_quantile |>
#'     scoringutils::score() |>
#'     filter_to_shared_forecasts("EuroCOVIDhub-ensemble",
#'                                "UMass-MechBayes")
#' @export
filter_to_shared_forecasts <- function(
  forecasts,
  name_comparator1,
  name_comparator2,
  compare = "model"
) {
  forecasts <- data.table::as.data.table(forecasts)
  checkmate::assert_names(names(forecasts), must.include = compare)
  checkmate::assert_scalar(compare)
  forecast_unit <- scoringutils::get_forecast_unit(forecasts)

  if (name_comparator1 == name_comparator2) {
    stop(glue::glue(
      "Must provide two distinct comparator values ",
      "for which to compute shared forecasts. Got ",
      "'{name_comparator1}', '{name_comparator2}'."
    ))
  }

  ## select only columns in c(by, var)
  a <- forecasts[get(compare) == name_comparator1]
  b <- forecasts[get(compare) == name_comparator2]

  ## remove compare column from 'by' before merging
  merge_by <- setdiff(forecast_unit, compare)

  overlap <- dplyr::inner_join(
    a[, ..merge_by],
    b[, ..merge_by],
    by = merge_by
  ) |>
    dplyr::distinct()

  return(dplyr::bind_rows(
    dplyr::inner_join(a, overlap, by = merge_by),
    dplyr::inner_join(b, overlap, by = merge_by)
  ))
}
