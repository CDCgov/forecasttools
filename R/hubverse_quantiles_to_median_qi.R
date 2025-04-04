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
    dplyr::mutate(
      .width = .data$.upper_quantile - .data$.lower_quantile
    ) |>
    dplyr::arrange(.data$.width)

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
    dplyr::arrange(.data$.width)

  return(qi_table)
}


#' Convert a hubverse-format table of quantiles into a
#' `ggdist`-format table of quantiles.
#'
#' Expects a hubverse-format quantile table (e.g. as created by
#' [get_hubverse_table()]). Treats all columns in that table other
#' than `output_type_id`, `output_type`, and `value` as grouping / id columns,
#' and returns them in the output table.
#'
#' @param hubverse_quantile_table Hubverse-format quantile table,
#' as an object something coerceible to a [`tibble`][tibble::tibble()].
#' @param .width Vector of interval width(s) to for which to create rows
#' in the output. Default `0.95`, to align with the default value in
#' [ggdist::median_qi()].
#' @param require_only_quantiles Boolean. Error if the table contains
#' `output_type` values other than `"quantile"`? If `FALSE`, will
#' silently filter the table to rows with `output_type == 'quantile'`.
#' Default `FALSE`.
#' @param require_all_widths Boolean. Error if the output is missing any
#' rows for any of the requested `.width`s. If `TRUE`, the function will
#' error if unless it can calculate all the requested intervals for all
#' the groups present. Default `FALSE`.
#' @return A [`tibble`][tibble::tibble()] in the output format of
#' [ggdist::median_qi()]
#' added.
#'
#' @examples
#'
#' hub_quantiles_to_median_qi(hubExamples::forecast_outputs,
#'   .width = c(0.5, 0.8)
#' )
#'
#' @export
hub_quantiles_to_median_qi <- function(hubverse_quantile_table,
                                       .width = 0.95,
                                       require_only_quantiles = FALSE,
                                       require_all_widths = FALSE,
                                       quantile_tol = 10) {
  checkmate::assert_names(names(hubverse_quantile_table),
    must.include = purrr::discard(
      hubverse_std_colnames,
      ~ . == "model_id"
    )
  )
  if (require_only_quantiles) {
    checkmate::assert_names(hubverse_quantile_table$output_type,
      subset.of = "quantile"
    )
  } else {
    hubverse_quantile_table <- hubverse_quantile_table |>
      dplyr::filter(.data$output_type == "quantile")
  }

  qi_table <- widths_to_qi_table(.width, quantile_tol)



  quant_tab <- hubverse_quantile_table |>
    dplyr::select(-"output_type") |>
    dplyr::mutate(
      output_type_id = round(as.numeric(.data$output_type_id),
        digits = quantile_tol
      ) |>
        as.character()
    )
  id_cols <- names(quant_tab) |>
    purrr::discard(~ . %in% c("value", "output_type_id"))

  with_lower <- qi_table |>
    dplyr::mutate(
      .lower_quantile = as.character(.data$.lower_quantile),
      .upper_quantile = as.character(.data$.upper_quantile),
      .point_quantile = "0.5",
      .point = "median",
      .interval = "qi"
    ) |>
    dplyr::left_join(
      quant_tab |> dplyr::rename(
        .lower = "value",
        .lower_quantile = "output_type_id"
      ),
      by = ".lower_quantile"
    )


  result <- with_lower |>
    dplyr::inner_join(
      quant_tab |> dplyr::rename(
        .upper = "value",
        .upper_quantile = "output_type_id"
      ),
      by = c(".upper_quantile", id_cols)
    ) |>
    dplyr::inner_join(
      quant_tab |> dplyr::rename(
        x = "value",
        .point_quantile = "output_type_id"
      ),
      by = c(".point_quantile", id_cols)
    ) |>
    dplyr::select(
      tidyselect::all_of(id_cols),
      "x",
      ".lower",
      ".upper",
      ".width",
      ".point",
      ".interval"
    ) |>
    dplyr::arrange(
      dplyr::across(!!id_cols),
      ".width"
    )


  if (require_all_widths) {
    id_groups <- quant_tab |>
      dplyr::distinct(dplyr::across(!!id_cols)) |>
      tidyr::crossing(.width = !!.width)

    missing <- id_groups |> dplyr::anti_join(
      result,
      by = c(".width", id_cols)
    )
    if (nrow(missing) > 0) {
      cli::cli_abort(c(
        paste0(
          "`require_all_widths` was set to `TRUE` but some ",
          "groups are missing required interval widths:"
        ),
        capture.output(print(missing))
      ))
    }
  }

  return(result)
}
