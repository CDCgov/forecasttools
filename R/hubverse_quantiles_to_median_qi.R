#' Get a table of quantile interval widths that can be computed
#' from a given set of absolute quantile levels.
#'
#' These intervals are also called "equal-tailed intervals". A quantile
#' interval can be computed whenver both the `x` quantile and the
#' `1 - x` quantile are present, yielding an interval of width
#' `abs(1 - 2 * x)`.
#'
#' Checks that all quantile_levels are between 0 and 1, inclusive.
#'
#' @param quantile_levels Vector of quantile levels from which
#' to get available quantile intervals.
#' @param quantile_tol Round quantile level values to this many
#' decimal places, to avoid problems with floating point number
#' equality comparisons. Passed as the `digits` argument to
#' [base::round()]. Default 10.
#' @return A vector of widths that can be computed.
#'
#' @export
#' @examples
#' get_available_qi_widths(c(0.25, 0.3, 0.5, 0.75, 0.99))
#' get_available_qi_widths(c(0.1, 0.2, 0.5, 0.8, 0.9))
get_available_qi_widths <- function(quantile_levels,
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

  return(sort(uppers[valid] - lowers[valid]))
}

#' Get a table of quantile intervals ("qi") corresponding to a
#' vector of interval widths.
#'
#' These intervals are also called "equal-tailed intervals". An interval
#' of width `x` ranges from the `0.5 - x / 2` quantile to the
#' `0.5 + x / 2` quantile.
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
#' than `output_type_id`, `output_type`, and `value` as grouping / id
#' columns, and returns them in the output table.
#'
#' @param hubverse_quantile_table Hubverse-format quantile table,
#' as an object something coerceible to a [`tibble`][tibble::tibble()].
#' @param .width Vector of interval width(s) to for which to create rows
#' in the output. If `NULL` (the default), use all and only the widths
#' that can be computed from quantiles available in
#' `hubverse_quantile_table`, as determined by [get_available_qi_widths].
#' @param require_only_quantiles Boolean. Error if the table contains
#' `output_type` values other than `"quantile"`? If `FALSE`, the function
#' will silently filter the table to rows with `output_type == 'quantile'`.
#' Default `TRUE`.
#' @param require_all_medians Boolean. Error if the output is missing
#' any medians? If `TRUE`, the function will succeed if it can find a
#' median (`0.5` quantile) value for all groups present in the input
#' and otherwise will error. Default `TRUE`.
#' @param require_all_widths Boolean. Error if the output is missing
#' any of the requested `.width`s for any group? If `TRUE`, the
#' function will succeed if it can calculate all the requested
#' interval `.width`s for all the groups present in the input and
#' otherwise will error. Default `TRUE`.
#' @param quantile_tol Round quantile level values to this many
#' decimal places, to avoid problems with floating point number
#' equality comparisons. Passed as the `digits` argument to
#' [base::round()]. Default 10.
#' @return A [`tibble`][tibble::tibble()] in the output format of
#' [ggdist::median_qi()]
#' added.
#'
#' @examples
#' hub_quantiles_to_median_qi(
#'   hubExamples::forecast_outputs |>
#'     dplyr::filter(.data$output_type == "quantile"),
#'   .width = c(0.5, 0.8)
#' )
#'
#' hub_quantiles_to_median_qi(hubExamples::forecast_outputs,
#'   .width = c(0.5),
#'   require_only_quantiles = FALSE
#' )
#'
#' hub_quantiles_to_median_qi(hubExamples::forecast_outputs,
#'   require_only_quantiles = FALSE
#' )

#' @export
hub_quantiles_to_median_qi <- function(hubverse_quantile_table,
                                       .width = NULL,
                                       require_only_quantiles = TRUE,
                                       require_all_medians = TRUE,
                                       require_all_widths = TRUE,
                                       quantile_tol = 10) {
  checkmate::assert_names(names(hubverse_quantile_table),
    must.include = purrr::discard(
      hubverse_std_colnames,
      ~ . == "model_id"
    )
  )
  if (require_only_quantiles) {
    checkmate::assert_names(unique(hubverse_quantile_table$output_type),
      identical.to = "quantile"
    )
  } else {
    hubverse_quantile_table <- hubverse_quantile_table |>
      dplyr::filter(.data$output_type == "quantile")
  }

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
  id_groups <- quant_tab |>
    dplyr::distinct(dplyr::across(!!id_cols))


  if (require_all_medians) {
    missing <- id_groups |>
      dplyr::mutate(output_type_id = "0.5") |>
      dplyr::anti_join(quant_tab,
        by = c("output_type_id", id_cols)
      )
    if (nrow(missing) > 0) {
      cli::cli_abort(c(
        paste0(
          "`require_all_medians` was set to `TRUE` but some ",
          "groups are missing medians: "
        ),
        utils::capture.output(print(missing))
      ))
    }
  }

  if (is.null(.width)) {
    .width <- get_available_qi_widths(quant_tab$output_type_id)
  }

  qi_table <- widths_to_qi_table(.width, quantile_tol)


  result <- qi_table |>
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
    ) |>
    dplyr::inner_join(
      quant_tab |> dplyr::rename(
        .upper = "value",
        .upper_quantile = "output_type_id"
      ),
      by = c(".upper_quantile", id_cols)
    ) |>
    dplyr::left_join(
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
    id_groups_widths <- id_groups |>
      tidyr::crossing(.width = !!.width)

    missing <- id_groups_widths |>
      dplyr::anti_join(
        result,
        by = c(".width", id_cols)
      )
    if (nrow(missing) > 0) {
      cli::cli_abort(c(
        paste0(
          "`require_all_widths` was set to `TRUE` but some ",
          "groups are missing required interval widths:"
        ),
        utils::capture.output(print(missing))
      ))
    }
  }

  return(result)
}
