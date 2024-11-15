#' Aggregate a daily timeseries to epiweekly
#'
#' Given a set of daily timeseries /
#' time trajectories in
#' tidy long-form format (i.e. with
#' one row per day per trajectory,
#' with a `.draws` or other trajectory id column),
#' aggregate it to the scale of epiweeks.
#'
#' This requires a `date` column that is a set
#' of timepoints coercible to dates.
#'
#' @param tidy_daily_trajectories tibble of trajectories,
#' with dates, trajectory ids, and observeable values to
#' aggregate
#' @param value_col name of the column containing trajectory
#' values. Default `"value"`.
#' @param date_col name of the column containing dates.
#' Default `"date"`.
#' @param id_cols name(s) of the column(s)
#' that uniquely identify a single timeseries
#' (e.g. a single posterior trajectory).
#' Default `".draw"` (as the output of
#' [tidybayes::spread_draws()]).
#' @param weekly_value_name name to use for the output coluqmn
#' containing weekly trajectory values. Default `"weekly_value"`.
#' @param strict Boolean. If `TRUE` only aggregate an epiweek
#' if all seven days are represented in the trajectory.
#' If `FALSE`, allow partial weeks. Default `TRUE`.
#' @return tibble of aggregated epiweekly trajectories.
#' @export
daily_to_epiweekly <- function(tidy_daily_trajectories,
                               value_col = "value",
                               date_col = "date",
                               id_cols = ".draw",
                               weekly_value_name = "weekly_value",
                               strict = TRUE) {
  if (!(value_col %in% names(tidy_daily_trajectories))) {
    cli::cli_abort(
      c(
        "Provided trajectory data frame is ",
        "missing specified value column ",
        "'{value_col}'"
      )
    )
  }
  if (!(date_col %in% names(tidy_daily_trajectories))) {
    cli::cli_abort(
      c(
        "Provided trajectory data frame is ",
        "missing specified date column ",
        "'{date_col}'"
      )
    )
  }

  if (!all((id_cols %in% names(tidy_daily_trajectories)))) {
    cli::cli_abort(
      c(
        "Provided trajectory data frame is ",
        "missing specified trajectory id column ",
        "'{id_cols}'"
      )
    )
  }

  grouped_df <- tidy_daily_trajectories |>
    dplyr::mutate(
      "epiweek" = lubridate::epiweek(.data[[!!date_col]]),
      "epiyear" = lubridate::epiyear(.data[[!!date_col]])
    ) |>
    dplyr::group_by(
      .data$epiweek,
      .data$epiyear,
      dplyr::across(tidyselect::all_of(!!id_cols))
    )

  n_elements <- grouped_df |>
    dplyr::summarise("n_elements" = dplyr::n()) |>
    dplyr::pull()

  ## check that no weeks have more than
  ## 7 contributing dates
  if (!all(n_elements <= 7)) {
    cli::cli_abort(c(
      "At least one trajectory had more ",
      "than 7 values for a given epiweek ",
      "of a given year. Check your date ",
      "column for repeated values and ",
      "check that the trajectory ",
      "id columns are the ones you intended to ",
      "use. This run used columns named ",
      "'{id_cols}' to identify unique trajectories"
    ))
  }

  ## if strict, use only
  ## weeks that have
  ## *exactly* 7 contributing dates
  if (strict) {
    grouped_df <- grouped_df |>
      dplyr::filter(dplyr::n() == 7)
  }

  df <- grouped_df |>
    dplyr::summarise(
      {{ weekly_value_name }} := sum(.data[[!!value_col]])
    ) |>
    dplyr::ungroup()

  return(df)
}
