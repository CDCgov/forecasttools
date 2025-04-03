#' Internal and user-exposed constants for forecasttools.

#' Names of the standard columns in a hubverse table.
#'
#' @export
hubverse_column_names <- c(
  "reference_date",
  "target",
  "horizon",
  "target_end_date",
  "location",
  "output_type",
  "output_type_id",
  "value"
)

#' Integer start date of a USA MMWR epiweek in ISO numbering.
#' Value (7) corresponds to Sunday.
mmwr_epiweek_start <- 7L

#' Integer start date of an ISO in ISO numbering.
#' Value (1) corresponds to Monday.
isoweek_start <- 1L
