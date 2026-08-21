nhsn_hrd_admissions_columns <- c(
  covid = "totalconfc19newadm",
  flu = "totalconfflunewadm",
  rsv = "totalconfrsvnewadm"
)

#' Get the NHSN HRD column names for a disease's weekly
#' hospital admissions.
#'
#' @param disease Character. One of `"covid"`, `"flu"`,
#' or `"rsv"`.
#' @return Named character vector with `count` and `rate`
#' entries giving the corresponding HRD column names.
#' @export
#' @examples
#' nhsn_hrd_admissions_column_names("covid")
nhsn_hrd_admissions_column_names <- function(disease) {
  checkmate::assert_choice(
    disease,
    choices = names(nhsn_hrd_admissions_columns)
  )
  count_column <- nhsn_hrd_admissions_columns[[disease]]

  c(
    count = count_column,
    rate = paste0(count_column, "per100k")
  )
}

#' Pull weekly hospital admissions from NHSN HRD, as
#' counts and rates. NHSN HRD publishes weekly
#' admissions both as counts and as a rate per 100K
#' population.
#'
#' @param disease Character. One of `"covid"`, `"flu"`, or
#' `"rsv"`.
#' @param dataset Character. Which HRD release to read, either
#' `"nhsn_hrd_prelim"` (the preliminary Wednesday release,
#' the default) or `"nhsn_hrd_final"` (the final Friday
#' release).
#' @param ... Additional arguments passed to
#' [pull_data_cdc_gov_dataset()], such as `start_date`,
#' `end_date`, `locations`, or `limit`.
#' @return A [`tibble`][tibble::tibble()] with columns
#' `weekendingdate`, `jurisdiction`, `count`, and `rate`.
#' `rate` is admissions per 100,000 population, as published.
#'
#' @seealso [pull_data_cdc_gov_dataset()],
#' [nhsn_hrd_admissions_column_names()]
#'
#' @export
pull_nhsn_hrd_admissions <- function(
  disease,
  dataset = "nhsn_hrd_prelim",
  ...
) {
  checkmate::assert_choice(
    dataset,
    choices = c("nhsn_hrd_prelim", "nhsn_hrd_final")
  )
  admissions_columns <- nhsn_hrd_admissions_column_names(disease)

  pull_data_cdc_gov_dataset(
    dataset = dataset,
    columns = unname(admissions_columns),
    ...
  ) |>
    dplyr::mutate(
      weekendingdate = lubridate::as_date(.data$weekendingdate),
      dplyr::across(
        tidyselect::all_of(unname(admissions_columns)),
        as.numeric
      )
    ) |>
    dplyr::select(
      "weekendingdate",
      "jurisdiction",
      count = tidyselect::all_of(admissions_columns[["count"]]),
      rate = tidyselect::all_of(admissions_columns[["rate"]])
    )
}
