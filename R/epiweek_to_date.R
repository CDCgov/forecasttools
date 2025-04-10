#' Get a date-to-epiweek function from an
#' epiweek standard.
#'
#' Get the function that converts dates to epiweek numbers
#' for a given epiweek standard.
#'
#' @param epiweek_standard One of `"MMWR"` or `"USA"`
#' (USA MMWR epiweek, starts on Sunday) and `"ISO"` (ISO
#' week, starts on Monday). Not case-sensitive.
#' Must be a single value.
#' @return A function, either [lubridate::epiweek()] or
#' [lubridate::isoweek()], depending on the value of
#' `epiweek_standard`
epiweek_fn <- function(epiweek_standard) {
  epiweek_standard <- stringr::str_to_lower(epiweek_standard)
  epiweek_fns <- c(
    "mmwr" = lubridate::epiweek,
    "usa" = lubridate::epiweek,
    "iso" = lubridate::isoweek
  )

  checkmate::assert_scalar(
    epiweek_standard
  )
  checkmate::assert_names(
    epiweek_standard,
    subset.of = names(epiweek_fns),
  )

  return(epiweek_fns[[epiweek_standard]])
}

#' Get a date-to-epiyear function from an epiweek
#' standard.
#'
#' Get the function that converts dates to epiyear numbers
#' for a given epiweek standard.
#'
#' @param epiweek_standard One of `"MMWR"` or `"USA"`
#' (USA / MMWR epiweek, starts on Sunday) and `"ISO"` (ISO
#' week, starts on Monday). Not case-sensitive. Must be a
#' single value.
#' @return A function, either [lubridate::epiyear()] or
#' [lubridate::isoyear()], depending on the value of
#' `epiweek_standard`
epiyear_fn <- function(epiweek_standard) {
  epiweek_standard <- stringr::str_to_lower(epiweek_standard)
  epiyear_fns <- c(
    "mmwr" = lubridate::epiyear,
    "usa" = lubridate::epiyear,
    "iso" = lubridate::isoyear
  )

  checkmate::assert_scalar(
    epiweek_standard
  )

  checkmate::assert_names(
    epiweek_standard,
    subset.of = names(epiyear_fns),
  )

  return(epiyear_fns[[epiweek_standard]])
}

#' Get a date from an epiweek and epiyear
#'
#' Function to convert an epidemiological week and year
#' ("epiweek" and "epiyear") pair to a date in a safe way.
#'
#' By default returns the first date of the epiweek, but can
#' return any date in the epiweek. Two standard definitions
#' of epidemiological weeks and years are supported: USA
#' CDC / MMWR (`"USA"` or `"MMWR"`, the default) and ISO
#' (`"ISO"`).
#'
#' @param epiweek Epidemiological week, as an integer
#' (e.g. `46`). Must be between 1 and the number of
#' weeks in `epiyear`, as determined by [epiyear_n_weeks()].
#' @param epiyear Epidemiological year, as an integer
#' (e.g. `2022`).
#' @param day_of_week day of the epiweek whose date should
#' be returned, as a 1-indexed integer. 1 is the first date
#' in the epiweek and 7 the last. Default `1`.
#' @param epiweek_standard One of `"USA"` (USA epiweek,
#' starts on Sunday) and `"ISO"` (ISO  week, starts on Monday).
#' Passed to [epiyear_first_date()]. Not case-sensitive.
#' Default `"MMWR"`
#' @return The date, as a [`lubridate::date`][lubridate::date()]
#' object
#' @export
epiweek_to_date <- function(epiweek,
                            epiyear,
                            day_of_week = 1,
                            epiweek_standard = "MMWR") {
  checkmate::assert_scalar(epiweek_standard)
  recycled_inputs <- vctrs::vec_recycle_common(
    epiweek,
    epiyear,
    day_of_week
  )
  epiweek <- recycled_inputs[[1]]
  epiyear <- recycled_inputs[[2]]
  day_of_week <- recycled_inputs[[3]]

  checkmate::assert_integerish(
    epiweek,
    lower = 1,
    upper = 53
  )
  checkmate::assert_integerish(epiyear)

  checkmate::assert_integerish(day_of_week,
    lower = 1,
    upper = 7
  )

  n_weeks <- epiyear_n_weeks(
    epiyear,
    epiweek_standard = epiweek_standard
  )

  wk_not_valid <- epiweek > n_weeks
  if (any(wk_not_valid)) {
    cli::cli_abort(
      c(
        "Some requested epiweeks are not valid: ",
        glue::glue(
          "epiweek {epiweek[wk_not_valid]} of ",
          "epiyear {epiyear[wk_not_valid]}"
        )
      )
    )
  }

  result <- (epiyear_first_date(
    epiyear,
    epiweek_standard = epiweek_standard
  ) +
    lubridate::weeks(as.integer(epiweek) - 1L) +
    lubridate::days(as.integer(day_of_week) - 1L)
  )
  return(result)
}

#' Annotate a dataframe with epiweek and epiyear
#' columns with an epidate column.
#'
#' @param df data frame to annotate.
#' @param epiweek_col Name of the column containing epiweek values.
#' Default `"epiweek"`.
#' @param epiyear_col Name of the column containing epiyear values.
#' Default `"epiyear"`.
#' @param epidate_name Name for the output column containing the
#' associated "epidates". Default `"epidate"`.
#' @param day_of_week Which day of the epidemiological week to use
#' for the epidate. 1-indexed. Passed to [epiweek_to_date()].
#' Default 1 (start date of the epiweek).
#' @param epiweek_standard Which epiweek standard to use.
#' One of `"USA"` or `"MMWR" (USA / MMWR epiweek, starts on Sunday)
#' and `"ISO"` (ISO  week, starts on Monday). Not case-sensitive.
#' Default `"MMWR"`.
#' @return The data frame annotated with the epidate column.
#' @export
with_epidate <- function(df,
                         epiweek_col = "epiweek",
                         epiyear_col = "epiyear",
                         epidate_name = "epidate",
                         day_of_week = 1,
                         epiweek_standard = "MMWR") {
  return(df |> dplyr::mutate(!!epidate_name := epiweek_to_date(
    .data[[epiweek_col]],
    .data[[epiyear_col]],
    day_of_week = day_of_week,
    epiweek_standard = epiweek_standard
  )))
}
