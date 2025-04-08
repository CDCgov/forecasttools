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
get_epiweek_fn <- function(epiweek_standard) {
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
get_epiyear_fn <- function(epiweek_standard) {
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


#' Get the start day of an epiweek.
#'
#' Get the start day of an epiweek
#' according to different epiweek
#' standards. Returns it as a string
#' that can be passed to [lubridate::wday()]
#' as the `week_start` argument.
#'
#' @param epiweek_standard One of `"MMWR"` or `"USA"`
#' (USA / MMWR epiweek, starts on Sunday) and `"ISO"` (ISO
#' week, starts on Monday). Not case-sensitive.
#' @return the start day of the week for the
#' epiweek standard, as an integer that can be passed to
#' [lubridate::wday()] as the `week_start` argument.
#' @export
get_epiweek_start <- function(epiweek_standard) {
  epiweek_standard <- stringr::str_to_lower(epiweek_standard)
  epiweek_starts <- c(
    "mmwr" = mmwr_epiweek_start,
    "usa" = mmwr_epiweek_start,
    "iso" = isoweek_start
  )

  checkmate::assert_names(
    epiweek_standard,
    subset.of = names(epiweek_starts),
    what = "Unique values of 'epiweek_standard'"
  )

  return(unname(epiweek_starts[epiweek_standard]))
}


#' assert that a date corresponds to a given
#' epidemiological week and year, according to the
#' given epiweek standard.
#'
#' @param date Vector of dates to validate.
#' @param expected_epiweek Vector of expected epiweek values.
#' @param expected_epiyear Vector of expected epiyear values.
#' @param epiweek_standard Epiweek standard to use.
#' One of `"USA"` or `"MMWR"` (USA / MMWR
#' epiweek, starts on Sunday) or `"ISO"` (ISO
#'  week, starts on Monday). Not case-sensitive.
#'
#' @return Nothing on success, or raise an error if the assertion
#' fails.
#'
#' @examples
#' assert_date_in_epiweek("2025-01-02", 1, 2025, "USA")
#' assert_date_in_epiweek(
#'   c("2024-12-27", "2025-01-05"),
#'   c(52, 1),
#'   c(2024, 2025),
#'   "ISO"
#' )
#'
#' tryCatch(
#'   assert_date_in_epiweek(
#'     "2024-12-01", 52, 2024, "ISO"
#'   ),
#'   error = \(e) print(e)
#' )
#'
#' @export
assert_date_in_epiweek <- function(date,
                                   expected_epiweek,
                                   expected_epiyear,
                                   epiweek_standard) {
  week_fn <- get_epiweek_fn(epiweek_standard)
  year_fn <- get_epiyear_fn(epiweek_standard)

  invalid <- (
    (week_fn(date) != expected_epiweek) |
      (year_fn(date) != expected_epiyear)
  )

  if (any(invalid)) {
    failed <- date[invalid]
    cli::cli_abort(paste0(
      "Date(s) '{failed}' failed epiweek validation! ",
      "Did not obtain the user-requested values ",
      "for `expected_epiweek` and `expected_epiyear` ",
      "when passed to the epiweek and epiyear functions ",
      "corresponding to epiweek standard '{epiweek_standard}'"
    ))
  }

  invisible()
}

#' Get the first date of an
#' epidemiological year ("epiyear")
#'
#' For the ISO, the epidemiological year ('epiyear') YYYY
#' begins on the Monday of the Monday to Sunday week
#' that includes January 4.
#'
#' The US CDC / MMWR epiyear is analogous
#' but with a Sunday to Saturday week. That is,
#' it begins on the Sunday of the Sunday to
#' Saturday week that includes January 4.
#'
#' Both of these definitions ensure that an epiyear
#' always starts on the first day of a seven day
#' week (Monday for ISO, Sunday for US CDC/MMWR),
#' and specifically on whichever first day of the
#' week falls nearest to 1 January.
#'
#' @param epiyear The epidemiological
#' year for which to get the first date.
#' @param epiweek_standard One of `"USA"` or `"MMWR"`
#' (USA / MMWR epiweek, starts on Sunday) and `"ISO"` (ISO
#'  week, starts on Monday). Not case-sensitive.
#' Default `"MMWR"`.
#' @return The first date of the epiyear,
#' as a [`lubridate::date`][lubridate::date()] object
#' @export
epiyear_first_date <- function(epiyear,
                               epiweek_standard = "MMWR") {
  jan4 <- lubridate::make_date(
    epiyear,
    1,
    4
  )

  epiweek_start <- get_epiweek_start(epiweek_standard)

  dow_jan4 <- lubridate::wday(jan4,
    week_start = epiweek_start
  )

  return(jan4 - lubridate::days(dow_jan4 - 1))
}

#' Get the number of days in a given epidemiological
#' year.
#'
# nolint start
#' Inspired by the [implementation](https://epiweeks.readthedocs.io/en/stable/_modules/epiweeks.html#Year.totalweeks)
# nolint end
#' in the [`epiweeks`](https://epiweeks.readthedocs.io/) Python package.
#'
#' @param epiyear Integer vector of epidemiological year values.
#' @param epiweek_standard One of `"USA"` or `"MMWR"`
#' (USA / MMWR epiweek, starts on Sunday) and `"ISO"` (ISO
#'  week, starts on Monday). Not case-sensitive.
#' Default `"MMWR"`.
#' @return Integer vector of the number(s) of days in the given
#' epidemiological year(s).
#'
#' @examples
#'
#' epiyear_n_days(2023)
#' epiyear_n_weeks(2025)
#' epiyear_n_days(2026)
#' epiyear_n_days(2026, epiweek_standard = "ISO")
#'
#' @export
epiyear_n_days <- function(epiyear,
                           epiweek_standard = "MMWR") {
  this_year_start <- epiyear_first_date(
    epiyear,
    epiweek_standard = epiweek_standard
  )
  next_year_start <- epiyear_first_date(
    epiyear + 1L,
    epiweek_standard = epiweek_standard
  )
  return(as.integer(next_year_start - this_year_start))
}

#' Get the number of epidemiological weeks in a given
#' epidemiological year.
#'
# nolint start
#' Inspired by the [implementation](https://epiweeks.readthedocs.io/en/stable/_modules/epiweeks.html#Year.totalweeks)
# nolint end
#' in the [`epiweeks`](https://epiweeks.readthedocs.io/) Python package.
#'
#' @param epiyear Integer vector of epidemiological year values.
#' @param epiweek_standard One of `"USA"` or `"MMWR"`
#' (USA / MMWR epiweek, starts on Sunday) and `"ISO"` (ISO
#'  week, starts on Monday). Not case-sensitive.
#' Default `"MMWR"`.
#' @return Integer vector of the number(s) of weeks in the given
#' epidemiological year(s).
#'
#' @examples
#'
#' epiyear_n_weeks(2023)
#' epiyear_n_weeks(2025)
#' epiyear_n_weeks(2026)
#' epiyear_n_weeks(2026, epiweek_standard = "ISO")
#' @export
epiyear_n_weeks <- function(epiyear,
                            epiweek_standard = "MMWR") {
  return(epiyear_n_days(
    epiyear,
    epiweek_standard = epiweek_standard
  ) %/% 7L)
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
  epiweek_standard <- stringr::str_to_lower(epiweek_standard)

  checkmate::assert_integerish(day_of_week,
    lower = 1,
    upper = 7
  )
  n_weeks <- epiyear_n_weeks(epiyear,
    epiweek_standard = epiweek_standard
  )
  purrr::map2(
    epiweek,
    n_weeks,
    \(wk, n_wk) {
      checkmate::assert_integerish(wk,
        lower = 1,
        upper = n_wk
      )
    }
  )

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
