#' Get the start or end day of an epiweek.
#'
#' Get the starting or ending day of the
#' epidemiological week ("epiweek")
#' according to different epiweek
#' standards. Returns the day of the week index
#' as an integer that can be passed to
#' [lubridate::lubridate-package] functions as a
#' day-of-the-week index (e.g. as the
#' `week_start` argument to [lubridate::wday()]):
#' 1 for Monday, 2 = Tuesday, ... 7 for Sunday.
#'
#' @param epiweek_standard One of `"MMWR"` or `"USA"`
#' (USA / MMWR epiweek, starts on Sunday) and `"ISO"` (ISO
#' week, starts on Monday). Not case-sensitive.
#' @return the start day of the week for the
#' epiweek standard, as an integer that can be passed to
#' [lubridate::wday()] as the `week_start` argument.
#' @export
#'
#' @examples
#' epiweek_start("ISO")
#' epiweek_end("iso")
#' epiweek_start("MMWR")
#' epiweek_end("usa")
epiweek_start <- function(epiweek_standard) {
  # Integer start date of a USA MMWR epiweek in ISO numbering.
  # Value (7) corresponds to Sunday.
  mmwr_epiweek_start <- 7L

  # Integer start date of an ISO week in ISO numbering.
  # Value (1) corresponds to Monday.
  isoweek_start <- 1L

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

#' @rdname epiweek_start
#' @export
epiweek_end <- function(epiweek_standard) {
  zero_indexed_start <- epiweek_start(epiweek_standard) - 1L
  zero_indexed_end <- (zero_indexed_start + 6L) %% 7L
  one_indexed_end <- zero_indexed_end + 1L
  return(one_indexed_end)
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
  week_fn <- epiweek_fn(epiweek_standard)
  year_fn <- epiyear_fn(epiweek_standard)

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

  return(floor_epiweek(jan4, epiweek_standard = epiweek_standard))
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
  return(as.integer(
    as.numeric(next_year_start - this_year_start, units = "days")
  ))
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
