#' Get the start day of an epiweek.
#'
#' Get the start day of an epiweek
#' according to different epiweek
#' standards. Returns it as a string
#' that can be passed to [lubridate::wday()]
#' as the `week_start` argument.
#'
#' @param epiweek_standard One of `"USA"` (USA
#' epiweek, starts on Sunday) and `"ISO"` (ISO
#' week, starts on Monday). Not case-sensitive.
#' @return the start day of the week for the
#' epiweek standard, as a three letter abbreviation
#' (`'Sun'` or `'Mon'`) that can be passed to
#' [lubridate::wday()] as the `week_start`
#' argument.
#' export
get_epiweek_start <- function(epiweek_standard) {
  standard <- stringr::str_to_lower(epiweek_standard)

  epiweek_start <- dplyr::case_when(
    standard == "usa" ~ "Sun",
    standard == "iso" ~ "Mon",
    TRUE ~ NA
  )

  na_starts <- is.na(epiweek_start)
  if (any(na_starts)) {
    cli::cli_abort(paste0(
      "Unknown epiweek standard(s) ",
      "{epiweek_standard[na_starts]}. ",
      "Expected `'USA'` or `'ISO'`"
    ))
  }

  return(epiweek_start)
}

#' Get the first date of an
#' epidemiological year ("epiyear")
#'
#' For the ISO, the epidemiological year
#' ('epiyear') YYYY begins on the Monday
#' of the Monday to Sunday  week
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
#' @param epiweek_standard One of `"USA"` (USA
#' epiweek, starts on Sunday) and `"ISO"` (ISO
#'  week, starts on Monday). Not case-sensitive.
#' Default `"USA"`.
#' @return The first date of the epiyear,
#' as a [`'lubridate::date`][lubridate::date()] object
#' @export
epiyear_first_date <- function(epiyear,
                               epiweek_standard = "USA") {
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


#' Get a date from an epiweek
#' and epiyear
#'
#' Function to convert an
#' epidemiological week and
#' year ("epiweek" and "epiyear")
#' pair to a date in a safe way.
#'
#' By default returns the first
#' date of the epiweek,
#' but can return any date in
#' the epiweek. Two standard
#' definitions of epidemiological
#' weeks and years are supported:
#' USA CDC / MMWR (`"USA"`, the default)
#' and ISO (`"ISO"`).
#'
#' @param epiweek Epidemiological week,
#' as an integer (e.g. `46`)
#' @param epiyear Epidemiological year,
#' as an integer. (e.g. `2022`).
#' @param day_of_week day of the epiweek
#' whose date should be returned, as a
#' 1-indexed integer, so 1 is the first date
#' in the epiweek and 7 the last. Default `1`.
#' @param epiweek_standard One of `"USA"` (USA
#' epiweek, starts on Sunday) and `"ISO"` (ISO
#'  week, starts on Monday). Passed to
#' [epiyear_first_date()]. Not case-sensitive.
#' Default `"USA"`
#' @param validate Validate the answer by
#' passing it back to [lubridate::epiweek()]
#' and [lubridate::epiyear()]? Boolean,
#' default `TRUE`.
#' @return The date, as a
#' [`lubridate::date`][lubridate::date()] object
#' @export
epiweek_to_date <- function(epiweek,
                            epiyear,
                            day_of_week = 1,
                            epiweek_standard = "USA",
                            validate = TRUE) {
  candidates <- (epiyear_first_date(
    epiyear,
    epiweek_standard = epiweek_standard
  ) +
    lubridate::weeks(epiweek - 1L) +
    lubridate::days(day_of_week - 1L)
  )

  if (validate) {
    epiweek_start <- get_epiweek_start(epiweek_standard)

    if (epiweek_start == "Sun") {
      invalid <- (
        lubridate::epiweek(candidates) != epiweek |
          lubridate::epiyear(candidates) != epiyear
      )
    } else if (epiweek_start == "Mon") {
      invalid <- (
        lubridate::isoweek(candidates) != epiweek |
          lubridate::isoyear(candidates) != epiyear
      )
    } else {
      cli::cli_abort(paste0(
        "Could not validate results ",
        "Got unknown epiweek standard ",
        "{epiweek_standard}"
      ))
    }

    if (any(invalid)) {
      failed <- candidates[invalid]
      cli::cli_abort(paste0(
        "Calculated date(s) {failed} failed validation! ",
        "It does not return the user-requested values ",
        "for `epiweek` and `epiyear` when passed to ",
        "`lubridate::epiweek()` and `lubridate::epiyear()`. ",
        "One reason this can occur is if the user specifies ",
        "an epiweek value greater than the number of epiweeks ",
        "in the specified epiyear. Note that not all epiyears ",
        "contain the same number of epiweeks"
      ))
    }
  }

  return(candidates)
}
