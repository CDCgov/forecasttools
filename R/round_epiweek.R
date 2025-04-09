#' Get the first date in an epidemiological week.
#'
#' Given any date from a ("epiweek") return
#' the date of the first (`floor_` functions) or
#' final (`ceiling_` functions) day of the epiweek,
#' according to the given epidemiological week standard.
#'
#' @param date A date.
#' @param epiweek_standard One of `"USA"` or `"MMWR"`
#' (USA / MMWR epiweek, starts on Sunday) and `"ISO"` (ISO
#'  week, starts on Monday). Not case-sensitive.
#' @return The date that starts or ends the epidemiological week, as a
#' [lubridate::date()].
#' @examples
#' floor_isoweek(as.Date("2025-03-13")) # rounded to Monday
#' floor_isoweek(as.Date("2025-03-10")) # this is already Monday
#' ceiling_isoweek(as.Date("2025-03-13")) # rounded to Sunday
#' ceiling_isoweek(as.Date("2025-03-16")) # this is already Sunday
#' floor_mmwr_epiweek(as.Date("2025-03-13")) # rounded down to Sunday
#' floor_mmwr_epiweek(as.Date("2025-03-09")) # this is already Sunday
#' ceiling_mmwr_epiweek(as.Date("2025-03-13")) # rounded to Saturday
#' ceiling_mmwr_epiweek(as.Date("2025-03-15")) # this is already Saturday
#' floor_epiweek(as.Date("2025-03-13"), epiweek_standard = "iso")
#' ceiling_epiweek(as.Date("2025-03-13"), epiweek_standard = "USA")
#' ceiling_epiweek(as.Date("2025-03-13"), epiweek_standard = "mmwr")
#'
#' @export
floor_epiweek <- function(date, epiweek_standard) {
  return(lubridate::floor_date(date,
    "week",
    week_start = epiweek_start(epiweek_standard)
  ))
}

#' @rdname floor_epiweek
#' @export
ceiling_epiweek <- function(date, epiweek_standard) {
  return(lubridate::ceiling_date(date,
    "week",
    week_start = epiweek_end(epiweek_standard),
    change_on_boundary = FALSE
  ))
  ## Note: the parameter name `week_start` is misleading
  ## given how we are using the function here.
  ## It may be helpful to think of it as the
  ## "boundary day of the week".
  ##
  ## That is, ceiling_date() with `scale = "week"`,
  ## `week_start = 6` (i.e. Saturday),
  ## and `change_on_boundary = FALSE` means
  ## "convert non-Saturday dates to the next Saturday,
  ## leaving Saturday dates as they are".
}


#' @rdname floor_epiweek
#' @export
floor_isoweek <- function(date) {
  return(floor_epiweek(date, epiweek_standard = "ISO"))
}

#' @rdname floor_epiweek
#' @export
floor_mmwr_epiweek <- function(date) {
  return(floor_epiweek(date, epiweek_standard = "MMWR"))
}

#' @rdname floor_epiweek
#' @export
ceiling_isoweek <- function(date) {
  return(ceiling_epiweek(date, epiweek_standard = "ISO"))
}

#' @rdname floor_epiweek
#' @export
ceiling_mmwr_epiweek <- function(date) {
  return(ceiling_epiweek(date, epiweek_standard = "MMWR"))
}
