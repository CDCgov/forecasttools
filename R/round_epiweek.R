#' Get the first date in a US/MMWR epidemiological week
#'
#' Given any date from a US/MMWR epidemiological week ("epiweek") return
#' the date of the first day of the epiweek (a Monday).
#'
#' @param date A date.
#' @return The date that ends the epidemiological week, as a
#' [lubridate::date()].
#' @examples
#' floor_mmwr_epiweek(as.Date("2025-03-13")) # rounded down to Sunday
#' floor_mmwr_epiweek(as.Date("2025-03-08")) # this is already Sunday
#' @export
floor_mmwr_epiweek <- function(date) {
  return(lubridate::floor_date(date,
    "week",
    week_start = 7
  ))
}


#' Get the first date in an ISO week
#'
#' Given any date from an ISO week ("isoweek") return
#' the date of the first day of the isoweek (a Monday).
#'
#' @param date A date.
#' @return The date that ends the epidemiological week, as a
#' [lubridate::date()].
#' @examples
#' floor_isoweek(as.Date("2025-03-13")) # rounded to Monday
#' floor_isoweek(as.Date("2025-03-09")) # this is already Monday
#' @export
floor_isoweek <- function(date) {
  return(lubridate::floor_date(date,
    "week",
    week_start = 1
  ))
}


#' Get the final date in a US/MMWR epidemiological week
#'
#' Given any date from a US/MMWR epidemiological week ("epiweek") return
#' the date of the final day of the epiweek (a Saturday).
#'
#' @param date A date.
#' @return The date that ends the epidemiological week, as a
#' [lubridate::date()].
#' @examples
#' ceiling_mmwr_epiweek(as.Date("2025-03-13")) # rounded to Saturday
#' ceiling_mmwr_epiweek(as.Date("2025-03-15")) # this is already Saturday
#' @export
ceiling_mmwr_epiweek <- function(date) {
  return(lubridate::ceiling_date(date,
    "week",
    week_start = 6,
    change_on_boundary = FALSE
  ))
}


#' Get the final date in an ISO week
#'
#' Given any date from an ISO week ("isoweek") return
#' the date of the final day of the isoweek (a Sunday).
#'
#' @param date A date.
#' @return The date that ends the epidemiological week, as a
#' [lubridate::date()].
#' @examples
#' ceiling_isoweek(as.Date("2025-03-13")) # rounded to Sunday
#' ceiling_isoweek(as.Date("2025-03-16")) # this is already Sunday
#' @export
ceiling_isoweek <- function(date) {
  return(lubridate::ceiling_date(date,
    "week",
    week_start = 7,
    change_on_boundary = FALSE
  ))
}
