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
#' floor_mmwr_epiweek(as.Date("2025-03-09")) # this is already Sunday
#' @export
floor_mmwr_epiweek <- function(date) {
  return(lubridate::floor_date(date,
    "week",
    week_start = mmwr_epiweek_start
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
#' floor_isoweek(as.Date("2025-03-10")) # this is already Monday
#' @export
floor_isoweek <- function(date) {
  return(lubridate::floor_date(date,
    "week",
    week_start = isoweek_start
  ))
}


#' Get the final date in a US/MMWR epidemiological week
#'
#' Given any date from a US/MMWR epidemiological week ("epiweek"),
#' return the date of the final day of that epiweek (a Saturday).
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
    week_start = mmwr_epiweek_end,
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


#' Get the final date in an ISO week
#'
#' Given any date from an ISO week ("isoweek") return
#' the date of the final day of that isoweek (a Sunday).
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
    week_start = isoweek_end,
    change_on_boundary = FALSE
  ))
  ## Note: the parameter name `week_start` is misleading
  ## given how we are using the function here.
  ## It may be helpful to think of it as the
  ## "boundary day of the week".
  ##
  ## That is, ceiling_date() with `scale = "week"`,
  ## `week_start = 7` (i.e. Sunday),
  ## and `change_on_boundary = FALSE` means
  ## "convert non-Sunday dates to the next Sunday,
  ## leaving Sunday dates as they are".
}
