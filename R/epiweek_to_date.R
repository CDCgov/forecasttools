#' Get the start day of an epiweek.
#'
#' Get the start day of an epiweek
#' according to different epiweek
#' standards. Returns it as a string
#' that can be passed to [lubridate::wday()]
#' as the `week_start` argument.
#'
#' @param epiweek_standard One of `"MMWR"` or `"USA"`
#' (USA MMWR epiweek, starts on Sunday) and `"ISO"` (ISO
#' week, starts on Monday). Not case-sensitive.
#' @return the start day of the week for the
#' epiweek standard, as an integer that can be passed to
#' [lubridate::wday()] as the `week_start`
#' argument.
#' export
get_epiweek_start <- function(epiweek_standard) {
  standard <- stringr::str_to_lower(epiweek_standard)
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
#' as a [`lubridate::date`][lubridate::date()] object
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
#' USA CDC / MMWR (`"USA"` or `"MMWR"`, the default)
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
#' @param validate Validate the results by passing
#' them back to the appropriate epiweek and epiyear
#' functions? Boolean, default `TRUE`.
#' @return The date, as a
#' [`lubridate::date`][lubridate::date()] object
#' @export
epiweek_to_date <- function(epiweek,
                            epiyear,
                            day_of_week = 1,
                            epiweek_standard = "USA",
                            validate = TRUE) {
  epiweek_standard <- stringr::str_to_lower(epiweek_standard)
  result <- (epiyear_first_date(
    epiyear,
    epiweek_standard = epiweek_standard
  ) +
    lubridate::weeks(epiweek - 1L) +
    lubridate::days(day_of_week - 1L)
  )

  if (validate) {
    if (epiweek_standard == "usa" || epiweek_standard == "mmwr") {
      week_fn <- lubridate::epiweek
      year_fn <- lubridate::epiyear
    } else if (epiweek_standard == "iso") {
      week_fn <- lubridate::isoweek
      year_fn <- lubridate::isoyear
    } else {
      cli::cli_abort(paste0(
        "Could not validate results. ",
        "Got unknown epiweek standard ",
        "{epiweek_standard}"
      ))
    }

    invalid <- week_fn(result) != epiweek | year_fn(result) != epiyear

    if (any(invalid)) {
      failed <- result[invalid]
      cli::cli_abort(paste0(
        "Calculated date(s) {failed} failed validation! ",
        "It does not return the user-requested values ",
        "for `epiweek` and `epiyear` when passed to ",
        "{week_fn} and {year_fn}. ",
        "One reason this can occur is if the user specifies ",
        "an epiweek value greater than the number of epiweeks ",
        "in the specified epiyear. Note that not all epiyears ",
        "contain the same number of epiweeks."
      ))
    }
  }

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
#' @param epiweek_standard Which epiweek standard to use. Passed
#' to [epiweek_to_date()]. Default `"MMWR"`.
#' @param validate Validate the result by
#' passing it back to [lubridate::epiweek()]
#' and [lubridate::epiyear()]? Boolean,
#' default `TRUE`. Passed to [epiweek_to_date()].
#' @return The data frame annotated with the epidate column.
#' @export
with_epidate <- function(df,
                         epiweek_col = "epiweek",
                         epiyear_col = "epiyear",
                         epidate_name = "epidate",
                         day_of_week = 1,
                         epiweek_standard = "MMWR",
                         validate = TRUE) {
  return(df |> dplyr::mutate(!!epidate_name := epiweek_to_date(
    .data[[epiweek_col]],
    .data[[epiyear_col]],
    day_of_week = day_of_week,
    epiweek_standard = epiweek_standard,
    validate = validate
  )))
}
