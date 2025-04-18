% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/round_epiweek.R
\name{floor_epiweek}
\alias{floor_epiweek}
\alias{ceiling_epiweek}
\alias{floor_isoweek}
\alias{floor_mmwr_epiweek}
\alias{ceiling_isoweek}
\alias{ceiling_mmwr_epiweek}
\title{Get the first date in an epidemiological week.}
\usage{
floor_epiweek(date, epiweek_standard)

ceiling_epiweek(date, epiweek_standard)

floor_isoweek(date)

floor_mmwr_epiweek(date)

ceiling_isoweek(date)

ceiling_mmwr_epiweek(date)
}
\arguments{
\item{date}{A date.}

\item{epiweek_standard}{One of \code{"USA"} or \code{"MMWR"}
(USA / MMWR epiweek, starts on Sunday) and \code{"ISO"} (ISO
week, starts on Monday). Not case-sensitive. Must be a
single value.}
}
\value{
The date that starts or ends the epidemiological week, as a
\code{\link[lubridate:date]{lubridate::date()}}.
}
\description{
Given any date from a ("epiweek") return
the date of the first (\code{floor_} functions) or
final (\code{ceiling_} functions) day of the epiweek,
according to the given epidemiological week standard.
}
\examples{
floor_isoweek(as.Date("2025-03-13")) # rounded to Monday
floor_isoweek(as.Date("2025-03-10")) # this is already Monday
ceiling_isoweek(as.Date("2025-03-13")) # rounded to Sunday
ceiling_isoweek(as.Date("2025-03-16")) # this is already Sunday
floor_mmwr_epiweek(as.Date("2025-03-13")) # rounded down to Sunday
floor_mmwr_epiweek(as.Date("2025-03-09")) # this is already Sunday
ceiling_mmwr_epiweek(as.Date("2025-03-13")) # rounded to Saturday
ceiling_mmwr_epiweek(as.Date("2025-03-15")) # this is already Saturday
floor_epiweek(as.Date("2025-03-13"), epiweek_standard = "iso")
ceiling_epiweek(as.Date("2025-03-13"), epiweek_standard = "USA")
ceiling_epiweek(as.Date("2025-03-13"), epiweek_standard = "mmwr")

}
