% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/round_epiweek.R
\name{floor_isoweek}
\alias{floor_isoweek}
\title{Get the first date in an ISO week}
\usage{
floor_isoweek(date)
}
\arguments{
\item{date}{A date.}
}
\value{
The date that ends the epidemiological week, as a
\code{\link[lubridate:date]{lubridate::date()}}.
}
\description{
Given any date from an ISO week ("isoweek") return
the date of the first day of the isoweek (a Monday).
}
\examples{
floor_isoweek(as.Date("2025-03-13")) # rounded to Monday
floor_isoweek(as.Date("2025-03-10")) # this is already Monday
}
