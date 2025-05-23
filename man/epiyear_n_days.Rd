% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/epiweek.R
\name{epiyear_n_days}
\alias{epiyear_n_days}
\title{Get the number of days in a given epidemiological
year.}
\usage{
epiyear_n_days(epiyear, epiweek_standard)
}
\arguments{
\item{epiyear}{Integer vector of epidemiological year values.}

\item{epiweek_standard}{One of \code{"MMWR"} or \code{"USA"}
(USA MMWR epiweek, starts on Sunday) and \code{"ISO"} (ISO
week, starts on Monday). Not case-sensitive.
Must be a single value.}
}
\value{
Integer vector of the number(s) of days in the given
epidemiological year(s).
}
\description{
Inspired by the \href{https://epiweeks.readthedocs.io/en/stable/_modules/epiweeks.html#Year.totalweeks}{implementation}
in the \href{https://epiweeks.readthedocs.io/}{\code{epiweeks}} Python package.
}
\examples{

epiyear_n_days(2023, "MMWR")

epiyear_n_days(2023, "USA")

epiyear_n_days(2026, "MMWR")

epiyear_n_days(2026, "ISO")

}
