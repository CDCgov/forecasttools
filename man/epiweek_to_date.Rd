% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/epiweek_to_date.R
\name{epiweek_to_date}
\alias{epiweek_to_date}
\title{Get a date from an epiweek and epiyear}
\usage{
epiweek_to_date(epiweek, epiyear, day_of_week = 1, epiweek_standard = "MMWR")
}
\arguments{
\item{epiweek}{Epidemiological week, as an integer
(e.g. \code{46}). Must be between 1 and the number of
weeks in \code{epiyear}, as determined by \code{\link[=epiyear_n_weeks]{epiyear_n_weeks()}}.}

\item{epiyear}{Epidemiological year, as an integer
(e.g. \code{2022}).}

\item{day_of_week}{day of the epiweek whose date should
be returned, as a 1-indexed integer. 1 is the first date
in the epiweek and 7 the last. Default \code{1}.}

\item{epiweek_standard}{One of \code{"MMWR"} or \code{"USA"}
(USA MMWR epiweek, starts on Sunday) and \code{"ISO"} (ISO
week, starts on Monday). Not case-sensitive. Must be a single
value. Default \code{"MMWR"}.}
}
\value{
The date, as a \code{\link[lubridate:date]{lubridate::date}}
object
}
\description{
Function to convert an epidemiological week and year
("epiweek" and "epiyear") pair to a date in a safe way.
}
\details{
By default returns the first date of the epiweek, but can
return any date in the epiweek. Two standard definitions
of epidemiological weeks and years are supported: USA
CDC / MMWR  and ISO.
}
\examples{

epiweek_to_date(5, 2025)

epiweek_to_date(5, 2025, day_of_week = 7)

epiweek_to_date(5, 2025, day_of_week = 7, epiweek_standard = "ISO")

}
