% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/epiweek.R
\name{epiweek_start}
\alias{epiweek_start}
\alias{epiweek_end}
\title{Get the start or end day of an epiweek.}
\usage{
epiweek_start(epiweek_standard)

epiweek_end(epiweek_standard)
}
\arguments{
\item{epiweek_standard}{One of \code{"MMWR"} or \code{"USA"}
(USA / MMWR epiweek, starts on Sunday) and \code{"ISO"} (ISO
week, starts on Monday). Not case-sensitive.}
}
\value{
the start day of the week for the
epiweek standard, as an integer that can be passed to
\code{\link[lubridate:day]{lubridate::wday()}} as the \code{week_start} argument.
}
\description{
Get the starting or ending day of the
epidemiological week ("epiweek")
according to different epiweek
standards. Returns the day of the week index
as an integer that can be passed to
\code{\link[lubridate:lubridate-package]{lubridate}} functions as a
day-of-the-week index (e.g. as the
\code{week_start} argument to \code{\link[lubridate:day]{lubridate::wday()}}):
1 for Monday, 2 for Tuesday, ... 7 for Sunday.
}
\examples{
epiweek_start("ISO")
epiweek_end("iso")
epiweek_start("MMWR")
epiweek_end("usa")
}
