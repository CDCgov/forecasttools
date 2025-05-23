% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{us_location_table}
\alias{us_location_table}
\title{Table of US location names and codes}
\format{
A tibble with 58 rows and 3 columns:
\describe{
\item{location_code}{2-character location code}
\item{short_name}{2-character location abbreviation}
\item{long_name}{Full name of the location}
}
}
\source{
\url{https://www2.census.gov/geo/docs/reference/state.txt}

data-raw/us_location_table.R
}
\usage{
us_location_table
}
\description{
A table of US location names and codes, assembled from a
US census reference table with the addition of "US" for the
United States as a whole.
}
\keyword{datasets}
