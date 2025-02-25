% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recode_locations.R
\name{us_loc_code_to_abbr}
\alias{us_loc_code_to_abbr}
\title{Convert a 2-character USA location code
to a 2-letter USA location abbreviation.}
\usage{
us_loc_code_to_abbr(location_codes)
}
\arguments{
\item{location_codes}{vector of location codes}
}
\value{
vector of the same length recoded as USPS
two letter abbreviations.
}
\description{
Given a vector of US location
codes (legacy FIPS code for states and territories,
\code{US} for the US, the schema typically used in forecast
hubs), return the corresponding state/territory
two-letter USPS short names (e.g. MA, TX, PR).
}
\seealso{
\code{\link[=us_loc_abbr_to_code]{us_loc_abbr_to_code()}}
}
