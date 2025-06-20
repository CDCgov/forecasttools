#' Map location format strings to corresponding
#' location lookup table column names.
#'
#' Convert a location format string to the name of the
#' corresponding column in [forecasttools::us_location_table],
#' raising an error if an unknown format is provided.
#'
#' @param location_format the format string to convert.
#' @return the corresponding column name in
#' [forecasttools::us_location_table].
#' @export
to_location_table_column <- function(location_format) {
  col_keys <- list(
    "abbr" = "short_name",
    "hub" = "location_code",
    "long_name" = "long_name"
  )
  col_key <- col_keys[[location_format]]

  if (is.null(col_key)) {
    cli::cli_abort(c(
      "Unknown location format {location_format}. ",
      "Expected `'abbr'`, `'hub'`, or `'long_name'`"
    ))
  }
  return(col_key)
}

#' Look up rows of the USA location table
#' corresponding to the entries of a given
#' vector.
#'
#' Get the rows of
#' [forecasttools::us_location_table]
#' corresponding to a given location vector
#' and format, with repeats possible
#' @param location_vector vector of location values
#' @param location_input_format format in which the location
#' vector is coded.
#' Permitted formats are `"abbr"` (state/territory
#' or nation two letter USPS abbreviation), `"hub"`
#' (legacy 2-digit FIPS code for states and territories, `US`
#' for the USA as a whole), and `"long_name"` (full English
#' name for jurisdiction).
#' @param location_output_format Return only this column of the
#' output table, if it is provided. Otherwise return the whole
#' table. Default `NULL` (return all columns).
#  Permitted formats are `"abbr"` (state/territory
#' or nation two letter USPS abbreviation), `"hub"`
#' (legacy 2-digit FIPS code for states and territories, `US`
#' for the USA as a whole), and `"long_name"` (full English
#' name for jurisdiction).
#' @return the corresponding rows of the [us_location_table]
#' matching the location vector, with repeats possible, or the
#' values of those rows for a given column, as specified in
#' `location_output_format`.
#' @export
location_lookup <- function(
  location_vector,
  location_input_format,
  location_output_format = NULL
) {
  tab_col <- to_location_table_column(location_input_format)
  mask <- match(
    x = as.character(location_vector),
    table = forecasttools::us_location_table[[tab_col]]
  )

  result <- forecasttools::us_location_table[mask, ]
  if (!is.null(location_output_format)) {
    result <- result[[to_location_table_column(location_output_format)]]
  }

  return(result)
}


#' Convert a two-letter USA location abbreviation to a
#' two-character USA location code
#'
#' Given a vector of state/territory two-letter
#' USPS short names (e.g. MA, TX, PR), return
#' the corresponding location code typically used
#' in forecast hubs (legacy FIPS code for states and territories,
#' `US` for the US).
#'
#' @param abbrs vector of USPS two letter name abbreviations
#' @return vector of the same length recoded as hub-style
#' location codes
#' @export
#' @seealso [us_loc_code_to_abbr()]
us_loc_abbr_to_code <- function(abbrs) {
  mask <- match(
    x = abbrs,
    table = forecasttools::us_location_table$short_name
  )
  return(
    forecasttools::us_location_table$location_code[mask]
  )
}

#' Convert a 2-character USA location code
#' to a 2-letter USA location abbreviation.
#'
#' Given a vector of US location
#' codes (legacy FIPS code for states and territories,
#' `US` for the US, the schema typically used in forecast
#' hubs), return the corresponding state/territory
#' two-letter USPS short names (e.g. MA, TX, PR).
#'
#' @param location_codes vector of location codes
#' @return vector of the same length recoded as USPS
#' two letter abbreviations.
#' @export
#' @seealso [us_loc_abbr_to_code()]
us_loc_code_to_abbr <- function(location_codes) {
  mask <- match(
    x = location_codes,
    table = forecasttools::us_location_table$location_code
  )
  return(
    forecasttools::us_location_table$short_name[mask]
  )
}
