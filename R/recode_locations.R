#' Map location format strings to corresponding
#' us location lookup table column names.
#'
#' Convert a location format string to the name of the
#' corresponding column in [forecasttools::us_location_table],
#' raising an error if an unknown format is provided.
#'
#' @param location_format the format string to convert.
#' Valid values are:
#' -`"abbr"` or `"short_name"`: USPS 2-letter abbreviation
#' - `"code"` or `"hub"`: Legacy 2-digit FIPS code for states and territories,
#' `US` for the US as a whole, the schema typically used in forecast
#' hubs.
#' - `"long_name"` or `"name"`: Full location name, as a string.
#'
#' @return the corresponding column name in
#' [forecasttools::us_location_table].
#' @export
to_us_location_table_column <- function(location_format) {
  col_keys <- c(
    "abbr" = "abbr",
    "short_name" = "abbr",
    "code" = "code",
    "hub" = "code",
    "name" = "name",
    "long_name" = "name"
  )
  checkmate::assert_names(location_format, subset.of = names(col_keys))
  cols <- unname(col_keys[location_format])
  return(cols)
}

#' @rdname to_us_location_table_column
to_location_table_column <- to_us_location_table_column

#' Look up rows of the USA location table
#' corresponding to the entries of a given
#' vector.
#'
#' Get the rows of
#' [forecasttools::us_location_table]
#' corresponding to a given location vector
#' and format, with repeats possible
#' @param location vector of location values
#' @param location_input_format format in which the location
#' vector is coded. See [to_us_location_table_column()]
#' for permitted formats.
#' @param location_output_format Vector specifying column(s) from
#' the output table to return. If not provided (default),
#' return all columns. See [to_us_location_table_column()]
#' for permitted strings to specify columns.
#' @return A [`tibble`][tibble::tibble()] with the
#' corresponding rows of the [us_location_table]
#' matching the location vector (with repeats possible) or the
#' values of those rows for given column(s), as specified in
#' `location_output_format`.
#'
#' @seealso [us_location_recode()]
#'
#' @export
#'
#' @examples
#'
#' us_location_lookup(c("01", "05", "US", "05"), "code")
#'
#' us_location_lookup(c("01", "05", "US", "05"), "code", "name")
#'
#' us_location_lookup(c("01", "05", "US", "05"), "code", c("abbr", "name"))
#'
#' us_location_lookup(c("Alaska", "Hawaii"), "name", c("code", "abbr"))
#'
us_location_lookup <- function(
  location,
  location_input_format,
  location_output_format = NULL
) {
  checkmate::assert_scalar(location_input_format)
  tab_col <- to_us_location_table_column(location_input_format)
  mask <- match(
    x = as.character(location),
    table = forecasttools::us_location_table[[tab_col]]
  )

  result <- forecasttools::us_location_table[mask, ]
  if (!is.null(location_output_format)) {
    result <- result[to_us_location_table_column(location_output_format)]
  }

  return(result)
}


#' @rdname us_location_lookup
#' @export
location_lookup <- us_location_lookup

#' Recode a vector of US locations
#' from one format to another
#'
#' Uses [us_location_lookup()] to perform recoding.
#'
#' @param location vector of location values
#' @param location_input_format format in which the location
#' vector is coded. See [to_us_location_table_column()]
#' for permitted formats. Must be a single value.
#' @param location_output_format Code the output vector in this
#' format. See [to_us_location_table_column()]
#' for permitted formats. Must be a single value.
#' @return The recoded vector.
#'
#' @examples
#'
#' us_location_recode(c("01", "05", "US", "05"), "code", "name")
#'
#' us_location_recode(c("AK", "US", "HI", "AK"), "abbr", "code")
#'
#' us_location_recode("AK", "abbr", "name")
#'
#' @export
us_location_recode <- function(
  location,
  location_input_format,
  location_output_format
) {
  checkmate::assert_scalar(location_input_format)
  checkmate::assert_scalar(location_output_format)
  return(dplyr::pull(us_location_lookup(
    location,
    location_input_format,
    location_output_format = location_output_format
  )))
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
#' @param abbr vector of USPS two letter name abbreviations
#' @return vector of the same length recoded as hub-style
#' location codes
#' @export
#' @seealso [us_location_lookup()]
us_loc_abbr_to_code <- function(abbr) {
  return(us_location_recode(abbr, "abbr", "hub"))
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
#' @param code vector of location codes
#' @return vector of the same length recoded as USPS
#' two letter abbreviations.
#' @export
#' @seealso [us_location_recode()]
us_loc_code_to_abbr <- function(code) {
  return(us_location_recode(code, "hub", "abbr"))
}
