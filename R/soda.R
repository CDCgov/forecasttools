#' Helper functions for working with the
#' Socrata Open Data API (SODA)

#' Default processing for a SODA response.
#'
#' Returns a data frame and optionally errors if a query row
#' limit was hit.
#'
#' @param response Response to process, as the output of
#' [httr2::req_perform()].
#' @param limit Limit number of rows for the SODA query.
#' @param error_on_limit Raise an error if the number
#' of rows returned is equal to the maximum? Default `TRUE`.
#'
#' @keywords internal
.process_soda_response <- function(
  response,
  limit,
  error_on_limit = TRUE
) {
  df <- httr2::resp_body_json(response) |>
    dplyr::bind_rows() |>
    tibble::as_tibble()

  if (error_on_limit && nrow(df) >= limit) {
    cli::cli_abort(glue::glue(
      "Query retrieved a number of ",
      "records equal to the query limit. ",
      "Some matching records may therefore ",
      "be excluded. Try a narrower query, a ",
      "higher limit, or, if this was intended, ",
      "set `error_on_limit = FALSE`."
    ))
  }

  return(df)
}

#' Perform an abitrary SOQL query against an arbitrary
#' dataset, and return the results as a tibble.
#'
#' @param query Full URL representation of the query to perform,
#' as a [soql::soql()] query object or a character string.
#' @param api_key_id API key id to use when performing the query.
#' Passed to [.perform_api_request()]. Default `NULL`.
#' @param api_key_secret API key secret to use when performing the query.
#' Passed to [.perform_api_request()]. Default `NULL`.
#'#' @param limit Limit number of rows for the SODA query.
#' @param limit Limit number of rows for the SODA query. Default `1e5`.
#' @param error_on_limit Raise an error if the number
#' of rows returned is equal to the maximum? Default `TRUE`.
#' @param api_key_creation_url The user will be warned if they fail
#' to provide an `api_key_id` and `api_key_secret`. Provide this
#' optional argument to direct the user to a website where they can
#' create those credentials. See [.warn_no_api_creds()].
#' @export
perform_soda_query <- function(
  query,
  api_key_id = NULL,
  api_key_secret = NULL,
  limit = 1e5,
  error_on_limit = TRUE,
  api_key_creation_url = NULL
) {
  df <- .perform_api_request(
    as.character(query),
    api_key_id,
    api_key_secret,
    api_key_creation_url
  ) |>
    .process_soda_response(
      limit = limit,
      error_on_limit = error_on_limit
    )

  return(df)
}
