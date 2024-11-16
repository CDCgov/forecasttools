#' Pull NHSN data from `data.cdc.gov`
#'
#' Pull relevant epidemiological
#' data from NHSN, defaulting to the
#' [`data.cdc.gov`](https://data.cdc.gov)
#' public API endpoint.
#'
#' @param api_endpoint API endpoint to
#' use. Defaults to the `https:`/`.json` Socrata
#' endpoint for NHSN COVID, Influenza, and RSV
#' data on [`data.cdc.gov`](https://data.cdc.gov), namely
#' [`data.cdc.gov/resource/ua7e-t2fy.json`
#' ](https://data.cdc.gov/resource/ua7e-t2fy.json)
#' @param api_key_id Key ID of an API key to use
#' when querying the dataset. Not required,
#' but polite and reduces throttling.
#' You can create one at
#' [`data.cdc.gov/profile/edit/developer_settings`
#' ](https://data.cdc.gov/profile/edit/developer_settings).
#' Defaults to the value of the environment variable
#' `NHSN_API_KEY_ID`, if any.
#' @param api_key_secret Associated key secret
#' for the API key given in `api_key_id`.
#' Defaults to the value of the environment variable
#' `NHSN_API_KEY_SECRET`, if any.
#' @param start_date Pull only rows with dates
#' greater than or equal to this date. If `NULL`,
#' no minimum date. Default `NULL`.
#' @param end_date Pull only rows with dates
#' less than or equal to this date. If `NULL`,
#' no maximum date. Default `NULL`.
#' @param columns Vector of columns to retrieve, in
#' addition to `weekendingdate` and `jurisdiction`, which are always
#' retrieved. If `NULL`, retrieve all columns.
#' Default `NULL`.
#' @param jurisdictions value or values to filter on for the `jurisdiction`
#' column of the NHSN dataset. If `NULL`, do not filter on that column.
#' Default `NULL`.
#' @param order_by column or columns to order (sort) by.
#' Default `c("jurisdiction", "weekendingdate")`
#' (sort first by jurisdiction, then by date).
#' @param desc Boolean. Whether to order descending instead of
#' ascending. Default `FALSE` (order ascending).
#' @param limit maximum number of rows to return. Default `1e5`
#' (100000)
#' @param error_on_limit Boolean. Raise an error if the number
#' of rows returned is equal to the maximum? Default `TRUE`.
#' This ensures that one does not silently end up with a
#' subset of the total set of rows matching the query. If a subset
#' is desired, one can set `error_on_limit = FALSE`.
#' @param ... other arguments passed to [nhsn_soda_query()]
#' @return the pulled data, as a [`tibble`][tibble::tibble()].
#' @export
pull_nhsn <- function(api_endpoint =
                        "https://data.cdc.gov/resource/ua7e-t2fy.json",
                      api_key_id = Sys.getenv("NHSN_API_KEY_ID"),
                      api_key_secret = Sys.getenv("NHSN_API_KEY_SECRET"),
                      start_date = NULL,
                      end_date = NULL,
                      columns = NULL,
                      jurisdictions = NULL,
                      order_by = c(
                        "jurisdiction",
                        "weekendingdate"
                      ),
                      desc = FALSE,
                      limit = 1e5,
                      error_on_limit = TRUE,
                      ...) {
  query <- nhsn_soda_query(
    api_endpoint,
    start_date = start_date,
    end_date = end_date,
    columns = columns,
    jurisdictions = jurisdictions,
    order_by = order_by,
    desc = desc,
    limit = limit,
    ...
  )

  socrata_url <- as.character(query)

  api_key_id <- if (is.null(api_key_id)) "" else api_key_id
  api_key_secret <- if (is.null(api_key_secret)) "" else api_key_secret

  credentials <- (
    api_key_id != "" &
      api_key_secret != ""
  )

  if (credentials) {
    response <- httr::GET(
      socrata_url,
      httr::authenticate(api_key_id, api_key_secret)
    )
  } else {
    cli::cli_warn(c(
      "No API key ID and secret provided. ",
      "This is considered impolite and ",
      "may result in your requests to the ",
      "server getting throttled. Create an ",
      "API key id/secret pair by visiting ",
      "https://data.cdc.gov/profile/edit/developer_settings"
    ))
    response <- httr::GET(
      socrata_url
    )
  }

  if (response$status != 200) {
    cli::cli_abort("Bad response {response}")
  }

  df <- jsonlite::fromJSON(httr::content(response, "text")) |>
    tibble::as_tibble()

  if (error_on_limit && !dim(df)[1] < limit) {
    cli::cli_abort(c(
      "Query retrieved a number of",
      "records equal to the query limit. ",
      "Some matching records may therefore",
      "be excluded. Try a narrower query, a ",
      "higher limit, or, if this was intended, ",
      "set `error_on_limit = FALSE`"
    ))
  }
  return(df)
}

#' Return a [soql::soql_where()] construct
#' for a given column being in a list of values
#'
#' @param soql_list A `soql` query object, which
#' can be piped in. If one hasn't been
#' created yet, use or pipe in [soql::soql()].
#' @param column The column to filter on
#' @param match_values A vector of values that column
#' must match
#' @return A new soql object with the filter added,
#' for use in other functions.
#' @export
soql_is_in <- function(soql_list, column, match_values) {
  query <- stringr::str_glue(
    "{column}='{match_values}'"
  ) |>
    paste(collapse = " OR ")
  return(soql::soql_where(soql_list, query))
}

#' Construct a Socrata open data
#' API (SODA) query for the NSHN
#' dataset
#' @param api_endpoint Base API endpoint URL to use
#' when constructing the query.
#' @param start_date Pull only rows with dates
#' greater than or equal to this date. If `NULL`,
#' no minimum date. Default `NULL`.
#' @param end_date Pull only rows with dates
#' less than or equal to this date. If `NULL`,
#' no maximum date. Default `NULL`.
#' @param columns Vector of columns to retrieve, in
#' addition to `weekendingdate` and `jurisdiction`, which
#' are always retrieved. If `NULL`, retrieve all columns.
#' Default `NULL`.
#' @param jurisdictions Vector of jurisdictions to
#' retrieve, by two letter US postal service code.
#' If `NULL`, retrieve all. Default `NULL`.
#' @param limit limit to the number of rows to retrieve.
#' Default 1e5.
#' @param order_by Vector of columns by which to order the
#' results. Default `c("jurisdiction", "weekendingdate")`
#' @param desc whether to order descending instead of
#' ascending. Default `FALSE` (order ascending).
#' @param ... additional arguments (ignored for now)
#' @return the query as [soql::soql()] output
#' @export
nhsn_soda_query <- function(api_endpoint,
                            start_date = NULL,
                            end_date = NULL,
                            columns = NULL,
                            jurisdictions = NULL,
                            limit = 1e5,
                            order_by = c(
                              "jurisdiction",
                              "weekendingdate"
                            ),
                            desc = FALSE,
                            ...) {
  query <- soql::soql() |>
    soql::soql_add_endpoint(api_endpoint)

  if (!is.null(columns)) {
    query <- query |>
      soql::soql_select(paste(
        unique(
          c("jurisdiction", "weekendingdate", columns)
        ),
        collapse = ","
      ))
  }

  if (!is.null(start_date)) {
    query <- query |>
      soql::soql_where(
        stringr::str_glue("weekendingdate >= '{start_date}'")
      )
  }

  if (!is.null(end_date)) {
    query <- query |>
      soql::soql_where(
        stringr::str_glue("weekendingdate <= '{end_date}'")
      )
  }

  if (!is.null(jurisdictions)) {
    query <- query |>
      soql_is_in(
        "jurisdiction", jurisdictions
      )
  }

  query <- query |>
    soql::soql_order(
      paste(unique(order_by),
        collapse = ","
      ),
      desc = desc
    )

  ## do limit string formatting
  ## manually since soql::soql_limit()
  ## coerces input to numeric and then
  ## string formats with XeY notation
  ## (e.g. 100000 as '1e5'), which endpoints
  ## will fail to parse
  query$clauses$limit <- sprintf("%d", as.numeric(limit))

  return(query)
}
