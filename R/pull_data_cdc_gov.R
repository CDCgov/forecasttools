#' Construct a data.cdc.gov API endpoint from a dataset ID
#'
#' @param dataset_id Dataset ID, as a string.
#' @return The full URL for the (JSON) Socrata Open Data API endpoint.
#' @export
#'
#' @examples
#'
#' data_cdc_gov_endpoint(data_cdc_gov_ids$nhsn_hrd_prelim)
#'
#' data_cdc_gov_endpoint(data_cdc_gov_ids$nssp_prop_ed_visits)
data_cdc_gov_endpoint <- function(dataset_id) {
  return(glue::glue("https://data.cdc.gov/resource/{dataset_id}.json"))
}


#' Pull NHSN data from `data.cdc.gov`
#'
#' Pull relevant epidemiological
#' data from NHSN, defaulting to the
#' [`data.cdc.gov`](https://data.cdc.gov)
#' public API endpoint.
#'
#' @param api_endpoint API endpoint to
#' use. Defaults to the `https:`/`.json` Socrata
#' endpoint for NHSN COVID, Influenza, and RSV Wednesday release of
#' data on [`data.cdc.gov`](https://data.cdc.gov), namely
#' [`data.cdc.gov/resource/mpgq-jmmr.json`
#' ](https://data.cdc.gov/resource/mpgq-jmmr.json)
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
pull_nhsn <- function(
  api_endpoint = data_cdc_gov_endpoint(data_cdc_gov_ids$nhsn_hrd_prelim),
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
  ...
) {
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

  soda_request <- query |>
    as.character() |>
    httr2::request() |>
    httr2::req_user_agent(
      paste0(
        "forecasttools R package ",
        "(https://cdcgov.github.io/forecasttools)"
      )
    )

  api_key_id <- if (is.null(api_key_id)) "" else api_key_id
  api_key_secret <- if (is.null(api_key_secret)) "" else api_key_secret

  credentials <- (api_key_id != "" &
    api_key_secret != "")

  if (credentials) {
    soda_request <- httr2::req_auth_basic(
      soda_request,
      api_key_id,
      api_key_secret
    )
  } else {
    cli::cli_warn(c(
      "No valid API key ID / secret pair provided. ",
      "This is considered impolite and ",
      "may result in your requests to the ",
      "server getting throttled. Create an ",
      "API key id/secret pair by visiting ",
      "https://data.cdc.gov/profile/edit/developer_settings."
    ))
  }

  df <- httr2::req_perform(soda_request) |>
    httr2::resp_body_json() |>
    dplyr::bind_rows() |>
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


#' Construct a Socrata open data
#' API (SODA) query for the NHSN Hospital Respiratory
#' Data set.
#'
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
nhsn_soda_query <- function(
  api_endpoint,
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
  ...
) {
  cols <- if (!is.null(columns)) {
    c("jurisdiction", "weekendingdate", columns)
  } else {
    NULL
  }

  query <- soql::soql() |>
    soql::soql_add_endpoint(api_endpoint) |>
    soql_nullable_select(cols) |>
    soql_nullable_where(
      "weekendingdate",
      ">=",
      start_date
    ) |>
    soql_nullable_where(
      "weekendingdate",
      "<=",
      end_date
    ) |>
    soql_nullable_is_in(
      "jurisdiction",
      jurisdictions
    )

  ## need to add order_by columns sequentially
  ## to ensure the specified desc option is applied to each
  for (x in unique(order_by)) query <- soql::soql_order(query, x, desc = desc)

  ## do limit string formatting
  ## manually since soql::soql_limit()
  ## coerces input to numeric and then
  ## string formats with XeY notation
  ## (e.g. 100000 as '1e5'), which endpoints
  ## will fail to parse
  query$clauses$limit <- sprintf("%d", as.numeric(limit))

  return(query)
}
