#' Pull NHSN data from `data.cdc.gov`
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function has been replaced by [pull_data_cdc_gov_dataset()],
#' which has a slightly different interface and better
#' leverages general tooling for interacting with data.cdc.gov.
#'
#' Pull relevant epidemiological
#' data from NHSN, defaulting to the
#' [`data.cdc.gov`](https://data.cdc.gov)
#' public API endpoint.
#'
#' @param api_endpoint API endpoint to
#' use. Defaults to the `https:`/`.json` Socrata
#' endpoint for NHSN COVID, Influenza, and RSV Wednesday release of
#' data on [`data.cdc.gov`](https://data.cdc.gov). See
#' [data_cdc_gov_endpoint()] and [data_cdc_gov_dataset_id()].
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
  api_endpoint = data_cdc_gov_endpoint(data_cdc_gov_dataset_id(
    "nhsn_hrd_prelim"
  )),
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
  lifecycle::deprecate_warn(
    "0.1.7",
    "pull_nhsn()",
    details = "Use pull_nhsn_hrd() instead."
  )
  df <- nhsn_soda_query(
    api_endpoint,
    start_date = start_date,
    end_date = end_date,
    columns = columns,
    jurisdictions = jurisdictions,
    order_by = order_by,
    desc = desc,
    limit = limit,
    ...
  ) |>
    perform_soda_query(
      api_key_id = api_key_id,
      api_key_secret = api_key_secret,
      limit = limit,
      error_on_limit = error_on_limit,
      api_key_creation_url = .data_cdc_gov_api_creation_url
    )

  return(df)
}


#' Construct a Socrata open data
#' API (SODA) query for the NHSN Hospital Respiratory
#' Data set.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function has been replaced the more general
#' [data_cdc_gov_soda_query()], which it now uses
#' internally. Use that function instead.
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
#' @param ... additional arguments passed to
#' [data_cdc_gov_soda_query()].
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
  dataset_info <- data_cdc_gov_dataset_lookup("nhsn_hrd_prelim", "key")
  return(
    data_cdc_gov_soda_query(
      dataset_id = dataset_info$id,
      date_col = dataset_info$date_column,
      location_col = dataset_info$location_column,
      start_date = start_date,
      end_date = end_date,
      columns = columns,
      locations = jurisdictions,
      limit = limit,
      order_by = order_by,
      desc = desc,
      ...
    ) |>
      soql::soql_add_endpoint(api_endpoint)
  )
}
