#' Dataset IDs and metadata for `data.cdc.gov` datasets of interest:
#'
#' `nhsn_hist_daily`: National Healthcare Safety Network
#' (NHSN) daily-resolution incident hospital admissions timeseries,
#' for COVID-19 and later Influenza, reported until 28 June 2024.
#' Former target for the COVID-19 Forecast Hub.
#'
#' `nhsn_hrd_final`, `nhsn_hrd_prelim`: NHSN Hospital Respiratory Data
#' (HRD).
#' Epiweekly-resolution incident hospital admissions
#' timeseries for COVID-19, Influenza, and RSV.
#' `nhsn_hrd_final` points to the most recent final (Friday) weekly data
#' release. `nhsn_hrd_prelim` points the most recent preliminary (Wednesday)
#' weekly release.
#'
#' `nssp_prop_ed_visits`: National Syndromic Surveillance
#' Program (NSSP) data on proportion of incident emergency
#' deparment (ED) visits due to COVID-19, Influenza, and RSV.
#'
#' @seealso [data_cdc_gov_dataset_id()]
#' @export
data_cdc_gov_datasets <- list(
  nhsn_hist_daily = list(
    id = "g62h-syeh",
    date_column = "date",
    location_column = "state"
  ),
  nhsn_hrd_prelim = list(
    id = "mpgq-jmmr",
    date_column = "weekendingdate",
    location_column = "jurisdiction"
  ),
  nhsn_hrd_final = list(
    id = "ua7e-t2fy",
    date_column = "weekendingdate",
    location_column = "jurisdiction"
  ),
  nssp_prop_ed_visits = list(
    id = "rdmq-nq56",
    date_column = "week_end",
    location_column = "county"
  )
)

#' Retrieve the dataset ID for a supported `data.cdc.gov` dataset
#'
#' @param dataset_name Internal forecasttools name for the dataset.
#' See [data_cdc_gov_datasets].
#'
#' @return The dataset id, as a string.
#'
#' @examples
#' data_cdc_gov_dataset_id("nhsn_hrd_prelim")
#'
#' data_cdc_gov_dataset_id("nssp_prop_ed_visits")
#'
#' @export
data_cdc_gov_dataset_id <- function(dataset_name) {
  checkmate::assert_scalar(dataset_name)
  checkmate::assert_names(
    dataset_name,
    subset.of = names(data_cdc_gov_datasets)
  )

  return(data_cdc_gov_datasets[[dataset_name]]$id)
}


.data_cdc_gov_api_creation_url <- paste0(
  "https://data.cdc.gov/",
  "profile/edit/developer_settings"
)

#' Construct a data.cdc.gov API endpoint from a dataset ID
#'
#' @param dataset_id Dataset ID, as a string.
#' @return The full URL for the (JSON) Socrata Open Data API endpoint.
#'
#' @examples
#'
#' data_cdc_gov_endpoint(data_cdc_gov_dataset_id("nhsn_hrd_prelim"))
#'
#' data_cdc_gov_dataset_id("nssp_prop_ed_visits") |>
#'   data_cdc_gov_endpoint()
#'
#' @export
data_cdc_gov_endpoint <- function(dataset_id) {
  return(glue::glue("https://data.cdc.gov/resource/{dataset_id}.json"))
}


#' Construct a base SOQL query pointing at at data.cdc.gov endpoint.
#'
#' @param dataset_id Dataset ID, as a string.
#' @return A [soql:soql()] object pointing at the dataset's JSON
#' API endpoint on `data.cdc.gov`.
#' @examples
#'
#' data_cdc_gov_dataset_id("nssp_prop_ed_visits") |>
#'   data_cdc_gov_endpoint() |>
#'   data_cdc_gov_base_query()
#'
#' @export
data_cdc_gov_base_query <- function(dataset_id) {
  return(
    soql::soql() |>
      soql::soql_add_endpoint(data_cdc_gov_endpoint(dataset_id))
  )
}


#' Helper function for common Socrata open data
#' API (SODA) queries targeted at `data.cdc.gov` datasets.
#'
#' @param dataset_name Name of the dataset to query.
#' @param start_date Pull only rows with dates
#' greater than or equal to this date. If `NULL`,
#' no minimum date. Default `NULL`.
#' @param end_date Pull only rows with dates
#' less than or equal to this date. If `NULL`,
#' no maximum date. Default `NULL`.
#' @param columns Vector of columns to retrieve, in
#' addition to the dataset's date and location columns, which
#' are always retrieved. If `NULL`, retrieve all columns.
#' Default `NULL`.
#' @param locations Vector of locations to
#' retrieve, in the format of the dataset's location
#' column. If `NULL`, retrieve all. Default `NULL`.
#' @param limit limit to the number of rows to retrieve.
#' Default 1e5.
#' @param order_by Vector of columns by which to order the
#' results, if any. Default `NULL` (do not order the dataset.
#' @param desc whether to order descending instead of
#' ascending. Default `FALSE` (order ascending).
#' @param ... additional arguments (ignored for now)
#' @return the query as [soql::soql()] output
#' @export
data_cdc_gov_soda_query <- function(
  dataset_name,
  start_date = NULL,
  end_date = NULL,
  columns = NULL,
  locations = NULL,
  limit = 1e5,
  order_by = NULL,
  desc = FALSE,
  ...
) {
  checkmate::assert_scalar(dataset_name)
  checkmate::assert_names(
    dataset_name,
    subset.of = names(data_cdc_gov_datasets)
  )

  dataset_info <- data_cdc_gov_datasets[[dataset_name]]

  date_col <- dataset_info$date_column
  loc_col <- dataset_info$location_column

  cols <- if (!is.null(columns)) {
    c(loc_col, date_col, columns)
  } else {
    NULL
  }

  query <- data_cdc_gov_base_query(dataset_info$id) |>
    soql_nullable_select(cols) |>
    soql_nullable_where(
      date_col,
      ">=",
      start_date
    ) |>
    soql_nullable_where(
      date_col,
      "<=",
      end_date
    ) |>
    soql_nullable_is_in(
      loc_col,
      locations
    )

  ## need to add order_by columns sequentially
  ## to ensure the specified desc option is applied to each
  if (!is.null(order_by)) {
    for (x in unique(order_by)) {
      query <- soql::soql_order(query, x, desc = desc)
    }
  }

  ## do limit string formatting
  ## manually since soql::soql_limit()
  ## coerces input to numeric and then
  ## string formats with XeY notation
  ## (e.g. 100000 as '1e5'), which endpoints
  ## will fail to parse
  query$clauses$limit <- sprintf("%d", as.numeric(limit))

  return(query)
}


#' Pull a dataset from `data.cdc.gov` with standard selection
#' and filtering options.
#'
#' @param dataset_name Name of the dataset. One of the keys
#' of [data_cdc_gov_datasets].
#' @param api_key_id Key ID of an API key to use
#' when querying the dataset. Not required,
#' but polite and reduces throttling.
#' You can create one at
#' [`data.cdc.gov/profile/edit/developer_settings`
#' ](https://data.cdc.gov/profile/edit/developer_settings).
#' Defaults to the value of the environment variable
#' `DATA_CDC_GOV_API_KEY_ID`, if any.
#' @param api_key_secret Associated key secret
#' for the API key given in `api_key_id`.
#' Defaults to the value of the environment variable
#' `DATA_CDC_GOV_API_KEY_SECRET`, if any.
#' @param start_date Pull only rows with dates
#' greater than or equal to this date. If `NULL`,
#' no minimum date. Default `NULL`.
#' @param end_date Pull only rows with dates
#' less than or equal to this date. If `NULL`,
#' no maximum date. Default `NULL`.
#' @param columns Vector of columns to retrieve, in
#' addition to the location and date columns for the dataset, which
#' are always retrieved. Default `NULL`.
#' @param locations value or values to filter on for the dataset's
#' location column. If `NULL`, do not filter on that column.
#' Default `NULL`.
#' @param order_by column or columns to order (sort) by.
#' Default `NULL` (do not order).
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
pull_data_cdc_gov_dataset <- function(
  dataset_name,
  api_key_id = Sys.getenv("DATA_CDC_GOV_API_KEY_ID"),
  api_key_secret = Sys.getenv("DATA_CDC_GOV_API_KEY_SECRET"),
  start_date = NULL,
  end_date = NULL,
  columns = NULL,
  locations = NULL,
  order_by = NULL,
  desc = FALSE,
  limit = 1e5,
  error_on_limit = TRUE,
  ...
) {
  checkmate::expect_scalar(dataset_name)
  checkmate::expect_names(
    dataset_name,
    subset.of = names(data_cdc_gov_datasets)
  )

  df <- data_cdc_gov_soda_query(
    dataset_name = dataset_name,
    start_date = start_date,
    end_date = end_date,
    columns = columns,
    locations = locations,
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
#' @param location value or values to filter on for the `jurisdiction`
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
  return(
    data_cdc_gov_soda_query(
      dataset_name = "nhsn_hrd_prelim",
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
