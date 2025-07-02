.data_cdc_gov_api_creation_url <- paste0(
  "https://data.cdc.gov/",
  "profile/edit/developer_settings"
)


#' Dataset IDs and metadata for `data.cdc.gov` datasets
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
data_cdc_gov_dataset_table <- dplyr::bind_rows(
  list(
    key = "nhsn_hrd_prelim",
    id = "mpgq-jmmr",
    date_column = "weekendingdate",
    location_column = "jurisdiction"
  ),
  list(
    key = "nhsn_hrd_final",
    id = "ua7e-t2fy",
    date_column = "weekendingdate",
    location_column = "jurisdiction"
  ),
  list(
    key = "nssp_prop_ed_visits",
    id = "rdmq-nq56",
    date_column = "week_end",
    location_column = "county"
  )
)

#' Construct data.cdc.gov API endpoints from dataset IDs
#'
#' @param dataset_id One or more dataset IDs, as a vector of strings.
#' @return The full URL(s) for the (JSON) Socrata Open Data API
#' endpoint(s).
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
  return(as.character(glue::glue(
    "https://data.cdc.gov/resource/{dataset_id}.json"
  )))
}


#' Look up rows of the data.cdc.gov dataset table
#' corresponding to the entries of a given
#' vector.
#'
#' @param dataset vector of dataset keys or ids
#' @param format format in which the input `dataset`
#' vector is coded. One of `"key"` or `"id"`.
#' @param strict Error if not all keys/ids can be matched?
#' Default `FALSE`.
#' @return A [`tibble`][tibble::tibble()] with the
#' corresponding rows of the [data_cdc_gov_dataset_table]
#' matching the location vector (with repeats possible).
#' Returns `NA` rows where matches cannot be found.
#'
#' @seealso [data_cdc_gov_dataset_id()]
#'
#' @examples
#'
#' data_cdc_gov_dataset_lookup(
#'   c("nhsn_hrd_prelim", "nhsn_hrd_final"),
#'   "key")
#'
#' data_cdc_gov_dataset_lookup(
#'   c("rdmq-nq56", "ua7e-t2fy", "ua7e-t2fy"),
#'   "id")
#'
#' data_cdc_gov_dataset_lookup(
#'   c("rdmq-nq56", "unavailable", "ua7e-t2fy"),
#'   "id")
#'
#' @export
data_cdc_gov_dataset_lookup <- function(
  dataset,
  format,
  strict = FALSE
) {
  checkmate::assert_scalar(format)
  checkmate::assert_names(format, subset.of = c("key", "id"))
  checkmate::assert_scalar(strict)
  checkmate::assert_logical(strict)

  mask <- match(
    x = as.character(dataset),
    table = data_cdc_gov_dataset_table[[format]]
  )

  not_found <- is.na(mask)
  if (any(not_found) && strict) {
    cli::cli_abort(paste0(
      "Dataset(s) matching '{format}' = ",
      "{glue::single_quote(dataset[not_found])} not found. Inspect ",
      "{.obj forecasttools::data_cdc_gov_dataset_table} ",
      "to view supported datasets, or set `strict = FALSE` to allow ",
      "non-matches"
    ))
  }

  return(data_cdc_gov_dataset_table[mask, ])
}


#' Retrieve the dataset IDs for supported `data.cdc.gov` datasets
#'
#' @param dataset_key Internal forecasttools name for the dataset.
#' See [data_cdc_gov_dataset_table].
#' @param strict Error on nom-matches? Passed as the `strict`
#' argument to [data_cdc_gov_dataset_lookup()]. Default `FALSE`.
#'
#' @return The dataset ids, as a vector string, with NA values for
#' non-matches if strict is `FALSE`.
#'
#' @seealso [data_cdc_gov_dataset_lookup()]
#'
#' @examples
#' data_cdc_gov_dataset_id("nhsn_hrd_prelim")
#'
#' data_cdc_gov_dataset_id("nssp_prop_ed_visits")
#'
#' data_cdc_gov_dataset_id(c("nssp_prop_ed_visits", "nhsn_hrd_final"))
#'
#' @export
data_cdc_gov_dataset_id <- function(dataset_key, strict = FALSE) {
  return(
    data_cdc_gov_dataset_lookup(
      dataset_key,
      "key",
      strict = strict
    )$id
  )
}


#' Construct a base SOQL query pointing at at data.cdc.gov endpoint.
#'
#' @param dataset_id Dataset ID, as a string.
#' @return A [soql::soql()] object pointing at the dataset's JSON
#' API endpoint on `data.cdc.gov`.
#' @examples
#'
#' data_cdc_gov_dataset_id("nssp_prop_ed_visits") |>
#'   data_cdc_gov_endpoint() |>
#'   data_cdc_gov_base_query()
#'
#' @export
data_cdc_gov_base_query <- function(dataset_id) {
  checkmate::assert_scalar(dataset_id)
  return(
    soql::soql() |>
      soql::soql_add_endpoint(data_cdc_gov_endpoint(dataset_id))
  )
}


#' Helper function for common Socrata open data
#' API (SODA) queries targeted at `data.cdc.gov` datasets.
#'
#' @param dataset_id Dataset ID for the dataset
#' @param date_col Name of the date column for the dataset,
#' if any. Default `NULL`.
#' @param location_col Name of the location column for the dataset,
#' if any. Default `NULL`.
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
#' results, if any. If `NULL` (default), order first by the
#' date column, then by the location column.
#' Having a default ordering makes queries more robust
#' against unpredictable results.
#' @param desc whether to order descending instead of
#' ascending. Default `FALSE` (order ascending).
#' @param ... additional arguments (ignored for now)
#' @return the query as [soql::soql()] output
#'
#' @examples
#'
#' data_cdc_gov_soda_query(
#'   "nhsn_hrd_prelim",
#'   "2025-05-01",
#'   "2025-07-01",
#'   "totalconfflunewadm")
#'
#' @export
data_cdc_gov_soda_query <- function(
  dataset_id,
  date_col = NULL,
  location_col = NULL,
  start_date = NULL,
  end_date = NULL,
  columns = NULL,
  locations = NULL,
  limit = 1e5,
  order_by = NULL,
  desc = FALSE,
  ...
) {
  checkmate::assert_scalar(dataset_id)

  cols <- if (!is.null(columns)) {
    c(location_col, date_col, columns)
  } else {
    NULL
  }

  query <- data_cdc_gov_base_query(dataset_id) |>
    soql_nullable_select(cols)

  if (!is.null(date_col)) {
    checkmate::assert_scalar(date_col)
    query <- query |>
      soql_nullable_where(
        date_col,
        ">=",
        start_date
      ) |>
      soql_nullable_where(
        date_col,
        "<=",
        end_date
      )
  }

  if (!is.null(location_col)) {
    checkmate::assert_scalar(location_col)
    query <- soql_nullable_is_in(
      query,
      location_col,
      locations
    )
  }

  if (is.null(order_by)) {
    order_by <- c(date_col, location_col)
  }
  ## need to add order_by columns sequentially
  ## to ensure the specified desc option is applied to each
  for (x in unique(order_by)) {
    query <- soql::soql_order(query, x, desc = desc)
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
#' @param dataset Dataset key or id (as one of the keys or ids
#' in [data_cdc_gov_dataset_table]. Format determined by the
#' value of `dataset_lookup_format`.
#' @param dataset_lookup_format Format for the `dataset`
#' string. One of `"key"` or `"id"`. Default `"key"`.
#' See [data_cdc_gov_dataset_lookup()].
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
#' are always retrieved if they exist. Default `NULL`.
#' @param locations value or values to filter on for the dataset's
#' location column. If `NULL`, do not filter on that column.
#' Default `NULL`.
#' @param order_by column or columns to order (sort) by.
#' If `NULL` (default) will order first by the date column
#' and then by the location column.
#' @param desc Boolean. Whether to order descending instead of
#' ascending. Default `FALSE` (order ascending).
#' @param limit maximum number of rows to return. Default `1e5`
#' (100000)
#' @param error_on_limit Boolean. Raise an error if the number
#' of rows returned is equal to the maximum? Default `TRUE`.
#' This ensures that one does not silently end up with a
#' subset of the total set of rows matching the query. If a subset
#' is desired, one can set `error_on_limit = FALSE`.
#' @param rename_columns Boolean. Rename the dataset-specific
#' date and location columns to `date` and `location`, respectively?
#' Default `FALSE`.
#' @param ... other arguments passed to [data_cdc_gov_soda_query()]
#' @return the pulled data, as a [`tibble`][tibble::tibble()].
#' @export
pull_data_cdc_gov_dataset <- function(
  dataset,
  dataset_lookup_format = "key",
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
  rename_columns = FALSE,
  ...
) {
  checkmate::assert_scalar(dataset)
  checkmate::assert_scalar(rename_columns)
  checkmate::assert_logical(rename_columns)

  dataset_info <- data_cdc_gov_dataset_lookup(
    dataset,
    dataset_lookup_format,
    strict = TRUE
  )

  df <- data_cdc_gov_soda_query(
    dataset_id = dataset_info$id,
    date_col = dataset_info$date_column,
    location_col = dataset_info$location_column,
    start_date = start_date,
    end_date = end_date,
    columns = columns,
    locations = locations,
    order_by = order_by,
    desc = desc,
    limit = limit,
    ...
  ) |>
    do_soda_query(
      api_key_id = api_key_id,
      api_key_secret = api_key_secret,
      limit = limit,
      error_on_limit = error_on_limit,
      api_key_creation_url = .data_cdc_gov_api_creation_url
    )

  if (rename_columns) {
    df <- dplyr::rename(
      df,
      location = !!dataset_info$location_column,
      date = !!dataset_info$date_column
    )
  }

  return(df)
}
