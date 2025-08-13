#' Download a forecast hub from `hub_url` to directory `hub_path`.
#'
#' @param hub_path Path to where forecast hub should be downloaded to.
#' @param force If TRUE then if `hub_path` directory already exists,
#' first delete it and then download the forecast hub.
#' @param hub_url URL to forecast hub on github.
#'
#' @return Absolute path to downloaded hub.
#' @export
download_hub <- function(hub_url, hub_path, force = FALSE) {
  if (fs::dir_exists(hub_path) && !force) {
    cfl <- hub_path
    cli::cli_abort(
      paste0(
        "Directory at {.path {cfl}} exists, ",
        "if you want to replace this directory ",
        "then use `force = TRUE`."
      )
    )
  }
  if (fs::dir_exists(hub_path) && force) {
    fs::dir_delete(hub_path)
  }
  return(gert::git_clone(hub_url, hub_path))
}

#' Update the forecast hub located at the path `hub_path`.
#'
#' @param hub_path Path to forecast hub.
#'
#' @export
update_hub <- function(hub_path) {
  if (fs::dir_exists(hub_path)) {
    gert::git_fetch(repo = I(hub_path), prune = TRUE)
  } else {
    cfl <- hub_path
    cli::cli_abort(
      paste0(
        "Cannot find directory at {.path {cfl}}. ",
        "If you wish to clone a repo you can use ",
        "`forecasttools::download_hub`."
      )
    )
  }
}

#' Collect and reformat hub quantile forecasts for scoring.
#'
#' @param hub_path Local path to hub
#'
#' @return Forecast data as a table.
#' @export
gather_hub_quantile_forecasts <- function(hub_path) {
  hub_connection <- hubData::connect_hub(hub_path)
  forecasts <- hub_connection |>
    dplyr::filter(.data$output_type == "quantile") |>
    hubData::collect_hub()
  return(forecasts)
}


#' Gather location data from a forecast hub.
#'
#' @param hub_path Local path to forecast hub.
#' @param location_file_rel_path The path to the data file for
#' location data relative to forecast hub directory.
#' Defaults to expected path in the FluSight forecast hub.
#' @param file_format Format of the target data file to read.
#' Passed to [read_tabular_file()]
#' One of `"tsv"`, `"csv"`, `"parquet"`. If `NULL`,
#' will be inferred from the file extension. Default
#' `NULL`.
#' @return Table of location data.
#' @export
gather_hub_location_data <- function(
  hub_path,
  location_file_rel_path = fs::path(
    "auxiliary-data",
    "locations.csv"
  ),
  file_format = NULL
) {
  location_data_path <- fs::path(hub_path, location_file_rel_path)
  if (!fs::file_exists(location_data_path)) {
    cfl <- location_data_path
    cli::cli_abort(
      "Cannot find location data file at {.path {cfl}}."
    )
  }

  location_data <- read_tabular_file(
    location_data_path,
    file_format = file_format
  )

  return(location_data)
}

#' Filter to a given vintage of hub target data and drop the `as_of`
#' column.
#'
#' This function succeeds silently on unvintaged target data tables
#' provided the user requests the latest available data. Otherwise,
#' it raises an error when the data set is not vintaged.
#'
#' @param hub_target_data Table of hub target data to filter
#' @param as_of As of date to filter to. If `"latest"` (default)
#' use the latest available vintage.
#' @param .drop Drop the `as_of` column once the dataset
#' has been filtered to a specific vintage? Default `TRUE`.
#' @return The specific requested vintage of target data,
#' potentially with the `as_of` column removed.
#' @export
hub_target_data_as_of <- function(
  hub_target_data,
  as_of = "latest",
  .drop = TRUE
) {
  checkmate::assert_scalar(as_of)
  vintaged <- "as_of" %in% colnames(hub_target_data)
  if (vintaged) {
    if (as_of == "latest") {
      as_of <- max(as.Date(hub_target_data$as_of))
    }
    checkmate::assert_date(as_of)
    hub_target_data <- dplyr::filter(
      hub_target_data,
      as.Date(.data$as_of) == !!as_of
    )
  } else if (as_of != "latest") {
    cli::cli_abort(
      "Requested an 'as_of' date other than the default 'latest', ",
      "but the provided hubverse target data table does not appear",
      "to be vintaged. It has no 'as_of' column."
    )
  }

  if (.drop) {
    hub_target_data <- dplyr::select(
      hub_target_data,
      -tidyselect::any_of("as_of")
    )
  }

  return(hub_target_data)
}
