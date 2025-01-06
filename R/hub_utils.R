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

#' Collect and reformat hub forecasts for scoring.
#'
#' Gather hub quantile forecasts.
#'
#' @param hub_path Local path to hub
#'
#' @return Forecast data as a table.
#' @export
gather_hub_quantile_forecasts <- function(hub_path) {
  hub_connection <- hubData::connect_hub(hub_path)
  forecasts <- hub_connection |>
    dplyr::filter(output_type == "quantile") |>
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
gather_hub_location_data <- function(hub_path,
                                     location_file_rel_path =
                                       fs::path(
                                         "auxiliary-data",
                                         "locations.csv"
                                       ),
                                     file_format = NULL) {
  location_data_path <- fs::path(hub_path, location_file_rel_path)
  if (fs::file_exists(location_data_path)) {
    location_data <- read_tabular_file(location_data_path,
      file_format = file_format
    )
    return(location_data)
  } else {
    cfl <- location_data_path
    cli::cli_abort(
      "Cannot find location data file at {.path {cfl}}."
    )
  }
}

#' Gather target truth data from a forecast hub.
#'
#' @param hub_path Local path to hub.
#' @param target_data_rel_path The path to the target
#' data file relative to forecast hub root directory.
#' Defaults to the path in the FluSight Forecast Hub.
#' @param file_format Format of the target data file to read.
#' Passed to [read_tabular_file()]
#' One of `"tsv"`, `"csv"`, `"parquet"`. If `NULL`,
#' will be inferred from the file extension. Default
#' `NULL`.
#' @return Table of target data
#' @export
gather_hub_target_data <- function(hub_path,
                                   target_data_rel_path = fs::path(
                                     "target-data",
                                     "target-hospital-admissions.csv"
                                   ),
                                   file_format = NULL) {
  target_data_path <- fs::path(hub_path, target_data_rel_path)
  if (fs::file_exists(target_data_path)) {
    target_data <- read_tabular_file(target_data_path,
      file_format = file_format
    )
    return(target_data)
  } else {
    cfl <- target_data_path
    cli::cli_abort(
      "Cannot find target data file at {.path {target_data_path}}."
    )
  }
}
