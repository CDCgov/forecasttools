#' Summarize a hubverse format hub into a
#' table.
#'
#' Combining one or more forecasts
#' with model metadata and particular
#' quantiles.
hub_summary_table <- function(path_to_hub,
                              reference_dates = NULL,
                              locations = NULL) {
  all_models_meta <- hubData::load_hub_metadata(path_to_hub)
  hub_con <- hubData::connect_hub(hub_path)

  hub_data_table <- hub_con |>
    dplyr::filter(
      nullable_is_in(.data$reference_date, !!reference_date),
      nullable_is_in(.data$location, !!locations)
    ) |>
    hubData::collect_hub()
}
