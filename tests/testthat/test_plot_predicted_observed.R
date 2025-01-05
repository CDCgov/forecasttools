silent_temp_save <- function(plot,
                             save_filename = "test",
                             save_filext = "pdf") {
  ggplot2::ggsave(
    fs::path(withr::local_tempdir(),
      save_filename,
      ext = save_filext
    ),
    plot
  ) |>
    suppressMessages()
}

test_that(paste0(
  "plot_pred_obs_by_forecast_date() works as expected ",
  "with scoringutils example quantile forecast"
), {
  filtered_forecast <- scoringutils::example_quantile |>
    dplyr::filter(
      location == "IT",
      target_type == "Cases",
      model == "epiforecasts-EpiNow2"
    )

  p1 <- plot_pred_obs_by_forecast_date(
    filtered_forecast,
    forecast_date_col = "forecast_date"
  )
  ## confirm plot renders and saves without warnings
  expect_no_warning(silent_temp_save(p1))

  ## attempting to plot invalid data raises
  ## error even before plot is rendered.
  expect_error(
    plot_pred_obs_by_forecast_date(
      scoringutils::example_quantile |>
        dplyr::filter(is.na(model)),
      forecast_date_col = "forecast_date"
    ),
    regexp = "no forecasts are left"
  )

  ## attempting to plot non-existent quantile
  ## raises error
  expect_error(
    plot_pred_obs_by_forecast_date(
      filtered_forecast,
      forecast_date_col = "forecast_date",
      prediction_interval_width = 0.99999
    ),
    regexp = "Quantiles to plot"
  )
})
