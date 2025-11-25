mock_scorable_table <- tibble::tibble(
  model = "test_model",
  target_end_date = as.Date("2023-01-01") + 0:1,
  horizon = c(1, 2),
  reference_date = as.Date("2022-12-31"),
  predicted = c(10, 20),
  observed = c(12, 18),
  quantile_level = c(0.5, 0.5)
) |>
  scoringutils::as_forecast_quantile()


silent_temp_save <- function(
  plot,
  save_filename = "test",
  save_filext = "pdf"
) {
  ggplot2::ggsave(
    fs::path(withr::local_tempdir(), save_filename, ext = save_filext),
    plot
  ) |>
    suppressMessages()
}

test_that(
  paste0(
    "plot_pred_obs_by_forecast_date() works as expected ",
    "with scoringutils example quantile forecast"
  ),
  {
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
  }
)


test_that(
  paste0(
    "plot_pred_obs_pointintervals() drops ",
    "reference_date_col if horizon_col present"
  ),
  {
    # Creating a minimal scorable_table with both 'horizon' and
    # 'reference_date' should not error
    expect_no_message({
      result <- plot_pred_obs_pointintervals(mock_scorable_table)
    })
    ## manually dropping reference_date should be equivalent
    expect_no_message({
      result_manual <- plot_pred_obs_pointintervals(
        dplyr::select(mock_scorable_table, -"reference_date")
      )
    })

    expect_equal(result, result_manual)
  }
)

test_that(
  paste0(
    "plot_pred_obs_pointintervals() errors informatively ",
    "if horizon_col is absent"
  ),
  {
    df <- dplyr::select(mock_scorable_table, -"horizon")
    checkmate::expect_names(names(df), disjunct.from = "horizon")

    expect_error(
      plot_pred_obs_pointintervals(df),
      regexp = paste0(
        "Table must have a horizon column.",
        "*horizons_from_target_end_dates"
      )
    )
  }
)
