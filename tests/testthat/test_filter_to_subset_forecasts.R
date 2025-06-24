test_that("get_subset_forecasts errors appropriately", {
  ## errors if compare column not present
  expect_error(
    get_subset_forecasts(
      scoringutils::example_quantile,
      "EuroCOVIDhub-ensemble",
      compare = "modela"
    ),
    "is missing elements"
  )

  ## errors if comparator argument is not length 1
  expect_error(
    get_subset_forecasts(
      scoringutils::example_quantile,
      c("EuroCOVIDhub-ensemble", "EuroCOVIDhub-baseline"),
      compare = "model"
    ),
    "length 1"
  )

  ## errors if compare argument is not length 1
  expect_error(
    get_subset_forecasts(
      scoringutils::example_quantile,
      "EuroCOVIDhub-ensemble",
      compare = c("model", "horizon")
    ),
    "length 1"
  )
})

test_that("filter_to_subset_forecasts output has expected properties", {
  forecast_unit <- scoringutils::get_forecast_unit(
    scoringutils::example_quantile
  )
  models <- scoringutils::example_quantile |>
    tibble::as_tibble() |>
    dplyr::distinct(model) |>
    tidyr::drop_na() |>
    dplyr::pull() |>
    head(2)

  purrr::walk(models, \(model) {
    result <- filter_to_subset_forecasts(
      scoringutils::example_quantile,
      model
    ) |>
      dplyr::count(dplyr::across(tidyselect::all_of(forecast_unit)))

    expect_equal(dplyr::n_distinct(result$n), 1)
    expect_in(model, result$model)
  })

  ## filtering with a non-existant model should produce a
  ## zero-length table
  result_zero <- filter_to_subset_forecasts(
    scoringutils::example_quantile,
    "FakeModel"
  )

  expect_equal(nrow(result_zero), 0)
})
