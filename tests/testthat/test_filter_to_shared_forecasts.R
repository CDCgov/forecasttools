test_that("get_shared_forecasts errors appropriately", {
  ## errors if compare column not present
  expect_error(
    get_shared_forecasts(
      scoringutils::example_quantile,
      c("EuroCOVIDhub-ensemble", "EuroCOVIDhub-baseline"),
      compare = "modela"
    ),
    "is missing elements"
  )

  ## errors if compare argument is not length 1
  expect_error(
    get_shared_forecasts(
      scoringutils::example_quantile,
      c("EuroCOVIDhub-ensemble", "EuroCOVIDhub-baseline"),
      compare = c("model", "horizon")
    ),
    "length 1"
  )
})


test_that("get_shared_forecasts accords with manual expectation", {
  ## EuroCOVIDhub-baseline and EuroCOVIDhub ensemble share all
  ## forecasts
  forecast_unit <- scoringutils::get_forecast_unit(
    scoringutils::example_quantile
  )
  all_ensemble <- dplyr::filter(
    scoringutils::example_quantile |> tibble::tibble(),
    .data$model == "EuroCOVIDhub-ensemble"
  )
  all_baseline <- dplyr::filter(
    scoringutils::example_quantile |> tibble::tibble(),
    .data$model == "EuroCOVIDhub-baseline"
  )
  expected_result <- dplyr::select(
    all_baseline,
    tidyselect::any_of(forecast_unit),
    -"model"
  ) |>
    dplyr::distinct()

  actual_result <- get_shared_forecasts(
    scoringutils::example_quantile,
    c("EuroCOVIDhub-ensemble", "EuroCOVIDhub-baseline")
  )
  actual_result_scores <- get_shared_forecasts(
    scoringutils::score(scoringutils::example_quantile),
    c("EuroCOVIDhub-ensemble", "EuroCOVIDhub-baseline")
  )

  expect_equal(
    expected_result,
    dplyr::select(
      all_ensemble,
      tidyselect::any_of(forecast_unit),
      -"model"
    ) |>
      dplyr::distinct()
  )
  expect_equal(
    data.frame(actual_result),
    data.frame(expected_result)
  )

  expect_equal(
    actual_result,
    actual_result_scores
  )
})


test_that("get_shared_forecasts defaults to all available comparators", {
  result_default <- get_shared_forecasts(scoringutils::example_quantile)
  result_manual <- get_shared_forecasts(
    scoringutils::example_quantile,
    comparator_values = unique(scoringutils::example_quantile$model)
  )
  expect_equal(result_default, result_manual)
})


test_that(
  paste0(
    "filter_to_shared_forecasts defaults to all ",
    "available comparators"
  ),
  {
    result_default <- filter_to_shared_forecasts(
      scoringutils::example_sample_discrete
    )
    result_manual <- filter_to_shared_forecasts(
      scoringutils::example_sample_discrete,
      comparator_values = unique(scoringutils::example_quantile$model)
    )
    expect_equal(result_default, result_manual)
  }
)


test_that("filter_to_shared_forecasts output has expected properties", {
  model_sets <- list(
    c("EuroCOVIDhub-ensemble", "EuroCOVIDhub-baseline"),
    c("EuroCOVIDhub-ensemble", "EuroCOVIDhub-baseline", "UMass-MechBayes")
  )

  purrr::walk(model_sets, \(model_set) {
    result <- filter_to_shared_forecasts(
      scoringutils::example_quantile,
      model_set
    ) |>
      tibble::tibble() |>
      dplyr::summarise(
        count = dplyr::n(),
        .by = "model"
      )
    expect_equal(dplyr::n_distinct(result$count), 1)
    expect_setequal(model_set, result$model)
  })

  ## filtering with a non-existant model should produce a
  ## zero-length table
  result_zero <- filter_to_shared_forecasts(
    scoringutils::example_quantile,
    c("EuroCOVIDhub-ensemble", "EuroCOVIDhub-baseline", "FakeModel")
  )

  expect_equal(nrow(result_zero), 0)

  ## filtering to shared forecasts for a single model
  ## should be equivalent to just filtering to that model.
  result_single <- filter_to_shared_forecasts(
    scoringutils::example_quantile,
    "EuroCOVIDhub-ensemble"
  )
  expected_single <- dplyr::filter(
    scoringutils::example_quantile,
    .data$model == "EuroCOVIDhub-ensemble"
  )
  expect_equal(result_single, expected_single)
})
