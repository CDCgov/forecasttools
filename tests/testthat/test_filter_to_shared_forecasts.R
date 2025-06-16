test_that("filter_to_shared_forecasts errors appropriately", {
  # errors if given the same model twice
  expect_error(
    filter_to_shared_forecasts(
      scoringutils::example_quantile,
      "EuroCOVIDhub-ensemble",
      "EuroCOVIDhub-ensemble"
    ),
    "Must provide two distinct comparator values"
  )

  ## errors if compare column not present
  expect_error(
    filter_to_shared_forecasts(
      scoringutils::example_quantile,
      "EuroCOVIDhub-ensemble",
      "EuroCOVIDhub-baseline",
      compare = "modela"
    ),
    "is missing elements"
  )

  ## errors if compare argument is not length 1
  expect_error(
    filter_to_shared_forecasts(
      scoringutils::example_quantile,
      "EuroCOVIDhub-ensemble",
      "EuroCOVIDhub-baseline",
      compare = c("model", "horizon")
    ),
    "length 1"
  )
})
