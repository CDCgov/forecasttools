test_that("filter_to_shared_forecasts errors appropriately", {
  ## errors if compare column not present
  expect_error(
    filter_to_shared_forecasts(
      scoringutils::example_quantile,
      c("EuroCOVIDhub-ensemble", "EuroCOVIDhub-baseline"),
      compare = "modela"
    ),
    "is missing elements"
  )

  ## errors if compare argument is not length 1
  expect_error(
    filter_to_shared_forecasts(
      scoringutils::example_quantile,
      c("EuroCOVIDhub-ensemble", "EuroCOVIDhub-baseline"),
      compare = c("model", "horizon")
    ),
    "length 1"
  )
})
