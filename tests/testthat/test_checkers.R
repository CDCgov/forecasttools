test_that("assert_hubverse_output_types() works as expected", {
  valid_input <- c(
    "sample",
    "quantile",
    "sample",
    "pmf",
    "cdf",
    "mean",
    "median",
    "median"
  )
  expect_no_message(assert_hubverse_output_types(valid_input))
  expect_equal(valid_input, assert_hubverse_output_types(valid_input))
  expect_error(
    assert_hubverse_output_types(c(valid_input, "samplea")),
    "samplea"
  )
})
