test_that("get_available_qi works as expected", {
  expect_error(
    get_available_qi(c(0.25, 0.5, 0.75, 1.01)),
    "<= 1"
  )
  expect_error(
    get_available_qi(c(0.25, 0.5, 0.75, -0.25)),
    ">= 0"
  )

  expect_equal(
    get_available_qi(c(0, 0.25, 0.3, 0.5, 0.75, 0.99)),
    tibble::tibble(
      .lower_quantile = 0.25,
      .upper_quantile = 0.75,
      .width = 0.5
    )
  )

  expect_equal(
    get_available_qi(c(0.02, 0.1, 0.2, 0.5, 0.8, 0.9, 1)),
    tibble::tibble(
      .lower_quantile = c(0.1, 0.2),
      .upper_quantile = c(0.9, 0.8),
      .width = c(0.8, 0.6)
    )
  )
  # works with out of order and duplicate levels
  expect_equal(
    get_available_qi(c(1, 0.8, 0.8, 0.8, 0, 0.5, 0.2, 0.9, 0.1)),
    tibble::tibble(
      .lower_quantile = c(0, 0.1, 0.2),
      .upper_quantile = c(1, 0.9, 0.8),
      .width = c(1, 0.8, 0.6)
    )
  )
})
