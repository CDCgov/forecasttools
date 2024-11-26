test_that("nullable_is_in() returns expected results", {
  test_vec <- c(1, 32, -5, 3, -5.23, 1, 1.0001)
  valid_values_not_null <- c(1, 3)
  valid_values_null <- NULL

  expect_equal(
    nullable_is_in(test_vec, valid_values_not_null),
    test_vec %in% valid_values_not_null
  )

  expect_equal(
    nullable_is_in(test_vec, valid_values_null),
    TRUE
  )
})
