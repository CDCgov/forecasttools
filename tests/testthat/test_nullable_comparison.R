test_that("nullable_comparison() returns expected results", {
  test_vec <- c(1, 32, -5, 3, -5.23, 1, 1.0001)
  equal_length_vec <- c(1, -2, 3, 5.2, 5, 1, 10.5)
  shorter_length_vec <- c(1, 3)
  single_value <- 3
  null_vec <- NULL

  expect_equal(
    nullable_comparison(test_vec, "%in%", single_value),
    test_vec %in% single_value
  )
  expect_equal(
    nullable_comparison(test_vec, "%in%", shorter_length_vec),
    test_vec %in% shorter_length_vec
  )
  expect_equal(
    nullable_comparison(test_vec, "%in%", null_vec),
    TRUE
  )

  for (operator in c("==", "<", ">", "<=", ">=")) {
    expect_equal(
      nullable_comparison(test_vec, operator, single_value),
      getFunction(operator)(test_vec, single_value)
    )

    expect_equal(
      nullable_comparison(test_vec, operator, equal_length_vec),
      getFunction(operator)(test_vec, equal_length_vec)
    )

    expect_equal(
      nullable_comparison(test_vec, operator, null_vec),
      TRUE
    )
  }
})
