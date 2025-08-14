test_that(
  paste0(
    "hub_target_data_as_of errors when no as_of column ",
    "exists and as_of is not 'latest'"
  ),
  {
    # Create test data without as_of column
    test_data <- data.frame(
      location = c("US", "CA", "TX"),
      value = c(100, 50, 30),
      date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03"))
    )

    ## Should error when requesting specific date on unvintaged data,
    ## regardless of whether date is passed directly as a date or as
    ## something coercible to a date
    expect_error(
      hub_target_data_as_of(test_data, as_of = as.Date("2023-01-01")),
      "Requested an 'as_of' date other than the default 'latest'"
    )
    expect_error(
      hub_target_data_as_of(test_data, as_of = "2023-01-01"),
      "Requested an 'as_of' date other than the default 'latest'"
    )
  }
)


test_that(
  paste0(
    "hub_target_data_as_of does not error when ",
    "no as_of column if as_of is 'latest' (default)"
  ),
  {
    # Create test data without as_of column
    test_data <- data.frame(
      location = c("US", "CA", "TX"),
      value = c(100, 50, 30),
      date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03"))
    )

    # Should not error with as_of = "latest" on unvintaged data
    result_drop_true <- hub_target_data_as_of(
      test_data,
      .drop = TRUE
    )
    result_drop_false <- hub_target_data_as_of(
      test_data,
      .drop = FALSE
    )

    # Should return original data unchanged
    expect_equal(result_drop_true, test_data)
    expect_equal(result_drop_false, test_data)
  }
)

test_that(
  paste0(
    "hub_target_data_as_of filters to latest available",
    "as_of date when as_of = 'latest'"
  ),
  {
    # Create vintaged test data
    test_data <- data.frame(
      location = rep(c("US", "CA"), 3),
      value = c(100, 50, 110, 55, 120, 60),
      as_of = rep(
        as.Date(c("2023-01-01", "2023-01-02", "2023-01-03")),
        each = 2
      )
    )

    result <- hub_target_data_as_of(test_data)

    ## Should contain only data from latest date (2023-01-03)
    ## and no as_of column
    expected_data <- data.frame(
      location = c("US", "CA"),
      value = c(120, 60)
    )

    expect_equal(result, expected_data)
  }
)

test_that(
  paste0(
    "hub_target_data_as_of filters to a ",
    "specific as_of date when provided, and errors if ",
    "the as_of argument cannot be coerced to a date"
  ),
  {
    # Create vintaged test data
    test_data <- data.frame(
      location = rep(c("US", "CA"), 3),
      value = c(100, 50, 110, 55, 120, 60),
      as_of = rep(
        as.Date(c("2023-01-01", "2023-01-02", "2023-01-03")),
        each = 2
      )
    )

    ## Filter to middle date, passing a string representation
    target_date <- "2023-01-02"
    result <- hub_target_data_as_of(test_data, as_of = target_date)

    ## Filter to middle date, passing a date directly
    result_date <- hub_target_data_as_of(
      test_data,
      as_of = as.Date(target_date)
    )

    ## Should contain only data from 2023-01-02
    ## and no as_of column.
    expected_data <- data.frame(
      location = c("US", "CA"),
      value = c(110, 55)
    )

    expect_equal(result, expected_data)
    expect_equal(result_date, expected_data)

    expect_error(
      hub_target_data_as_of(test_data, as_of = "latesta"),
      "character string is not in a standard unambiguous format"
    )
  }
)

test_that("hub_target_data_as_of respects .drop argument for as_of column", {
  # Create vintaged test data
  test_data <- data.frame(
    location = rep(c("US", "CA"), 2),
    value = c(100, 50, 110, 55),
    as_of = rep(as.Date(c("2023-01-01", "2023-01-02")), each = 2)
  )

  # Test with .drop = FALSE
  result_keep <- hub_target_data_as_of(
    test_data,
    as_of = "latest",
    .drop = FALSE
  )
  expect_true("as_of" %in% colnames(result_keep))
  expect_equal(unique(result_keep$as_of), as.Date("2023-01-02"))

  # Test with .drop = TRUE (default)
  result_drop <- hub_target_data_as_of(
    test_data,
    as_of = "latest",
    .drop = TRUE
  )
  expect_false("as_of" %in% colnames(result_drop))

  # Data content should be the same except for as_of column
  expect_equal(
    dplyr::select(result_keep, -"as_of"),
    result_drop
  )
})

test_that("hub_target_data_as_of works with single row vintage data", {
  # Test edge case with single vintage
  test_data <- data.frame(
    location = "US",
    value = 100,
    as_of = as.Date("2023-01-01")
  )

  result <- hub_target_data_as_of(test_data, as_of = "latest")

  expected_data <- data.frame(
    location = "US",
    value = 100
  )

  expect_equal(result, expected_data)
})
