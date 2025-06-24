test_that(
  paste0(
    "us_loc_abbr_to_code returns correct codes ",
    "and NAs and preserves length"
  ),
  {
    abbrs <- c("MA", "TX", "PR", "ZZ") # "ZZ" is invalid
    expected_codes <- forecasttools::us_location_table$code[
      match(abbrs, forecasttools::us_location_table$abbr)
    ]
    result <- us_loc_abbr_to_code(abbrs)
    expect_equal(length(result), length(abbrs))
    expect_equal(expected_codes[4], as.character(NA))
    expect_equal(result, expected_codes)
  }
)

test_that(
  paste0(
    "us_loc_code_to_abbr returns correct abbreviations",
    "and NAs and preserves length"
  ),
  {
    codes <- c("25", "48", "XX", 72) # "XX" is invalid
    expected_abbrs <- forecasttools::us_location_table$abbr[
      match(codes, forecasttools::us_location_table$code)
    ]
    result <- us_loc_code_to_abbr(codes)
    expect_equal(length(result), length(codes))
    expect_equal(expected_abbrs[3], as.character(NA))
    expect_equal(result, expected_abbrs)
  }
)

test_that(
  paste0(
    "to_us_location_table_column returns correct column names ",
    "and errors on invalid input, both scalar and vectorized"
  ),
  {
    expect_equal(to_us_location_table_column("abbr"), "abbr")
    expect_equal(to_us_location_table_column("short_name"), "abbr")
    expect_equal(to_us_location_table_column("code"), "code")
    expect_equal(to_us_location_table_column("hub"), "code")
    expect_equal(to_us_location_table_column("name"), "name")
    expect_equal(to_us_location_table_column("long_name"), "name")
    expect_equal(
      to_us_location_table_column(
        c("long_name", "name", "abbr")
      ),
      c("name", "name", "abbr")
    )
    expect_error(
      to_us_location_table_column("invalid_name"),
      "has additional elements"
    )
    expect_error(
      to_us_location_table_column(c("name", "invalid_name")),
      "has additional elements"
    )
  }
)

test_that(
  paste0(
    "us_location_lookup returns correct rows and columns ",
    "for valid, invalid, and mixed input"
  ),
  {
    abbrs <- c("MA", "TX", "PR", "ZZ")
    # Get expected rows
    expected <- forecasttools::us_location_table[
      match(abbrs, forecasttools::us_location_table$abbr),
    ]

    result <- us_location_lookup(abbrs, "abbr")
    expect_equal(result, expected)

    # Output column only
    expected_codes <- forecasttools::us_location_table[
      match(abbrs, forecasttools::us_location_table$abbr),
      "code"
    ]
    codes_result <- us_location_lookup(abbrs, "abbr", "hub")
    expect_equal(codes_result, expected_codes)

    expected_names <- forecasttools::us_location_table[
      match(abbrs, forecasttools::us_location_table$abbr),
      "name"
    ]
    names_result <- us_location_lookup(abbrs, "abbr", "long_name")
    expect_equal(names_result, expected_names)
  }
)


test_that("us_location_lookup preserves output length with repeats", {
  abbrs <- c(rep("MA", 5), "CA", "MT", "MA")
  result <- us_location_lookup(abbrs, "abbr")
  expect_equal(nrow(result), 8)
})

test_that("location_lookup is an alias for us_location_lookup", {
  expect_identical(us_location_lookup, location_lookup)
})

test_that(
  paste0(
    "to_location_table_column is an alias ",
    "for to_us_location_table_column"
  ),
  {
    expect_identical(to_us_location_table_column, to_location_table_column)
  }
)
