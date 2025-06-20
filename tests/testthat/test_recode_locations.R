test_that(
  paste0(
    "us_loc_abbr_to_code returns correct codes ",
    "and NAs and preserves length"
  ),
  {
    # Example values, adjust as per your actual us_location_table
    abbrs <- c("MA", "TX", "PR", "ZZ") # "ZZ" is invalid
    expected_codes <- forecasttools::us_location_table$location_code[
      match(abbrs, forecasttools::us_location_table$short_name)
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
    expected_abbrs <- forecasttools::us_location_table$short_name[
      match(codes, forecasttools::us_location_table$location_code)
    ]
    result <- us_loc_code_to_abbr(codes)
    expect_equal(length(result), length(codes))
    expect_equal(expected_abbrs[3], as.character(NA))
    expect_equal(result, expected_abbrs)
  }
)

test_that(
  paste0(
    "to_location_table_column returns correct column names ",
    "and errors on invalid input"
  ),
  {
    expect_equal(to_location_table_column("abbr"), "short_name")
    expect_equal(to_location_table_column("hub"), "location_code")
    expect_equal(to_location_table_column("long_name"), "long_name")
    expect_error(to_location_table_column("invalid_format"))
  }
)

test_that(
  paste0(
    "location_lookup returns correct rows and columns ",
    "for valid, invalid, and mixed input"
  ),
  {
    abbrs <- c("MA", "TX", "PR", "ZZ")
    # Get expected rows
    expected <- forecasttools::us_location_table[
      match(abbrs, forecasttools::us_location_table$short_name),
    ]

    result <- location_lookup(abbrs, "abbr")
    expect_equal(result, expected)

    # Output column only
    expected_codes <- forecasttools::us_location_table$location_code[
      match(abbrs, forecasttools::us_location_table$short_name)
    ]
    codes_result <- location_lookup(abbrs, "abbr", "hub")
    expect_equal(codes_result, expected_codes)
  }
)


test_that("location_lookup preserves output length with repeats", {
  abbrs <- c(rep("MA", 5), "CA", "MT", "MA")
  result <- location_lookup(abbrs, "abbr")
  expect_equal(nrow(result), 8)
})
