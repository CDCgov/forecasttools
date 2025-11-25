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

test_that(
  paste0(
    "location_lookup is an alias for us_location_lookup ",
    "unless location_output_format is a scalar, in which case ",
    "it is an alias for us_location_recode"
  ),
  {
    abbrs <- c(rep("MA", 5), "CA", "MT", "MA")
    result_vec <- location_lookup(abbrs, "abbr", "code")
    expect_type(result_vec, "character")
    expect_equal(
      result_vec,
      us_location_lookup(abbrs, "abbr", "code") |>
        dplyr::pull()
    )

    result_tbl <- location_lookup(
      abbrs,
      "abbr",
      c("code", "name", "long_name")
    )
    expect_s3_class(result_tbl, "tbl")

    expect_equal(
      result_tbl,
      us_location_lookup(
        abbrs,
        "abbr",
        c("code", "name", "long_name")
      )
    )
  }
)

test_that(
  paste0(
    "to_location_table_column is an alias ",
    "for to_us_location_table_column"
  ),
  {
    expect_identical(to_us_location_table_column, to_location_table_column)
  }
)


test_that(
  paste0(
    "us_location_recode returns correct codes ",
    "and NAs and preserves lengths"
  ),
  {
    input_vecs <- list(
      abbr = c("MA", "TX", "PR", "ZZ"), # "ZZ" is invalid
      code = c("25", "48", "XX", 72), # "XX" is invalid
      name = c("United States", "AL", "Alabama", "Wyoming") # "AL" is invalid
    )

    io <- tidyr::crossing(
      input = c("abbr", "code", "name"),
      output = c("abbr", "code", "name")
    )

    purrr::pwalk(io, \(input, output) {
      in_vec <- input_vecs[[input]]
      lookup <- forecasttools::us_location_table[[input]]
      expected <- forecasttools::us_location_table[
        match(in_vec, lookup),
      ][[output]]

      result <- us_location_recode(in_vec, input, output)
      expect_equal(length(result), length(in_vec))
      expect_true(anyNA(expected))
      expect_equal(result, expected)
    })
  }
)

test_that(
  paste0(
    "us_loc_abbr_to_code and us_loc_code_to_abbr ",
    "are special cases of us_location_recode"
  ),
  {
    abbrs <- c("MA", "TX", "PR", "ZZ") # "ZZ" is invalid
    codes <- c("25", "48", "XX", 72) # "XX" is invalid

    expect_equal(
      us_location_recode(abbrs, "abbr", "code"),
      us_loc_abbr_to_code(abbrs)
    )
    expect_equal(
      us_location_recode(codes, "code", "abbr"),
      us_loc_code_to_abbr(codes)
    )
  }
)
