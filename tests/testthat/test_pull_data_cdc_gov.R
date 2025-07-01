start_date <- "2024-01-01"
end_date <- "2024-02-03"
jurisdictions <- c("CA", "TX")

mockdir_tests <- fs::path(mockdir, "dcg")


test_that("data_cdc_gov_endpoint() works as expected", {
  # compare to equivalent manual implementation
  purrr::walk(c("test-dataset-id1", "another-dataset-id3"), \(x) {
    expect_equal(
      data_cdc_gov_endpoint(x),
      glue::glue("https://data.cdc.gov/resource/{x}.json")
    )
  })
})

test_that("data_cdc_gov_dataset_lookup() errors appropriately", {
  test_vec_id <- c("ua7e-t2fy", "unavailable")
  test_vec_key <- c("nhsn_hrd_prelim", "unavailable")

  expect_error(
    data_cdc_gov_dataset_lookup(test_vec_id, "fake-column", strict = FALSE),
    "id"
  )
  expect_error(
    data_cdc_gov_dataset_lookup(test_vec_id, c("id", "id"), strict = FALSE),
    "length 1"
  )
  expect_error(
    data_cdc_gov_dataset_lookup(test_vec_id, "id", strict = "FALSE"),
    "logical"
  )
  expect_error(
    data_cdc_gov_dataset_lookup(
      test_vec_id,
      "id",
      strict = c(FALSE, TRUE)
    ),
    "length 1"
  )
  expect_error(
    data_cdc_gov_dataset_lookup(test_vec_id, "id", strict = TRUE),
    "matching 'id'"
  )
  expect_error(
    data_cdc_gov_dataset_lookup(test_vec_key, "key", strict = TRUE),
    "matching 'key'"
  )
  expect_error(
    data_cdc_gov_dataset_lookup(test_vec_id, "id", strict = FALSE),
    NA
  )
  expect_error(
    data_cdc_gov_dataset_lookup(test_vec_key, "key", strict = FALSE),
    NA
  )
})

test_that("data_cdc_gov_dataset_lookup() agrees with manual approach", {
  query_vector_key <- c(
    "nhsn_hrd_prelim",
    "nhsn_hrd_final",
    "missing_dataset",
    "nhsn_hrd_prelim"
  )
  query_vector_id <- c(
    "nhsn_hrd_prelim",
    "nhsn_hrd_final",
    "missing_dataset",
    "nhsn_hrd_prelim"
  )
  expect_equal(
    data_cdc_gov_dataset_lookup(query_vector_key, "key"),
    data_cdc_gov_dataset_table[
      match(x = query_vector_key, table = data_cdc_gov_dataset_table$key),
    ]
  )
  expect_equal(
    data_cdc_gov_dataset_lookup(query_vector_id, "id"),
    data_cdc_gov_dataset_table[
      match(x = query_vector_id, table = data_cdc_gov_dataset_table$id),
    ]
  )
})

test_that(
  paste0(
    "data_cdc_gov_dataset_id() is a simple wrapper of ",
    "data_cdc_gov_dataset_lookup()"
  ),
  {
    purrr::walk(
      list(
        "nhsn_hrd_prelim",
        "unavailable",
        c("nhsn_hrd_prelim", "unavailable")
      ),
      \(x) {
        expect_equal(
          data_cdc_gov_dataset_id(x),
          data_cdc_gov_dataset_lookup(x, "key")$id
        )
      }
    )
  }
)


test_that("data_cdc_gov_base_query() works as expected", {
  # compare to equivalent manual implementation
  purrr::walk(c("mock-dataset-id1", "3-another-dataset-id"), \(x) {
    expect_equal(
      data_cdc_gov_base_query(x),
      soql::soql(glue::glue("https://data.cdc.gov/resource/{x}.json"))
    )
  })
})


## replace env variables with fakes if and only if
## we are mocking api calls
if (fs::dir_exists(mockdir_tests)) {
  withr::local_envvar(
    .new = c(
      "DATA_CDC_GOV_API_KEY_ID" = "fake_key",
      "DATA_CDC_GOV_API_KEY_SECRET" = "fake_secret"
    )
  )
}

with_mock_dir(mockdir_tests, {
  test_that("Warnings raised if API key/secret not provided", {
    expect_warning(
      pull_data_cdc_gov_dataset(
        "nhsn_hrd_prelim",
        api_key_id = NULL,
        api_key_secret = "test",
        limit = 10,
        error_on_limit = FALSE
      ),
      "No valid API key"
    )
    expect_warning(
      pull_data_cdc_gov_dataset(
        "nhsn_hrd_prelim",
        api_key_id = "test",
        api_key_secret = NULL,
        limit = 10,
        error_on_limit = FALSE
      ),
      "No valid API key"
    )
    expect_warning(
      pull_data_cdc_gov_dataset(
        "nhsn_hrd_prelim",
        api_key_id = "test",
        api_key_secret = "",
        limit = 10,
        error_on_limit = FALSE
      ),
      "No valid API key"
    )
    expect_warning(
      pull_data_cdc_gov_dataset(
        "nhsn_hrd_prelim",
        api_key_id = "",
        api_key_secret = "test",
        limit = 10,
        error_on_limit = FALSE
      ),
      "No valid API key"
    )
  })

  test_that(
    paste0(
      "Filters by start_date, end_date, and ",
      "jurisdiction work as expected"
    ),
    {
      purrr::walk(forecasttools::data_cdc_gov_dataset_table$key, \(key) {
        info <- data_cdc_gov_dataset_lookup(key, "key", strict = TRUE)
        result <- pull_data_cdc_gov_dataset(
          key,
          start_date = start_date,
          end_date = end_date,
          locations = jurisdictions
        ) |>
          expect_warning(regexp = NA) # fail if warning

        if (nrow(result) > 0) {
          expect_true(all(
            as.Date(result[[info$date_column]]) >= as.Date(start_date)
          ))
          expect_true(all(
            as.Date(result[[info$date_column]]) <= as.Date(end_date)
          ))
          expect_setequal(result[[info$location_column]], jurisdictions)
        }
      })
    }
  )

  test_that("Selection of columns works as expected", {
    columns <- c("numinptbeds", "totalconfc19newadmped")

    result <- pull_data_cdc_gov_dataset(
      "nhsn_hrd_prelim",
      columns = columns,
      limit = 10,
      error_on_limit = FALSE
    ) |>
      expect_warning(regexp = NA) # fail if warning

    expected_columns <- c("jurisdiction", "weekendingdate", columns)
    checkmate::expect_names(
      colnames(result),
      identical.to = expected_columns
    )
  })

  test_that("ordering via order_by and desc works as expected", {
    columns <- c("numinptbeds", "totalconfc19newadmped")

    order_by <- c("weekendingdate", "jurisdiction")

    result_asc <- pull_data_cdc_gov_dataset(
      "nhsn_hrd_prelim",
      start_date = start_date,
      end_date = end_date,
      jurisdictions = jurisdictions,
      columns = columns,
      order_by = order_by,
      desc = FALSE
    ) |>
      expect_warning(regexp = NA) # fail if warning
    result_desc <- pull_data_cdc_gov_dataset(
      "nhsn_hrd_prelim",
      start_date = start_date,
      end_date = end_date,
      jurisdictions = jurisdictions,
      columns = columns,
      order_by = order_by,
      desc = TRUE
    ) |>
      expect_warning(regexp = NA) # fail if warning

    expect_equal(
      result_asc,
      result_desc |>
        dplyr::arrange(
          .data$weekendingdate,
          .data$jurisdiction
        )
    )
    expect_equal(
      result_desc,
      result_asc |>
        dplyr::arrange(
          dplyr::desc(.data$weekendingdate),
          dplyr::desc(.data$jurisdiction)
        )
    )
  })

  test_that("rename_columns works as expected", {
    limit <- 10
    info <- data_cdc_gov_dataset_lookup("nssp_prop_ed_visits", "key")

    result_no_rename <- pull_data_cdc_gov_dataset(
      info$key,
      limit = limit,
      error_on_limit = FALSE
    )
    result_rename <- pull_data_cdc_gov_dataset(
      info$key,
      limit = limit,
      error_on_limit = FALSE,
      rename_columns = TRUE
    )
    expect_equal(
      result_rename,
      result_no_rename |>
        dplyr::rename(
          location = !!info$location_column,
          date = !!info$date_column
        )
    )
  })

  test_that("limit and error_on_limit work as expected", {
    limit <- 10
    result <- pull_data_cdc_gov_dataset(
      "nssp_prop_ed_visits",
      limit = limit,
      error_on_limit = FALSE
    )
    expect_equal(nrow(result), limit)

    expect_error(
      pull_data_cdc_gov_dataset(
        "nssp_prop_ed_visits",
        limit = limit,
        error_on_limit = TRUE
      )
    )
  })
})
