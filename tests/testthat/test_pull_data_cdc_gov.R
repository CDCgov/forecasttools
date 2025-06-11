start_date <- "2023-01-01"
end_date <- "2023-10-31"
jurisdictions <- c("CA", "TX")

mockdir <- "api_mocks"

test_that(".warn_no_api_creds() works as expected", {
  expect_warning(
    .warn_no_api_creds(),
    "No valid API key ID"
  )
  expect_warning(
    .warn_no_api_creds("https://example.com"),
    "by visiting https://example.com"
  )
})


test_that("data_cdc_gov_dataset_id() works as expected", {
  # compare to equivalent manual implementation
  purrr::map(names(data_cdc_gov_datasets), \(x) {
    expect_equal(
      data_cdc_gov_dataset_id(x),
      data_cdc_gov_datasets[[x]]$id
    )
  })

  # should error with vector input
  expect_error(
    data_cdc_gov_dataset_id(c("nhsn_hist_daily", "nhsn_hrd_prelim")),
    "length 1"
  )

  # should error with non-supported name
  expect_error(data_cdc_gov_dataset_id("test_name"), "subset of")
})


test_that("data_cdc_gov_endpoint() works as expected", {
  # compare to equivalent manual implementation
  purrr::map(c("test-dataset-id1", "another-dataset-id3"), \(x) {
    expect_equal(
      data_cdc_gov_endpoint(x),
      glue::glue("https://data.cdc.gov/resource/{x}.json")
    )
  })
})

test_that("data_cdc_gov_base_query() works as expected", {
  # compare to equivalent manual implementation
  purrr::map(c("mock-dataset-id1", "3-another-dataset-id"), \(x) {
    expect_equal(
      data_cdc_gov_base_query(x),
      soql::soql(glue::glue("https://data.cdc.gov/resource/{x}.json"))
    )
  })
})


with_mock_dir(mockdir, {
  test_that("Warnings raised if API key/secret not provided", {
    expect_warning(
      pull_nhsn(
        api_key_id = NULL,
        api_key_secret = NULL,
        limit = 10,
        error_on_limit = FALSE
      ) |>
        suppressMessages()
    )
  })

  test_that(
    paste0(
      "Filters by start_date, end_date, and ",
      "jurisdiction work as expected"
    ),
    {
      result <- pull_nhsn(
        start_date = start_date,
        end_date = end_date,
        jurisdictions = jurisdictions
      ) |>
        suppressMessages()

      expect_true(all(result$weekendingdate >= as.Date(start_date)))
      expect_true(all(result$weekendingdate <= as.Date(end_date)))
      expect_setequal(result$jurisdiction, jurisdictions)
    }
  )

  test_that("Selection of columns works as expected", {
    columns <- c("numinptbeds", "totalconfc19newadmped")

    result <- pull_nhsn(
      columns = columns,
      limit = 10,
      error_on_limit = FALSE
    ) |>
      suppressMessages()

    expected_columns <- c("jurisdiction", "weekendingdate", columns)
    checkmate::expect_names(
      colnames(result),
      identical.to = expected_columns
    )
  })

  test_that("ordering via order_by and desc works as expected", {
    columns <- c("numinptbeds", "totalconfc19newadmped")

    order_by <- c("weekendingdate", "jurisdiction")

    result_asc <- pull_nhsn(
      start_date = start_date,
      end_date = end_date,
      jurisdictions = jurisdictions,
      columns = columns,
      order_by = order_by,
      desc = FALSE
    ) |>
      suppressMessages()
    result_desc <- pull_nhsn(
      start_date = start_date,
      end_date = end_date,
      jurisdictions = jurisdictions,
      columns = columns,
      order_by = order_by,
      desc = TRUE
    ) |>
      suppressMessages()

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

  test_that("limit and error_on_limit work as expected", {
    limit <- 10
    result <- pull_nhsn(limit = limit, error_on_limit = FALSE) |>
      suppressMessages()
    expect_equal(nrow(result), limit)

    expect_error(
      pull_nhsn(limit = limit, error_on_limit = TRUE) |>
        suppressMessages()
    )
  })
})
