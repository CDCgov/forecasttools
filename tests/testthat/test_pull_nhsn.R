start_date <- as.Date("2024-01-01")
end_date <- as.Date("2024-02-03")
jurisdictions <- c("CA", "TX")

mockdir_tests <- fs::path(mockdir, "nhsn")

## replace env variables with fakes if and only if
## we are mocking api calls
if (fs::dir_exists(mockdir_tests)) {
  withr::local_envvar(
    .new = c(
      "NHSN_API_KEY_ID" = "fake_key",
      "NHSN_API_KEY_SECRET" = "fake_secret"
    )
  )
}

with_mock_dir(mockdir_tests, {
  test_that("pull_nhsn is deprecated", {
    rlang::local_options(lifecycle_verbosity = "error")

    expect_error(
      pull_nhsn(
        limit = 1,
        error_on_limit = FALSE
      ) |>
        suppressMessages(),
      "deprecated in forecasttools 0.1.7"
    )
  })

  test_that("Warnings raised if API key/secret not provided", {
    rlang::local_options(lifecycle_verbosity = "quiet")

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
      rlang::local_options(lifecycle_verbosity = "quiet")

      result <- pull_nhsn(
        start_date = start_date,
        end_date = end_date,
        jurisdictions = jurisdictions
      ) |>
        expect_warning(regexp = NA) # fail if warning

      expect_true(all(result$weekendingdate >= start_date))
      expect_true(all(result$weekendingdate <= end_date))
      expect_setequal(result$jurisdiction, jurisdictions)
    }
  )

  test_that("Selection of columns works as expected", {
    rlang::local_options(lifecycle_verbosity = "quiet")

    columns <- c("numinptbeds", "totalconfc19newadmped")

    result <- pull_nhsn(
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
    rlang::local_options(lifecycle_verbosity = "quiet")

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
      expect_warning(regexp = NA) # fail if warning
    result_desc <- pull_nhsn(
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

  test_that("limit and error_on_limit work as expected", {
    rlang::local_options(lifecycle_verbosity = "quiet")

    limit <- 10
    result <- pull_nhsn(limit = limit, error_on_limit = FALSE)
    expect_equal(nrow(result), limit)

    expect_error(
      pull_nhsn(limit = limit, error_on_limit = TRUE)
    )
  })
})
