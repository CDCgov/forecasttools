start_date <- "2023-01-01"
end_date <- "2023-10-31"
jurisdictions <- c("CA", "TX")

mockdir <- "api_mocks"

with_mock_dir(mockdir, {
  test_that("Warnings raised if API key/secret not provided", {
    expect_warning(pull_nhsn(
      api_key_id = NULL, api_key_secret = NULL,
      limit = 10,
      error_on_limit = FALSE
    ))
  })

  test_that(paste0(
    "Filters by start_date, end_date, and ",
    "jurisdiction work as expected"
  ), {
    result <- pull_nhsn(
      start_date = start_date, end_date = end_date,
      jurisdictions = jurisdictions
    )

    expect_true(all(result$weekendingdate >= as.Date(start_date)))
    expect_true(all(result$weekendingdate <= as.Date(end_date)))
    expect_setequal(result$jurisdiction, jurisdictions)
  })

  test_that("Selection of columns works as expected", {
    columns <- c("numinptbeds", "totalconfc19newadmped")

    result <- pull_nhsn(
      columns = columns,
      limit = 10,
      error_on_limit = FALSE
    )

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
    )
    result_desc <- pull_nhsn(
      start_date = start_date,
      end_date = end_date,
      jurisdictions = jurisdictions,
      columns = columns,
      order_by = order_by,
      desc = TRUE
    )


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
    result <- pull_nhsn(limit = limit, error_on_limit = FALSE)
    expect_equal(nrow(result), limit)

    expect_error(pull_nhsn(limit = limit, error_on_limit = TRUE))
  })
})
