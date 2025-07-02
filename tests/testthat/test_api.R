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

mockdir_tests <- fs::path(mockdir, "api")

if (fs::dir_exists(mockdir_tests)) {
  api_key <- "fake_key"
  api_secret <- "fake_secret"
} else {
  api_key <- Sys.getenv("DATA_CDC_GOV_API_KEY_ID")
  api_secret <- Sys.getenv("DATA_CDC_GOV_API_KEY_SECRET")
}

with_mock_dir(mockdir_tests, {
  url <- "https://data.cdc.gov/resource/mpgq-jmmr.json?$limit=1"
  test_that(".do_api_request() warns if API key missing", {
    purrr::walk(c(NULL, ""), \(x) {
      expect_warning(
        .do_api_request(
          url,
          api_key_id = x,
          api_key_secret = "test"
        ),
        "No valid API key ID"
      )
      result_warn <- expect_warning(
        .do_api_request(
          url,
          api_key_id = "test",
          api_key_secret = x
        ),
        "No valid API key ID"
      )
    })
  })

  test_that(".do_api_request() works as expected", {
    result <- expect_warning(
      .do_api_request(
        url,
        api_key_id = api_key,
        api_key_secret = api_secret
      ),
      regexp = NA
    )

    expect_s3_class(result, "httr2_response")
    expect_equal(result$status_code, 200L)
    expect_equal(result$method, "GET")
  })
})
