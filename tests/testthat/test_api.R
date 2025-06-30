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

mockdir_tests <- fs::path(mockdir, "test_api")

with_mock_dir(mockdir_tests, {
  test_that(".perform_api_request() works as expected", {
    url <- "https://data.cdc.gov/resource/mpgq-jmmr.json?$limit=1"
    purrr::map(c(NULL, ""), \(x) {
      expect_warning(
        .perform_api_request(
          url,
          api_key_id = x,
          api_key_secret = "test"
        ),
        "No valid API key ID"
      )
      expect_warning(
        .perform_api_request(
          url,
          api_key_id = "test",
          api_key_secret = x
        ),
        "No valid API key ID"
      )
    })
  })
})
