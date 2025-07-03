test_that(".process_soda_response errors on limit or not appropriately", {
  nrows <- 100
  my_resp <- httr2::response_json(
    status_code = 200,
    url = "https://example.com",
    method = "GET",
    headers = list(),
    body = data.frame(my_key = 1:nrows)
  )
  expect_error(
    .process_soda_response(my_resp, limit = nrows),
    "equal to the query limit"
  )
  expect_error(
    .process_soda_response(my_resp, limit = nrows, error_on_limit = FALSE),
    NA
  )
  expect_error(
    .process_soda_response(my_resp, limit = nrows + 1),
    NA
  )
})

test_that(".process_soda_response returns the expected result on success", {
  nrows <- 50
  body <- data.frame(
    my_key_one = 1:nrows,
    my_key_two = rep("a", nrows)
  )
  expected <- tibble::as_tibble(body)

  my_resp <- httr2::response_json(
    status_code = 200,
    url = "https://example.com",
    method = "GET",
    headers = list(),
    body = body
  )
  result <- .process_soda_response(my_resp, limit = nrows + 1)
  expect_equal(result, expected)
})
