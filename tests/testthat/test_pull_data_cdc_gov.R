start_date <- "2024-01-01"
end_date <- "2024-02-03"
jurisdictions <- c("CA", "TX")

mockdir_tests <- fs::path(mockdir, "dcg")


test_that("data_cdc_gov_dataset_id() works as expected", {
  manual <- function(x) {
    mask <- match(x = as.character(x), table = data_cdc_gov_dataset_table$key)
    return(data_cdc_gov_dataset_table[mask, ]$id)
  }
  ## compare to equivalent manual implementation for scalar
  ## values

  purrr::map(names(data_cdc_gov_dataset_table), \(x) {
    expect_equal(
      data_cdc_gov_dataset_id(x),
      manual(x)
    )
  })

  query_vector <- c(
    "nhsn_hrd_prelim",
    "nhsn_hrd_final",
    "missing_dataset",
    "nhsn_hrd_prelim"
  )
  expect_equal(
    data_cdc_gov_dataset_id(query_vector),
    manual(query_vector)
  )
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
