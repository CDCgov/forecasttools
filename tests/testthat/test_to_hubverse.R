testthat::test_that(
  paste0(
    "get_hubverse_table errors if reference date ",
    "is the wrong day of the week"
  ),
  {
    testthat::expect_error(
      forecasttools::get_hubverse_table(
        tibble::tibble(),
        "2025-01-01"
      ),
      "which is day number 4"
    )
    testthat::expect_error(
      forecasttools::get_hubverse_table(
        tibble::tibble(),
        "2025-01-01",
        week_start = 3
      ),
      "which is day number 1"
    )
    testthat::expect_error(
      forecasttools::get_hubverse_table(
        tibble::tibble(),
        "2025-01-01",
        reference_dow = 3
      ),
      "to be day number 3"
    )
  }
)
