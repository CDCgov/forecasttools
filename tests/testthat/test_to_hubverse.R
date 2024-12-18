test_that(
  paste0(
    "get_hubverse_table errors if reference date ",
    "is the wrong day of the week"
  ),
  {
    expect_error(
      forecasttools::get_hubverse_table(
        tibble::tibble(),
        "2025-01-01"
      ),
      "4"
    )
    expect_error(
      forecasttools::get_hubverse_table(
        tibble::tibble(),
        "2025-01-01",
        week_start = 3
      ),
      "1"
    )
    expect_error(
      forecasttools::get_hubverse_table(
        tibble::tibble(),
        "2025-01-01",
        reference_dow = 3
      ),
      "3"
    )
  }
)
