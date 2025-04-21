temp_dir <- withr::local_tempdir()
withr::with_seed(5, {
  test_table <- tibble::tibble(
    x = 1:5,
    y = LETTERS[x],
    z = runif(5)
  )
})

test_that(
  paste0(
    "write_tabular_file and read_tabular_file are inverses ",
    "for all valid file extensions"
  ),
  {
    purrr::map(
      c("tsv", "csv", "parquet"),
      \(ext) {
        outpath <- fs::path(temp_dir, "test_table", ext = ext)
        write_tabular_file(test_table, outpath)
        result <- read_tabular_file(outpath) |>
          suppressMessages() |>
          tibble::as_tibble()

        expect_equal(result, test_table)
      }
    )
  }
)


test_that(
  paste0(
    "read_tabular_file and write_tabular_file error ",
    "when passed invalid extensions"
  ),
  {
    purrr::map(c("acsv", "tsv_", "parquetb"), \(ext) {
      outpath <- fs::path(temp_dir, "test_table", ext = ext)
      expect_error(
        write_tabular_file(test_table, outpath),
        "Names must be a subset of"
      )
      expect_error(
        read_tabular_file(outpath),
        "Names must be a subset of"
      )
    })
  }
)
