temp_dir <- withr::local_tempdir()
withr::with_seed(5, {
  test_table_simple_types <- tibble::tibble(
    x = 1:5,
    y = LETTERS[x],
    z = runif(5)
  )
  test_table <- test_table_simple_types |>
    dplyr::mutate(y = fs::path(.data$y))
})

test_that("write_tabular simplfies column types correctly", {
  purrr::map(
    c("tsv", "csv", "parquet"),
    \(ext) {
      outpath <- withr::local_tempfile(fileext = ".parquet")
      write_tabular(test_table, outpath, simplify_column_types = TRUE)
      result <- read_tabular(outpath) |>
        suppressMessages() |>
        tibble::as_tibble()

      expect_equal(result, test_table_simple_types)
    }
  )
})

test_that("write_tabular allows complex column types for parquet files", {
  outpath <- withr::local_tempfile(fileext = ".parquet")
  write_tabular(test_table, outpath, simplify_column_types = FALSE)
  result <- read_tabular(outpath)

  expect_equal(result, test_table)
})

test_that(
  paste0(
    "read_tabular and write_tabular error ",
    "when passed invalid extensions"
  ),
  {
    purrr::map(c("acsv", "tsv_", "parquetb"), \(ext) {
      outpath <- fs::path(temp_dir, "test_table", ext = ext)
      expect_error(
        write_tabular(test_table, outpath),
        "Names must be a subset of"
      )
      expect_error(
        read_tabular(outpath),
        "Names must be a subset of"
      )
    })
  }
)

test_that("read_tabular handles timezones correctly in parquet files", {
  non_utc_tz <- "Asia/Tokyo"
  timestap_df_no_tz <- tibble::tibble(
    id = 1:5,
    timestamp = as.POSIXct(c(
      "2023-01-01 10:00:00",
      "2023-01-02 11:00:00",
      "2023-01-03 12:00:00",
      "2023-01-04 13:00:00",
      "2023-01-05 14:00:00"
    ))
  )

  timestap_df_with_tz <- timestap_df_no_tz |>
    dplyr::mutate(timestamp = lubridate::with_tz(timestamp, non_utc_tz))

  outpath1 <- withr::local_tempfile(fileext = ".parquet")
  arrow::write_parquet(timestap_df_no_tz, outpath1)
  result <- read_tabular(outpath1)
  expect_equal(result$timestamp |> lubridate::tz() |> unique(), "UTC")

  outpath2 <- withr::local_tempfile(fileext = ".parquet")
  arrow::write_parquet(timestap_df_with_tz, outpath2)
  result <- read_tabular(outpath2)
  expect_equal(result$timestamp |> lubridate::tz() |> unique(), non_utc_tz)
})

test_that("sym_limits has expected manual properties", {
  withr::with_seed(5, {
    rand_vals <- rnorm(100, sd = 100)
    center <- rnorm(1, sd = 100)
    pos_rand_vals <- exp(rnorm(100, sd = 1))
    pos_center <- exp(rnorm(1, sd = 1))
  })

  lims <- sym_limits(rand_vals, center = center)
  checkmate::expect_numeric(
    lims,
    len = 2,
    sorted = TRUE
  )
  checkmate::expect_numeric(
    rand_vals,
    lower = lims[1],
    upper = lims[2]
  )
  expect_equal(mean(lims), center)

  log_lims <- sym_limits(
    pos_rand_vals,
    transform = "log10",
    center = pos_center
  )
  checkmate::expect_numeric(
    log_lims,
    len = 2,
    sorted = TRUE
  )
  checkmate::expect_numeric(
    pos_rand_vals,
    lower = log_lims[1],
    upper = log_lims[2]
  )
  expect_equal(mean(log10(log_lims)), log10(pos_center))
})

test_that("sym_limits agrees with manual expectation", {
  expect_equal(sym_limits(c(-5, 0.52, 8)), c(-8, 8))
  expect_equal(sym_limits(7, center = 2), c(-3, 7))
  expect_equal(sym_limits(c(-5, 0.52, 8), center = 5), c(-5, 15))
  expect_equal(
    sym_limits(c(100, 2, 0.00002), transform = "log"),
    c(0.00002, 1 / 0.00002)
  )
  expect_equal(
    sym_limits(c(100, 2, 0.00002), transform = "log", center = 2),
    c(0.00002, 2 * 2 / 0.00002)
  )
  expect_equal(sym_limits(c(1 / 5), transform = "log"), c(1 / 5, 5))

  expect_equal(
    sym_limits(c(0, 2, 0.00002), transform = "log", center = 2),
    c(0, Inf)
  )
  expect_equal(
    sym_limits(c(0, 2, 0.00002), transform = "log"),
    c(0, Inf)
  )

  expect_equal(sym_limits(c(2, 9), transform = "sqrt", center = 4), c(1, 9))
})


test_that("sym_limits functions argument checks work", {
  expect_error(sym_limits(c(1.3, "a")), "character")
  expect_error(sym_limits(c()), "NULL")
})
