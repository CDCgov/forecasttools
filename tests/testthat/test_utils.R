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

non_utc_tz <- "Asia/Tokyo"
timestamp_col_name <- "timestamp"

timestamp_wo_tz_tbl <- tibble::tibble(
  id = 1:5,
  !!timestamp_col_name := as.POSIXct(c(
    "2023-01-01 10:00:00",
    "2023-01-02 11:00:00",
    "2023-01-03 12:00:00",
    "2023-01-04 13:00:00",
    "2023-01-05 14:00:00"
  ))
)

timestamp_w_tz_tbl <- timestamp_wo_tz_tbl |>
  dplyr::mutate(
    !!timestamp_col_name := lubridate::with_tz(.data$timestamp, non_utc_tz)
  )

timestamp_wo_tz_tbl_arrow <- arrow::arrow_table(timestamp_wo_tz_tbl)
timestamp_w_tz_tbl_arrow <- arrow::arrow_table(timestamp_w_tz_tbl)

test_that("get_ts_tz_from_arrow_tbl works as expected", {
  expect_equal(
    get_ts_tz_from_arrow_tbl(timestamp_wo_tz_tbl_arrow),
    rlang::set_names("", timestamp_col_name)
  )
  expect_equal(
    get_ts_tz_from_arrow_tbl(timestamp_w_tz_tbl_arrow),
    rlang::set_names(non_utc_tz, timestamp_col_name)
  )
})

test_that("get_cols_wo_tz_from_arrow_tbl works as expected", {
  expect_equal(
    get_cols_wo_tz_from_arrow_tbl(timestamp_wo_tz_tbl_arrow),
    timestamp_col_name
  )
  expect_equal(
    get_cols_wo_tz_from_arrow_tbl(timestamp_w_tz_tbl_arrow),
    character(0)
  )
})


test_that("read_pq_with_tz_correction and read_tabular handle
timezones correctly in parquet files", {
  outpath_wo_tz <- withr::local_tempfile(fileext = ".parquet")
  arrow::write_parquet(timestamp_wo_tz_tbl, outpath_wo_tz)

  expect_equal(
    read_pq_with_tz_correction(outpath_wo_tz) |>
      dplyr::pull(timestamp_col_name) |>
      lubridate::tz() |>
      unique(),
    "UTC"
  )

  expect_equal(
    read_tabular(outpath_wo_tz) |>
      dplyr::pull(timestamp_col_name) |>
      lubridate::tz() |>
      unique(),
    "UTC"
  )

  outpath_w_tz <- withr::local_tempfile(fileext = ".parquet")
  arrow::write_parquet(timestamp_w_tz_tbl, outpath_w_tz)

  expect_equal(
    read_pq_with_tz_correction(outpath_w_tz) |>
      dplyr::pull(timestamp_col_name) |>
      lubridate::tz() |>
      unique(),
    non_utc_tz
  )

  expect_equal(
    read_tabular(outpath_w_tz) |>
      dplyr::pull(timestamp_col_name) |>
      lubridate::tz() |>
      unique(),
    non_utc_tz
  )
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

test_filter_lte_df <- tibble::tibble(
  number = c(-1, 1, 1, 3),
  letter = c("A", "N", "Y", "Y"),
  date = as.Date(c("2026-07-01", "2026-07-01", "2026-07-05", "2026-07-09"))
)

test_that("filter_largest_lte treats values above the bound equally to the bound", {
  expect_equal(
    test_filter_lte_df |> filter_largest_lte(date, as.Date("2027-01-01")),
    test_filter_lte_df |> filter_largest_lte(date, as.Date("2026-07-09"))
  )

  expect_equal(
    test_filter_lte_df |> filter_largest_lte(number, 5000),
    test_filter_lte_df |> filter_largest_lte(number, 3)
  )

  expect_equal(
    test_filter_lte_df |> filter_largest_lte(letter, "Z"),
    test_filter_lte_df |> filter_largest_lte(letter, "Y")
  )
})

test_that(
  paste0(
    "filter_largest_lte returns a length 0 tibble without a ",
    "warning when the lower bound is below all values"
  ),
  {
    expected_empty <- test_filter_lte_df |> dplyr::filter_out(TRUE)
    expect_no_warning(expect_equal(
      test_filter_lte_df |> filter_largest_lte(date, as.Date("1900-01-01")),
      expected_empty
    ))

    expect_no_warning(expect_equal(
      test_filter_lte_df |> filter_largest_lte(number, -2),
      expected_empty
    ))

    expect_no_warning(expect_equal(
      test_filter_lte_df |> filter_largest_lte(letter, ""),
      expected_empty
    ))
  }
)

test_that(
  paste0(
    "filter_largest_lte agrees with manual expectation on internal ",
    "values and preserves multiple row matches"
  ),
  {
    expect_equal(
      test_filter_lte_df |> filter_largest_lte(date, as.Date("2026-07-02")),
      test_filter_lte_df |> dplyr::filter(date == as.Date("2026-07-01"))
    )

    expect_equal(
      test_filter_lte_df |> filter_largest_lte(number, 2),
      test_filter_lte_df |> dplyr::filter(number == 1)
    )

    expect_equal(
      test_filter_lte_df |> filter_largest_lte(letter, "O"),
      test_filter_lte_df |> dplyr::filter(letter == "N")
    )
  }
)

test_that("filter_largest_lte works grouped", {
  # .by and piping in a grouped df are equivalent
  expect_equal(
    test_filter_lte_df |>
      dplyr::group_by(.data$letter) |>
      filter_largest_lte(date, as.Date("2026-07-02")) |>
      dplyr::ungroup(),
    test_filter_lte_df |>
      filter_largest_lte(date, as.Date("2026-07-02"), .by = "letter")
  )
  ## .preserve is respected, so if we set it to true even the group
  ## that gets filtered to size 0 ("Y") is retained.
  expect_equal(
    test_filter_lte_df |>
      dplyr::group_by(letter) |>
      filter_largest_lte(date, as.Date("2026-07-02")) |>
      dplyr::n_groups(),
    2
  )
  expect_equal(
    test_filter_lte_df |>
      dplyr::group_by(letter) |>
      filter_largest_lte(date, as.Date("2026-07-02"), .preserve = TRUE) |>
      dplyr::n_groups(),
    3
  )
})
