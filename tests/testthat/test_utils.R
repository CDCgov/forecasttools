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

test_that("sym_limits has expected manual properties", {
  withr::with_seed(5, {
    rand_vals <- rnorm(100, sd = 100)
    center <- rnorm(1, sd = 100)
    pos_rand_vals <- exp(rnorm(100, sd = 100))
    pos_center <- exp(rnorm(1, sd = 100))
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
    center = pos_center,
    transform = "log10"
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
  expect_equal(mean(log(log_lims)), log(pos_center))
})

test_that("sym_limits agrees with manual expectation", {
  expect_equal(sym_limits(c(-5, 0.52, 8)), c(-8, 8))
  expect_equal(sym_limits(7, center = 2), c(-3, 7))
  expect_equal(sym_limits(c(-5, 0.52, 8), center = 5), c(-5, 15))
  expect_equal(
    sym_limits(c(100, 2, 0.00002), transform = "log", center = 1),
    c(0.00002, 1 / 0.00002)
  )
  expect_equal(
    sym_limits(c(100, 2, 0.00002), transform = "log", center = 2),
    c(0.00002, 2 * 2 / 0.00002)
  )
  expect_equal(sym_limits(c(1 / 5), transform = "log", center = 1), c(1 / 5, 5))

  expect_equal(
    sym_limits(c(0, 2, 0.00002), transform = "log", center = 2),
    c(0, Inf)
  )
  expect_equal(
    sym_limits(c(0, 2, 0.00002), transform = "log", center = 1),
    c(0, Inf)
  )

  expect_equal(sym_limits(c(2, 9), transform = "sqrt", center = 4), c(1, 9))
})


test_that("sym_limits functions argument checks work", {
  expect_error(sym_limits(c(1.3, "a")), "character")
  expect_error(sym_limits(c()), "NULL")
})
