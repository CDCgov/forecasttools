expect_nan_warning <- function(x) {
  expect_warning(x, "NaNs produced")
}

test_that("scaled_logit is equivalent to qlogis where a = 0, b = 1", {
  vals <- runif(1000)
  expect_equal(scaled_logit(vals, 0, 1), qlogis(vals, 0, 1))
})

test_that("scaled_logit warns and returns NaN outside interval and returns Inf or -Inf at boundaries, as appropriate", {
  vals <- c(-50.1, -100, -50, 0, 3.1, 3)
  {
    result <- scaled_logit(vals, -50, 3)
  } |>
    expect_nan_warning() |>
    expect_nan_warning() # should warn exactly twice, once for each bound

  expect_equal(result[3], -Inf)
  expect_equal(result[6], Inf)
  purrr::walk(
    c(
      is.nan(result[1]),
      is.nan(result[2]),
      is.finite(result[4]),
      is.nan(result[5])
    ),
    expect_true
  )
})

test_that("scaled_shifted_log reduces to log for m = 1, b = 0", {
  vals <- exp(rnorm(1000, sd = 10))
  expect_equal(scaled_shifted_log(vals, 1, 0), log(vals))
})

test_that("scaled_shifted_log is -log(-x) for m = -1, b = 0", {
  vals <- -exp(rnorm(1000, sd = 10))
  expect_equal(scaled_shifted_log(vals, -1, 0), -log(-vals))
})

test_that("scaled_shifted_log reduces to log1p for m = 1, b = -1", {
  vals <- c(0, exp(rnorm(1000, sd = 10)))
  expect_equal(scaled_shifted_log(vals, 1, -1), log1p(vals))
})


test_that("scaled_shifted_log warns and returns NaN outside interval and returns Inf or -Inf at boundaries, as appropriate", {
  ## example defined on (-50, +Inf)
  vals_pos <- c(-50.1, -50, -100, 0, 3.1, 3, 300, Inf)
  expect_nan_warning({
    result_pos <- scaled_shifted_log(vals_pos, 1, -50)
  })

  expect_equal(result_pos[2], -Inf)
  expect_equal(result_pos[8], Inf)
  purrr::walk(
    list(
      is.nan(result_pos[1]),
      is.nan(result_pos[3]),
      all(is.finite(result_pos[4:7]))
    ),
    expect_true
  )

  ## example defined on (-Inf, 10)
  vals_neg <- c(10, 0, 3.52, 1, 10.1, 100, -100, -Inf)
  expect_nan_warning({
    result_neg <- scaled_shifted_log(vals_neg, -1, 10)
  })

  expect_equal(result_neg[1], Inf)
  expect_equal(result_neg[8], -Inf)
  purrr::walk(
    list(
      all(is.finite(result_neg[2:4])),
      all(is.nan(result_neg[5:6])),
      is.finite(result_neg[7])
    ),
    expect_true
  )
})

test_that("get_transform_to_real_line rejects invalid arguments", {
  ## upper bound <= lower bound should raise error even if Inf
  purrr::walk(
    list(
      c(Inf, Inf),
      c(-Inf, -Inf),
      c(5.0, 5.0),
      c(-3.5, -3.5)
    ),
    \(x) {
      expect_error(
        get_transform_to_real_line(x[1], x[2]),
        "must be strictly greater"
      )
    }
  )

  ## vector args should be rejected, including mix of vector and scalar
  expect_error(
    get_transform_to_real_line(c(2, 5), c(3, 6)),
    "Must have length 1"
  )
  expect_error(
    get_transform_to_real_line(2, c(5, 10)),
    "Must have length 1"
  )
  expect_error(
    get_transform_to_real_line(c(1, 2), 7),
    "Must have length 1"
  )

  ## non-numeric args should be rejected
  purrr::walk(
    list(
      "test",
      NULL,
      tibble::tibble(x = 1:5)
    ),
    \(x) {
      expect_error(
        get_transform_to_real_line(5, x),
        "Must be of type 'number'"
      )
      expect_error(
        get_transform_to_real_line(x, 3.2),
        "Must be of type 'number'"
      )
    }
  )

  ## NaN and NA args should be rejected
  purrr::walk(
    list(
      c(NaN, 4),
      c(NA, 5),
      c(3, NaN),
      c(2.5, NA)
    ),
    \(x) {
      expect_error(
        get_transform_to_real_line(x[1], x[2]),
        "May not be NA"
      )
    }
  )
})


test_that("get_transform_to_real_line returns expected transforms", {
  expect_identical(get_transform_to_real_line(-Inf, Inf), identity)

  lefts <- rnorm(10, sd = 50)
  rights <- lefts + exp(rnorm(10))
  purrr::walk2(lefts, rights, \(lb, ub) {
    expect_identical(
      get_transform_to_real_line(lb, ub),
      purrr::partial(scaled_logit, a = lb, b = ub),
      ignore_function_env = TRUE
    )
    expect_equal(
      get_transform_to_real_line(-Inf, ub),
      purrr::partial(scaled_shifted_log, m = -1, b = ub),
      ignore_function_env = TRUE
    )
    expect_equal(
      get_transform_to_real_line(lb, Inf),
      purrr::partial(scaled_shifted_log, m = 1, b = lb),
      ignore_function_env = TRUE
    )
  })
})
